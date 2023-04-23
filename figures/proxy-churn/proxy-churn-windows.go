// This program reads a metrics-ip-salted.jsonl and estimates how quickly the
// composition of a proxy pool changes over time.
//
// The program works by comparing pairs of "windows", which are consecutive
// groups of input records. The default duration of a window is 24h,
// controllable with the -duration command-line option. For each "reference"
// window, it looks at nearby "sample" windows that are within a certain
// distance of the end of the reference (by default up to 10h before and 40h
// after, controllable with the -before and -after command-line options). Then,
// for each reference–sample pair, it outputs the size of the reference set, the
// size of the sample set, and the size of their union (from which can be
// derived the size of their intersection, which is what we are really
// interested in). The output format is CSV, with the following columns:
//
//	reference_timestamp_end,reference_duration,sample_timestamp_end_offset,sample_duration,reference_count,sample_count,union_count
//
// One way to think about what this program does is that ideally, what we want
// is a big matrix that compares every possible window with every other window.
// But to save space, rather than compare with every other window, we only
// compare with other nearby windows. The output size is O(n) rather than O(n²).
//
// References
//
// "Support Distinct IP Counting" is the merge request that added the log output
// that is processed by this program.
// https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/merge_requests/95
package main

import (
	"bufio"
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"runtime"
	"sort"
	"strconv"
	"sync"
	"time"

	"github.com/clarkduvall/hyperloglog"
)

const (
	timestampFormat = "2006-01-02 15:04:05"

	// hllPrecision is a required argument when creating a new
	// HyperLogLogPlus. The value here needs to match the one in the
	// Snowflake broker:
	// https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/blob/c097d5f3bc9e95403006527b90207dfb11ce6438/common/ipsetsink/sink.go#L14
	hllPrecision = 18
)

// Type record represents a single record from the input file, or multiple
// records merged into one.
type record struct {
	Start time.Time
	End   time.Time
	HLL   *hyperloglog.HyperLogLogPlus
}

// Function readRecords reads a metrics-ip-salted.jsonl log and returns a slice of
// records.
func readRecords(f io.Reader) ([]record, error) {
	var records []record
	scanner := bufio.NewScanner(f)
	scanner.Buffer(nil, 1024*1024)
	for scanner.Scan() {
		// Reference for JSON format:
		// https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/blob/c097d5f3bc9e95403006527b90207dfb11ce6438/common/ipsetsink/sinkcluster/common.go
		var rec struct {
			RecordingStart time.Time `json:"recordingStart"`
			RecordingEnd   time.Time `json:"recordingEnd"`
			Recorded       []byte    `json:"recorded"`
		}
		if err := json.Unmarshal(scanner.Bytes(), &rec); err != nil {
			return nil, fmt.Errorf("json.Unmarshal: %w", err)
		}
		// The precision argument is required here, but arbitrary: it
		// will be overwritten when we hll.GobDecode.
		hll, err := hyperloglog.NewPlus(hllPrecision)
		if err != nil {
			return nil, fmt.Errorf("hyperloglog.NewPlus: %w", err)
		}
		err = hll.GobDecode(rec.Recorded)
		if err != nil {
			return nil, fmt.Errorf("GobDecode: %w", err)
		}
		records = append(records, record{
			Start: rec.RecordingStart,
			End:   rec.RecordingEnd,
			HLL:   hll,
		})
	}
	err := scanner.Err()
	if err != nil {
		return nil, err
	}
	return records, nil
}

// Function newHLL creates a new, empty HyperLogLogPlus structure.
func newHLL() *hyperloglog.HyperLogLogPlus {
	hll, err := hyperloglog.NewPlus(hllPrecision)
	// The only way hyperloglog.NewPlus can fail is if the precision is out
	// of bounds, so panic rather than return an error.
	if err != nil {
		panic(err)
	}
	return hll
}

// Function cloneHLL creates a new, independent copy of hll.
func cloneHLL(hll *hyperloglog.HyperLogLogPlus) *hyperloglog.HyperLogLogPlus {
	clone := newHLL()
	err := clone.Merge(hll)
	// The only way HyperLogLogPlus.Merge can fail is if the precisions are
	// unequal.
	if err != nil {
		panic(err)
	}
	return clone
}

// Function mergeHLL returns a new HyperLogLogPlus that is the result of merging
// x and y, without modifying the inputs.
func mergeHLL(x, y *hyperloglog.HyperLogLogPlus) *hyperloglog.HyperLogLogPlus {
	hll := cloneHLL(x)
	err := hll.Merge(y)
	if err != nil {
		panic(err)
	}
	return hll
}

// Function windowEnding returns the left and right endpoint indices
// of a window of records such that the left record's Start is no more than
// duration from record i's End. The window will not include record i itself if
// record i's Start is too far back.
func windowEnding(records []record, i int, duration time.Duration) (l, r int) {
	for l = i; l >= 0 && records[i].End.Sub(records[l].Start) <= duration; l-- {
	}
	l++
	r = i + 1
	return l, r
}

// Function windowSurrounding returns the left and right endpoint indices of a
// window of records such that the earliest Start is within beforeDuration of
// record i's End on the left, the lastest End is within afterDuration of record
// i's End on the right. The window will not include record i itself if record
// i's Start is too far back.
func windowSurrounding(records []record, i int, beforeDuration, afterDuration time.Duration) (l, r int) {
	for l = i; l >= 0 && records[i].End.Sub(records[l].Start) <= beforeDuration; l-- {
	}
	l++
	for r = i + 1; r < len(records) && records[r].End.Sub(records[i].End) <= afterDuration; r++ {
	}
	return l, r
}

// Function mergeRecords returns a new record whose Start is the minimum of all
// the inputs' Starts, whose End is the maximum of all the inputs' Ends, and
// whose HLL is the union of all the inputs' HLLs.
func mergeRecords(records []record) record {
	merged := record{
		HLL: newHLL(),
	}
	for _, rec := range records {
		if merged.Start.IsZero() || rec.Start.Before(merged.Start) {
			merged.Start = rec.Start
		}
		if merged.End.IsZero() || rec.End.After(merged.End) {
			merged.End = rec.End
		}
		err := merged.HLL.Merge(rec.HLL)
		if err != nil {
			panic(err)
		}
	}
	return merged
}

// Function process reads a metrics-ip-salted.jsonl log from in and writes a CSV
// file to out.
func process(
	in io.Reader, out io.Writer,
	concurrency int,
	windowDuration time.Duration,
	beforeDuration time.Duration,
	afterDuration time.Duration,
) error {
	// Read all input records into a slice.
	records, err := readRecords(in)
	if err != nil {
		return err
	}

	// Sort by start timestamp (and secondarily end timestamp, though we do
	// not expect any collisions in the start timestamp).
	sort.SliceStable(records, func(i, j int) bool {
		return records[i].Start.Before(records[j].Start) ||
			records[i].Start.Equal(records[j].Start) && records[i].End.Before(records[j].End)
	})

	// Start writing the output file.
	csvWriter := csv.NewWriter(out)
	err = csvWriter.Write([]string{
		"reference_timestamp_end",
		"reference_duration",
		"sample_timestamp_end_offset",
		"sample_duration",
		"reference_count",
		"sample_count",
		"union_count",
	})
	if err != nil {
		return err
	}

	// The many HyperLogLogPlus.Merge operations required for processing one
	// pair of windows are computationally expensive. Therefore we process
	// multiple reference windows in parallel. The slice of records is
	// read-only at this point, so no synchronization of the input is
	// needed.

	// We end up wanting the same merged windows over and over. Therefore we
	// memoize mergeRecords for pairs of left and right indexes.
	var memoLock sync.Mutex
	type memoKey struct {
		l, r int
	}
	memo := make(map[memoKey]record)
	memoMergeRecords := func(l, r int) record {
		key := memoKey{l, r}
		memoLock.Lock()
		defer memoLock.Unlock()
		rec, ok := memo[key]
		if ok {
			// Found in cache.
			return rec
		}
		// Not found, compute and add to cache.
		rec = mergeRecords(records[l:r])
		memo[key] = rec
		if len(memo) > 1024 {
			// When the cache gets too big, evict an old entry.
			minKey := key
			for k := range memo {
				if k.r < minKey.r {
					minKey = k
				}
			}
			delete(memo, minKey)
		}
		return rec
	}

	// The processOne function does the processing for one reference window,
	// the one that ends at index i, and all its nearby sample windows. It
	// returns a list of formatting rows ready to be written to CSV.
	processOne := func(i int) [][]string {
		// Compute the reference window that ends at i.
		wl, wr := windowEnding(records, i, windowDuration)
		reference := memoMergeRecords(wl, wr)
		// Compute the range of sample window endpoints.
		sl, sr := windowSurrounding(records, i, beforeDuration, afterDuration)
		rows := make([][]string, 0, sr-sl)
		for j := sl; j < sr; j++ {
			// Compute the sample window that ends at j.
			wl, wr := windowEnding(records, j, windowDuration)
			sample := memoMergeRecords(wl, wr)
			// Append the CSV row for this reference—sample pair.
			rows = append(rows, []string{
				reference.End.Format(timestampFormat),                               // reference_timestamp_end
				fmt.Sprintf("%.0f", reference.End.Sub(reference.Start).Seconds()),   // reference_duration
				fmt.Sprintf("%+.0f", sample.End.Sub(reference.End).Seconds()),       // sample_timestamp_end_offset
				fmt.Sprintf("%.0f", sample.End.Sub(sample.Start).Seconds()),         // sample_duration
				strconv.FormatUint(reference.HLL.Count(), 10),                       // reference_count
				strconv.FormatUint(sample.HLL.Count(), 10),                          // sample_count
				strconv.FormatUint(mergeHLL(reference.HLL, sample.HLL).Count(), 10), // union_count
			})
		}
		return rows
	}

	// Fire up multiple goroutines, each of which repeatedly reads a
	// reference window index from inChan, calls processOne, and writes the
	// output to outChan.
	type outRows struct {
		i    int // original index of this reference window, to keep the output in order
		rows [][]string
	}
	inChan := make(chan int, 16)
	outChan := make(chan outRows, 16)
	var wg sync.WaitGroup
	for i := 0; i < concurrency; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := range inChan {
				outChan <- outRows{
					i:    i,
					rows: processOne(i),
				}
			}
		}()
	}
	// Feed every reference window index to inChan.
	go func() {
		for i := range records {
			inChan <- i
		}
		close(inChan)
		wg.Wait()
		close(outChan)
	}()

	// The input reference windows may be processed out of order, but we
	// want to keep them sorted in the output. When a slice of output rows
	// arrives from outChan, check its index. If it's the next index we
	// expect to output, output it. If not, put it in the pending queue
	// until its turn comes up.
	next := 0
	pending := make(map[int][][]string)
	for out := range outChan {
		pending[out.i] = out.rows
		for {
			rows, ok := pending[next]
			if !ok {
				break
			}
			delete(pending, next)
			// Got what we want, output it.
			next++
			err := csvWriter.WriteAll(rows)
			if err != nil {
				return err
			}
		}
	}

	csvWriter.Flush()
	return csvWriter.Error()
}

func main() {
	flag.Usage = func() {
		fmt.Fprintf(flag.CommandLine.Output(), `Usage:
  %[1]s < metrics-ip-salted.jsonl > proxy-churn-windows.csv

Computes the overlap in unique proxy IP addresses between pairs of
windows in a metrics-ip-salted.jsonl file.

`, os.Args[0])
		flag.PrintDefaults()
	}

	const tolerance = 5 * time.Minute
	windowDuration := flag.Duration("duration", 24*time.Hour+tolerance, "window duration")
	beforeDuration := flag.Duration("before", 10*time.Hour+tolerance, "maximum sample time distance before reference")
	afterDuration := flag.Duration("after", 40*time.Hour+tolerance, "maximum sample time distance after reference")
	concurrency := flag.Int("concurrency", runtime.NumCPU(), "number of concurrent processing threads")

	flag.Parse()

	if *concurrency < 1 {
		fmt.Fprintf(os.Stderr, "error: -concurrency must be at least 1\n")
		os.Exit(1)
	}

	err := process(
		os.Stdin, os.Stdout,
		*concurrency,
		*windowDuration, *beforeDuration, *afterDuration,
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}
