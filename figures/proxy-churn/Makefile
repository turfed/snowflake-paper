all: \
	proxy-count-decay.pdf
.PHONY: all

*.pdf *.tmp.pdf *.svg *.tmp.png: .EXTRA_PREREQS = proxy-count-decay.r ../common.r
proxy-count-decay.tmp.pdf proxy-count-decay.svg proxy-count-decay.png: proxy-churn-windows.csv
	Rscript proxy-count-decay.r "$<" "$@"

proxy-churn-windows.csv: .EXTRA_PREREQS = proxy-churn-windows.go
proxy-churn-windows.csv: metrics-ip-salted.jsonl
	go run proxy-churn-windows.go -duration 24h5m -before 5m -after 40h5m < "$<" > "$@"

# metrics-ip-salted.jsonl is distributed separately.
metrics-ip-salted.jsonl: ;
.SECONDARY: metrics-ip-salted.jsonl

.INTERMEDIATE: proxy-count-decay.tmp.pdf
%.pdf: %.tmp.pdf
# Embed fonts.
	gs -q -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="$@" -dCompatibilityLevel=1.5 -dEmbedAllFonts=true -c '<</NeverEmbed []>> setdistillerparams' -f "$<"
%.png: %.tmp.png
	zopflipng -y -m "$<" "$@"

.DELETE_ON_ERROR:
