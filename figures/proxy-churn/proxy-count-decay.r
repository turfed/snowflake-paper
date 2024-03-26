# Makes a graph showing the number of unique Snowflake proxy IP addresses over
# sliding windows, also showing decay in the size of the intersection with
# windows in the near future.
#
# Usage:
#   Rscript proxy-count-delay.r [--hyphen-hack] proxy-churn-windows.csv proxy-count-delay.pdf

library("argparse")
library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd_hms(c(
	"2023-01-01 00:00:00",
	"2023-01-31 23:59:59"
))

DECAY_COLOR <- "slateblue"

(function() {
	parser <- ArgumentParser()
	parser$add_argument("--hyphen-hack", action="store_true", default=FALSE, help="convert hyphen to soft hyphen")
	parser$add_argument("proxy_churn_windows_csv_path", nargs=1)
	parser$add_argument("output_path", nargs=1)
	args <- parser$parse_args()
	hyphen_hack <<- args$hyphen_hack
	proxy_churn_windows_csv_path <<- args$proxy_churn_windows_csv_path
	output_path <<- args$output_path
})()

proxy_churn_windows <- read_csv(proxy_churn_windows_csv_path) %>%
	arrange(reference_timestamp_end, sample_timestamp_end_offset) %>%
	# Try to detect discontinuities in the time sequence, where a
	# reference_timestamp_end has not the expected 1-hour from the previous
	# value.
	mutate(restart = cumsum(is.na(lag(reference_timestamp_end)) | !(lag(reference_timestamp_end) == reference_timestamp_end | abs(lag(reference_timestamp_end) + 3600 - reference_timestamp_end) < 5*60)))

p <- ggplot() +
	geom_line(
		data = proxy_churn_windows %>%
			# It's too dense if we show the decay from every single
			# hourly sample. Just show one decay per 24 hours (the
			# one at 00 hours).
			filter(lubridate::hour(reference_timestamp_end) == 0) %>%
			# We have "decay" in windows before the reference too,
			# but only show the ones after.
			filter(sample_timestamp_end_offset >= 0),
		aes(
			group = reference_timestamp_end,
			x = reference_timestamp_end + sample_timestamp_end_offset,
			y = reference_count + sample_count - union_count,
			# Fade out the decay lines a little bit as they get
			# farther from the reference, but not to 0.0, so it's
			# visually clear that the samples stop at an arbitrary
			# limit and it not a natural consequence of the data.
			alpha = 1.0 - 0.2 * abs(sample_timestamp_end_offset / max(sample_timestamp_end_offset))
		),
		color = DECAY_COLOR
	) +
	geom_line(
		data = proxy_churn_windows %>%
			group_by(reference_timestamp_end) %>%
			summarize(reference_count = first(reference_count), restart = first(restart), .groups = "drop"),
		aes(x = reference_timestamp_end, y = reference_count, group = restart)
	) +
	annotate("text",
		x = lubridate::ymd_hms("2023-01-02 06:00:00"),
		y = 141000,
		# \u00ad (soft hyphen) avoids the hyphen turning into a minus sign: https://stackoverflow.com/a/48510383
		label = gsub("-", if (hyphen_hack) "\u00ad" else "-", "Unique proxy IP addresses per 24-hour window"),
		size = 3, lineheight = 0.7, hjust = 0, vjust = 0) +
	annotate("text",
		x = lubridate::ymd_hms("2023-01-05 12:00:00"),
		y = 23000,
		label = "Shared IP addresses 1, 2, …, 40 hours later",
		color = DECAY_COLOR,
		size = 3, lineheight = 0.7, hjust = 0, vjust = 1) +
	scale_y_continuous(
		limits = c(0, 157000),
		breaks = 50000*(0:3),
		minor_breaks = NULL,
		labels = scales::comma
	) +
	scale_x_datetime(
		date_breaks = "1 week",
		date_minor_breaks = "1 day",
		labels = function(breaks) {
			# Put the year on the first break.
			strftime(breaks, ifelse(cumsum(!is.na(breaks)) == 1, "%b %d\n%Y", "%b %d"), tz = "UTC")
		}
	) +
	coord_cartesian(xlim = DATE_LIMITS, expand = FALSE) +
	guides(alpha = "none") +
	labs(
		x = NULL,
		y = NULL
	)
ggsave(output_path, p, width = DOCUMENT_LINEWIDTH, height = 1.4, dpi = 300)
