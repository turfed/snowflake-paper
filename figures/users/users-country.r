# Makes a graph showing the estimated number of simultaneous Snowflake users
# in Russia, around the time of blocking events.
#
# Usage:
#   Rscript users-ru.r userstats-bridge-combined-multi.csv users-ru.pdf

library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd(c(
	"2021-11-01",
	# 2021-12-01 first blocking https://bugs.torproject.org/tpo/community/support/40050
	# 2022-02-15 release of Tor Browser 12.0.3 with altered fingerprint https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40030#note_2893870
	"2023-03-15"
))

WANTED_FINGERPRINTS <- c(
	"7659DA0F96B156C322FBFF3ACCC9B9DC01C27C73" = "snowman",
	"5481936581E23D2D178105D44DB6915AB06BFB7F" = "snowflake-01",
	"91DA221A149007D0FD9E5515F5786C3DD07E4BB0" = "snowflake-02"
)

# Return an abbreviation for the month, followed by a year for January only.
date_labels <- function(breaks) {
	strftime(breaks, ifelse(!is.na(breaks) & lubridate::month(breaks) == 1, "%b\n%Y", "%b"), tz = "UTC")
}

(function() {
	args <- commandArgs(trailingOnly = TRUE)
	if (length(args) != 2) {
		stop("usage: Rscript users-ru.r userstats-bridge-combined-multi.csv users-ru.pdf")
	}
	bridge_combined_multi_csv_path <<- args[[1]]
	output_path <<- args[[2]]
})()

bridge_combined_multi <- read_csv(bridge_combined_multi_csv_path, comment = "#") %>%
	# Keep only the transports and bridges we care about.
	filter(transport == "snowflake" & fingerprint %in% names(WANTED_FINGERPRINTS)) %>%

	# Adjust users by frac, and forget frac. This is just for completeness,
	# as we expect the input files to have frac == 100 everywhere.
	mutate(
		across(c(low, high), ~ .x * frac / 100),
		frac = NULL,
		users = (low + high) / 2
	)

bridge_combined <- bridge_combined_multi %>%
	# Sum the contributions of all bridge fingerprints by day.
	group_by(date, country, transport) %>%
	summarize(users = sum(users, na.rm = TRUE), .groups = "drop") %>%

	# Fill in entirely missing dates with NA.
	group_by(country, transport) %>%
	complete(date = seq.Date(min(date), max(date), "days")) %>%
	ungroup()

# TODO: assign country "??" proportionally to other countries.
# "WS.makeWebsocket ignores params (i.e. `client_ip`), losing country statistics" https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake-webext/82

p <- ggplot() +
	# Data series.
	geom_line(
		data = bridge_combined %>% filter(country == "ru"),
		aes(x = date, y = users),
		size = LINE_SIZE
	) +

	scale_x_date(
		date_breaks = "1 month",
		minor_breaks = NULL,
		labels = date_labels
	) +
	coord_cartesian(xlim = DATE_LIMITS, expand = FALSE) +

	COMMON_THEME +
	theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
	labs(x = NULL, y = NULL)
ggsave(output_path, p, width = DOCUMENT_LINEWIDTH, height = 1.5)
