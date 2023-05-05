# Makes a graph showing the estimated number of simultaneous Snowflake users
# in various countries, around the time of respective blocking events.
#
# Usage:
#   Rscript users-country.r userstats-bridge-combined-multi.csv

library("tidyverse")

source("../common.r")

HEIGHT <- 1.5

WANTED_FINGERPRINTS <- c(
	"7659DA0F96B156C322FBFF3ACCC9B9DC01C27C73" = "snowman",
	"5481936581E23D2D178105D44DB6915AB06BFB7F" = "snowflake-01",
	"91DA221A149007D0FD9E5515F5786C3DD07E4BB0" = "snowflake-02"
)

# Return an abbreviation for the month, followed by a year for January only.
date_labels <- function(breaks) {
	strftime(breaks, ifelse(!is.na(breaks) & lubridate::month(breaks) == 1, "%b\n%Y", "%b"), tz = "UTC")
}

# Return a one-letter abbreviation for the month, followed by a year for January only.
date_labels_abbrev <- function(breaks) {
	paste0(
		substr(strftime(breaks, "%b", tz = "UTC"), 1, 1),
		ifelse(!is.na(breaks) & lubridate::month(breaks) == 1, strftime(breaks, "\n%Y", tz = "UTC"), "")
	)
}

(function() {
	args <- commandArgs(trailingOnly = TRUE)
	if (length(args) != 1) {
		stop("usage: Rscript users-country.r userstats-bridge-combined-multi.csv")
	}
	bridge_combined_multi_csv_path <<- args[[1]]
})()

bridge_combined_multi <- read_csv(bridge_combined_multi_csv_path, comment = "#") %>%
	# Keep only the transports and bridges we care about.
	filter(transport == "snowflake" & fingerprint %in% names(WANTED_FINGERPRINTS)) %>%

	# Adjust low and high by frac, and forget frac. This is just for
	# completeness, as we expect the input files to have frac == 100
	# everywhere.
	mutate(across(c(low, high), ~ .x * frac / 100), frac = NULL) %>%

	# Derive a single user count from each day's low–high range. The
	# formulas from https://metrics.torproject.org/reproducible-metrics.html#bridge-users
	# can result in a "low" bound that is higher than the "high" bound, so
	# take the minimum of "low" and "high" as the true low, then take the
	# average. The raw "low" is noisy, especially when there is less data,
	# as in Turkmenistan.
	mutate(users = (pmin(low, high) + high) / 2)

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

for (g in list(
	list(
		country = "ru",
		date_limits = lubridate::ymd(c(
			"2021-11-01",
			# 2021-12-01 first blocking https://bugs.torproject.org/tpo/community/support/40050
			# 2022-02-15 release of Tor Browser 12.0.3 with altered fingerprint https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40030#note_2893870
			"2023-03-31"
		)),
		date_labels = date_labels
	),
	list(
		country = "ir",
		date_limits = lubridate::ymd(c(
			"2022-09-01",
			"2023-04-30"
		)),
		date_labels = date_labels
	),
	list(
		country = "tm",
		date_limits = lubridate::ymd(c(
			"2020-10-01",
			"2022-09-30"
		)),
		date_labels = date_labels_abbrev
	)
)) {
	p <- ggplot() +
		geom_line(
			data = bridge_combined %>% filter(country == g$country),
			aes(x = date, y = users),
			size = LINE_SIZE
		) +
		scale_x_date(
			date_breaks = "1 month",
			minor_breaks = NULL,
			labels = g$date_labels
		) +
		coord_cartesian(xlim = g$date_limits, expand = FALSE) +
		COMMON_THEME +
		labs(x = NULL, y = NULL)
	ggsave(sprintf("users-%s.pdf", g$country), p, width = DOCUMENT_LINEWIDTH, height = HEIGHT)
}
