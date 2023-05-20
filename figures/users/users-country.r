# Makes a graph showing the estimated number of simultaneous Snowflake users
# in various countries, around the time of respective blocking events.
#
# Usage:
#   Rscript users-country.r userstats-bridge-combined-multi.csv

library("tidyverse")

source("../common.r")

HEIGHT <- 1.5

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

	# Derive a single user count from each day's low–high range.
	mutate(users = (low + high) / 2) %>%

	# Reassign users from the country "??" proportionally to other
	# countries. Versions of snowflake-webext between 0.6.0 (2022-06-27)
	# and 0.7.2 (2023-04-10) had a bug where they did not report client IP
	# addresses, so all their users (which varied as a fraction between 5%
	# and 25% of all users) got assigned to "??". (Otherwise the fraction
	# of "??" users is usually below 1%.) This adjustment is to avoid the
	# loss of 5–25% of all country-specific counts between 2022-06-27 and
	# 2023-04-10, as well as to avoid illusory steps at the endpoints. The
	# assumption we're making is that the country distribution of users
	# that connect through snowflake-webext is the same as connect through
	# other proxy types.
	#
	# "WS.makeWebsocket ignores params (i.e. `client_ip`), losing country statistics" https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake-webext/82
	# "2023 April update" https://opencollective.com/censorship-circumvention/projects/snowflake-daily-operations/updates/2023-april-update
	group_by(date, transport) %>%
	# Count the total users, including those from "??".
	mutate(total_users = sum(users)) %>%
	# Delete the "??" rows, unless "??" is the only represented country for a day.
	filter(country != "??" | n() <= 1) %>%
	# Take each per-country fraction of users out of the total not including "??",
	# and scale it up to the total including "??".
	mutate(users = users/sum(users) * total_users) %>%
	ungroup()

bridge_combined <- bridge_combined_multi %>%
	# Sum the contributions of all bridge fingerprints by day.
	group_by(date, country, transport) %>%
	summarize(users = sum(users, na.rm = TRUE), .groups = "drop") %>%

	# Fill in entirely missing dates with NA.
	group_by(country, transport) %>%
	complete(date = seq.Date(min(date), max(date), "days")) %>%
	ungroup()

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
		) +
		scale_x_date(
			date_breaks = "1 month",
			minor_breaks = NULL,
			labels = g$date_labels
		) +
		coord_cartesian(xlim = g$date_limits, expand = FALSE) +
		labs(x = NULL, y = NULL)
	ggsave(sprintf("users-%s.pdf", g$country), p, width = DOCUMENT_LINEWIDTH, height = HEIGHT)
}
