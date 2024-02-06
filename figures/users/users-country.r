# Makes a graph showing the estimated number of simultaneous Snowflake users
# in various countries, around the time of respective blocking events.
#
# Usage:
#   Rscript users-country.r userstats-bridge-combined-multi.csv

library("tidyverse")
library("cowplot")

source("../common.r")

HEIGHT <- 1.25

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

	# Compensate for days when not all descriptors were published.
	mutate(across(c(low, high), ~ .x / (coverage / pmax(num_instances, coverage)))) %>%

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

EVENTS <- tribble(
	~country, ~date,
	"tm", "2021-10-25", # Blocking of default front domain https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40024
	"ru", "2021-12-01", # Tor blocking, DTLS Server Hello supported_groups https://bugs.torproject.org/tpo/community/support/40050
	"tm", "2022-05-03", # Partial unblocking of front domain https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40024#note_2930186
	"ru", "2022-05-16", # DTLS Client Hello supported_groups specific contents https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40030
	"ru", "2022-07-21", # https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40030#note_2823140
	"tm", "2022-08-03", # Possible blocking of STUN ports
	"ir", "2022-09-21", # Mahsa Amini protests https://lists.torproject.org/pipermail/anti-censorship-team/2022-September/000247.html
	"ru", "2022-09-30", # Decrease in users from an unknown cause
	"ir", "2022-10-07", # TLS fingerprint blocking https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40207
	"ir", "2022-10-29", # Release of Tor Browser 11.5.6 with uTLS support https://blog.torproject.org/new-release-tor-browser-1156/
	"ir", "2023-01-20", # Blocking of domain fronting rendezvous https://bugs.torproject.org/tpo/anti-censorship/team/115\#note_2873040
	"ru", "2023-02-15", # Release of Tor Browser 12.0.3 with DTLS Hello Verify Request countermeasure https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40030#note_2893870
	"cn", "2023-05-13", # Blocking of repeated HTTPS connections to the same SNI https://bugs.torproject.org/tpo/anti-censorship/censorship-analysis/40038
) %>%
	mutate(date = lubridate::ymd(date))

PLOT_INFO <- list(
	list(
		country = "cn",
		end_date = lubridate::ymd("2023-07-31")
	),
	list(
		country = "ir",
		end_date = lubridate::ymd("2023-07-31")
	),
	list(
		country = "ru",
		end_date = lubridate::ymd("2023-07-31")
	),
	list(
		country = "tm",
		end_date = lubridate::ymd("2022-10-10")
	)
)
plots <- lapply(PLOT_INFO, function(g) {
	# All the graphs show an equal amount of elapsed time, though the end
	# point may differ.
	date_limits <- c(g$end_date - 650, g$end_date)
	# Interpolated function to automaticlly put event markers near the data
	# lines.
	bridge_combined_fn <- with(bridge_combined %>% filter(country == g$country), approxfun(date, users))
	ggplot() +
		geom_point(
			data = EVENTS %>% filter(country == g$country),
			aes(x = date, y = bridge_combined_fn(date)),
			shape = 1,
			color = "#d97062",
			size = 3.75
		) +
		geom_line(
			data = bridge_combined %>% filter(country == g$country),
			aes(x = date, y = users),
		) +
		scale_y_continuous(
			minor_breaks = NULL,
			labels = scales::comma
		) +
		scale_x_date(
			date_breaks = "1 month",
			minor_breaks = NULL,
			labels = date_labels_abbrev,
		) +
		coord_cartesian(xlim = date_limits, expand = FALSE) +
		labs(x = NULL, y = NULL)
})
# Make the horizontal axis the same size in each graph.
plots <- align_plots(plotlist = plots, align = "v", axis = "lr")
for (i in 1:length(plots)) {
	ggsave(sprintf("users-%s.pdf", PLOT_INFO[[i]]$country), plots[[i]], width = DOCUMENT_LINEWIDTH, height = HEIGHT)
}
