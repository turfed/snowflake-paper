# Makes a graph showing the estimated number of simultaneous Snowflake users
# worldwide.
#
# Usage:
#   Rscript users-global.r userstats-bridge-transport-multi.csv users-global.pdf

library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd(c(
	"2020-12-31",
	"2023-05-31"
))

GAPS <- tribble(
	~begin, ~end, ~y, ~left_side,
	# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40033#note_2735468
	# https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/merge_requests/43#note_2740276
	"2021-05-12 14:13:56", "2021-06-21 14:22:24", 8800, T
) %>% mutate(
	begin = lubridate::ymd_hms(begin) %>% lubridate::as_date(),
	end = lubridate::ymd_hms(end) %>% lubridate::as_date()
)

EVENTS <- tribble(
	~date, ~y, ~left_side, ~label,
	# "2017-01-24 00:00:00",  1000, T, "Tor Browser 7.0a1\nSnowflake for Linux",      # https://blog.torproject.org/blog/tor-browser-70a1-released
	# "2017-08-08 00:00:00",  1000, T, "Tor Browser 7.5a4\nSnowflake for macOS",      # https://blog.torproject.org/blog/tor-browser-75a4-released
	# "2018-04-13 16:00:00",  1000, T, "App Engine domain fronting disabled",         # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/25804
	# "2018-05-09 20:46:16",  1000, T, "Rendezvous moved to Azure",                   # https://bugs.torproject.org/tpo/applications/tor-browser/26010
	# "2019-10-01 00:00:00",  1000, T, "Tor Browser 9.0a7\nSnowflake for Windows",    # https://blog.torproject.org/new-release-tor-browser-90a7
	# "2020-05-22 19:51:29",  1000, T, "Tor Browser 9.5a13\nadds Turbo Tunnel",       # https://blog.torproject.org/new-release-tor-browser-95a13
	# "2020-06-02 18:09:48",  1000, T, "Tor Browser 10.0a1\nSnowflake for Android",   # https://blog.torproject.org/new-release-tor-browser-100a1
	"2021-01-12 00:00:00", 23000, F, "Orbot 16.4.0\nincludes Snowflake",              # https://github.com/guardianproject/orbot/releases/tag/16.4.0-RC-1-tor-0.4.4.6
	"2021-07-06 16:56:37", 30000, T, "Tor Browser 10.5\nincludes Snowflake",          # https://blog.torproject.org/new-release-tor-browser-105
	"2021-12-01 00:00:00", 25000, T, "Onset of Tor blocking\nin Russia",               # https://bugs.torproject.org/tpo/community/support/40050
	"2021-12-14 00:00:00", 28000, T, "",                                              # https://blog.torproject.org/new-release-tor-browser-115a1/
	"2021-12-20 00:00:00", 42000, T, "Tor Browser 11.5a1 and 11.0.3\nalter DTLS fingerprint", # https://blog.torproject.org/new-release-tor-browser-1103/
	"2022-01-25 17:41:00", 54000, T, "Load balancing of bridge",                      # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40095#note_2772325
	# "2022-01-31 18:20:00", 12000, T, "Back to production bridge, now load-balanced",# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40095#note_2773704
	"2022-02-24 00:00:00", 64000, T, "Russian invasion of Ukraine",
	"2022-03-16 16:51:35", 74000, T, "Bridge hardware upgrade",                       # https://bugs.torproject.org/tpo/tpa/team/40664#note_2787624
	# "2022-03-18 03:21:45", 22000, T, "Fixed problem with onion keys",               # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40110#note_2788622
	# "2022-04-11 15:49:30", 22000, T, "Bridge server migration",                     # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40111#note_2794860
	"2022-07-14 00:00:00", 40000, T, "Tor Browser 11.5\nautomatic configuration",     # https://blog.torproject.org/new-release-tor-browser-115/
	"2022-09-20 00:00:00",100000, T, "Protests in Iran",                              # https://lists.torproject.org/pipermail/anti-censorship-team/2022-September/000247.html
	"2022-10-04 17:15:00",115000, T, "TLS fingerprint blocking in Iran",              # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40207#note_2849437
	"2022-10-27 00:00:00",115000, T, "",                                              # https://blog.torproject.org/new-release-tor-browser-1156/
	"2022-11-01 00:00:00",129000, T, "Tor Browser 11.5.6 and Orbot 16.6.3\nfix TLS fingerprint", # https://github.com/guardianproject/orbot/releases/tag/16.6.3-RC-1-tor.0.4.7.10
	"2022-12-07 00:00:00",138000, T, "Tor Browser 12.0 adds a second bridge",         # https://blog.torproject.org/new-release-tor-browser-120/
	"2023-01-16 00:00:00", 33000, T, "",                                              # https://bugs.torproject.org/tpo/anti-censorship/team/115
	"2023-01-24 00:00:00",  8000, T, "Domain fronting\nrendezvous\ntemporarily\nblocked in Iran", # https://bugs.torproject.org/tpo/anti-censorship/team/115
	# "2023-01-31 00:00:00", 33000, T, "",                                            # https://bugs.torproject.org/tpo/anti-censorship/team/115#note_2876012
	# "2023-02-02 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://bugs.torproject.org/tpo/anti-censorship/team/115#note_2876012
	# "2023-02-10 00:00:00", 33000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2883298
	# "2023-02-14 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2883298
	"2023-02-15 00:00:00",117000, T, "Tor Browser 12.0.3\nalters DTLS fingerprint",   # https://blog.torproject.org/new-release-tor-browser-1203/
	# "2023-02-19 00:00:00",  8000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2883298
	# "2023-02-20 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2883298
	# "2023-02-22 00:00:00",  8000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2883298
	# "2023-02-23 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2883298
	"2023-03-13 00:00:00",128000, T, "Bridge performance fix",                        # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40262#note_2886041
	# "2023-03-15 00:00:00", 10000, T, "Orbot 17.0.0 BETA 2 adds a second bridge",    # https://github.com/guardianproject/orbot/releases/tag/17.0.0-BETA-2-tor.0.4.7.11
	# "2023-03-03 00:00:00",  8000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-04 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-08 00:00:00",  8000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-09 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-13 00:00:00",  8000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-14 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-19 00:00:00",  8000, T, "",                                            # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
	# "2023-03-20 00:00:00",  8000, T, "Domain fronting rendezvous again blocked in Iran", # https://gitlab.torproject.org/tpo/anti-censorship/team/-/issues/115#note_2892825
) %>% mutate(date = lubridate::ymd_hms(date) %>% lubridate::as_date())

# Return an abbreviation for the month, followed by a year for January only.
date_labels <- function(breaks) {
	strftime(breaks, ifelse(!is.na(breaks) & lubridate::month(breaks) == 1, "%b\n%Y", "%b"), tz = "UTC")
}

# For each element of `at`, returns the minimum element of `values` whose
# corresponding element of `x` is within `radius` of the `at` element.
min_nearby <- function(x, values, at, radius) {
	in_radius <- abs(outer(x, at, `-`)) <= radius
	values_in_radius <- apply(in_radius, 2, function(x) values[x])
	apply(values_in_radius, 2, min, na.rm = TRUE)
}

# For each element of `at`, returns the maximum element of `values` whose
# corresponding element of `x` is within `radius` of the `at` element.
max_nearby <- function(x, values, at, radius) {
	in_radius <- abs(outer(x, at, `-`)) <= radius
	values_in_radius <- apply(in_radius, 2, function(x) values[x])
	apply(values_in_radius, 2, max, na.rm = TRUE)
}

# Returns which of a1 and a2 is closer to x in absolute value.
closer <- function(x, a1, a2) {
	ifelse(abs(x - a1) < abs(x - a2), a1, a2)
}

# Draw a vertical line from ymin to ymax at x, and draw a label next to the line.
text_annotation <- function(data) {
	list(
		geom_linerange(data = data, aes(x = x, ymin = ymin, ymax = ymax),
			color = "#808080",
			linewidth = 0.25
		),
		geom_label(data = data,
			aes(
				x = x + ifelse(left_side, -2, +2),
				y = ymax,
				label = label,
				hjust = ifelse(left_side, 1, 0)
			),
			fill = "#ffffffc0",
			label.size = 0,
			label.r = unit(0, "lines"),
			label.padding = unit(0.05, "lines"),
			size = 2.0,
			vjust = ifelse(data$ymin <= data$ymax, 1, 0),
			lineheight = 0.8
		)
	)
}

(function() {
	args <- commandArgs(trailingOnly = TRUE)
	if (length(args) != 2) {
		stop("usage: Rscript users-global.r userstats-bridge-transport-multi.csv users-global.pdf")
	}
	bridge_transport_multi_csv_path <<- args[[1]]
	output_path <<- args[[2]]
})()

bridge_transport_multi <- read_csv(bridge_transport_multi_csv_path, comment = "#") %>%
	# Keep only the transports and bridges we care about.
	filter(transport == "snowflake" & fingerprint %in% names(WANTED_FINGERPRINTS)) %>%

	# Compensate for days when not all descriptors were published.
	mutate(users = users / (coverage / pmax(num_instances, coverage)))

bridge_transport <- bridge_transport_multi %>%
	# Sum the contributions of all bridge fingerprints by day.
	group_by(date, transport) %>%
	summarize(users = sum(users, na.rm = TRUE), .groups = "drop") %>%

	# Keep only the records within DATE_LIMITS (necessary to avoid
	# interference with coord_cartesian(clip = "off") below.
	filter(lubridate::`%within%`(date, do.call(lubridate::interval, as.list(DATE_LIMITS)))) %>%

	# Fill in entirely missing dates with NA.
	group_by(transport) %>%
	complete(date = seq.Date(min(date), max(date), "days")) %>%
	ungroup()

max_users <- max(bridge_transport$users, na.rm = TRUE)

p <- ggplot() +
	# Gaps in the data.
	geom_rect(data = GAPS,
		aes(
			xmin = begin,
			xmax = end,
			ymin = 0,
			ymax = max(
				max_nearby(bridge_transport$date, bridge_transport$users, begin, 2),
				max_nearby(bridge_transport$date, bridge_transport$users, end, 2)
			) + max_users * 0.01
		),
		fill = "gray",
		alpha = 0.8
	) +
	text_annotation(GAPS %>% mutate(
		x = mean(c(begin, end)),
		ymin = max(
			max_nearby(bridge_transport$date, bridge_transport$users, begin, 2),
			max_nearby(bridge_transport$date, bridge_transport$users, end, 2)
		) + max_users * 0.02,
		ymax = y,
		label = "Data error"
	)) +

	# Event annotations.
	text_annotation(EVENTS %>% mutate(
		x = date,
		# Place the bottom of the indicator line 1% of the data range above or below nearby values.
		ymin = closer(y,
			max_nearby(bridge_transport$date, bridge_transport$users, date, 2) + max_users * 0.01,
			min_nearby(bridge_transport$date, bridge_transport$users, date, 2) - max_users * 0.01),
		ymax = y,
		label = label
	)) +

	# Data series.
	geom_line(data = bridge_transport, aes(x = date, y = users)) +

	scale_y_continuous(
		breaks = 20000*0:ceiling(144000 / 20000),
		minor_breaks = NULL,
		labels = scales::comma
	) +
	scale_x_date(
		date_breaks = "1 month",
		minor_breaks = NULL,
		labels = date_labels
	) +
	coord_cartesian(xlim = DATE_LIMITS, ylim = c(0, 100000), expand = FALSE, clip = "off") +
	theme(plot.margin = unit(c(13, 0, 0, 0), "mm")) +
	labs(x = NULL, y = "Average simultaneous users")
ggsave(output_path, p, width = DOCUMENT_TEXTWIDTH, height = 2)
