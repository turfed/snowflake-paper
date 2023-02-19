# Makes a graph showing the estimated number of simultaneous Snowflake users.
#
# Usage:
#   Rscript users-global.r userstats-bridge-transport.csv users-global.pdf
#
# userstats-bridge-transport.csv comes from the program
# userstats-bridge-transport.

library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd(c(
	"2020-12-31",
	"2022-12-01"
))

LINE_SIZE <- 0.2

GAPS <- tribble(
	~begin, ~end, ~y,
	# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40033#note_2735468
	# https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/merge_requests/43#note_2740276
	"2021-05-12 14:13:56", "2021-06-21 14:22:24", 8800
) %>% mutate(
	begin = lubridate::ymd_hms(begin) %>% lubridate::as_date(),
	end = lubridate::ymd_hms(end) %>% lubridate::as_date()
)

EVENTS <- tribble(
	~date, ~y, ~label,
	# "2017-01-24 00:00:00",  1000, "Tor Browser 7.0a1\nSnowflake for Linux",      # https://blog.torproject.org/blog/tor-browser-70a1-released
	# "2017-08-08 00:00:00",  1000, "Tor Browser 7.5a4\nSnowflake for macOS",      # https://blog.torproject.org/blog/tor-browser-75a4-released
	# "2018-04-13 16:00:00",  1000, "App Engine domain fronting disabled",         # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/25804
	# "2018-05-09 20:46:16",  1000, "Rendezvous moved to Azure",                   # https://bugs.torproject.org/tpo/applications/tor-browser/26010
	# "2019-10-01 00:00:00",  1000, "Tor Browser 9.0a7\nSnowflake for Windows",    # https://blog.torproject.org/new-release-tor-browser-90a7
	# "2020-05-22 19:51:29",  1000, "Tor Browser 9.5a13\nadds Turbo Tunnel",       # https://blog.torproject.org/new-release-tor-browser-95a13
	# "2020-06-02 18:09:48",  1000, "Tor Browser 10.0a1\nSnowflake for Android",   # https://blog.torproject.org/new-release-tor-browser-100a1
	"2021-07-06 16:56:37", 21000, "Tor Browser 10.5\nincludes Snowflake",          # https://blog.torproject.org/new-release-tor-browser-105
	"2021-12-01 00:00:00", 28800, "Onset of Tor blocking in Russia",               # https://bugs.torproject.org/tpo/community/support/40050
	"2021-12-14 00:00:00", 32800, "",                                              # https://blog.torproject.org/new-release-tor-browser-115a1/
	"2021-12-20 00:00:00", 42000, "Tor Browser 11.5a1 and 11.0.3\nalter DTLS fingerprint", # https://blog.torproject.org/new-release-tor-browser-1103/
	"2022-01-25 17:41:00", 50000, "Load balancing of bridge",                      # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40095#note_2772325
	# "2022-01-31 18:20:00", 12000, "Back to production bridge, now load-balanced",# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40095#note_2773704
	"2022-02-24 00:00:00", 60000, "Russian invasion of Ukraine",
	"2022-03-16 16:51:35", 67000, "Bridge hardware upgrade",                       # https://bugs.torproject.org/tpo/tpa/team/40664#note_2787624
	# "2022-03-18 03:21:45", 22000, "Fixed problem with onion keys",               # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40110#note_2788622
	# "2022-04-11 15:49:30", 22000, "Bridge server migration",                     # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40111#note_2794860
	"2022-07-14 00:00:00", 80000, "Tor Browser 11.5\nautomatic configuration",     # https://blog.torproject.org/new-release-tor-browser-115/
	"2022-09-20 00:00:00", 90000, "Protests in Iran",                              # https://lists.torproject.org/pipermail/anti-censorship-team/2022-September/000247.html
	"2022-10-04 17:15:00",110000, "TLS fingerprint blocking in Iran",              # https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40207#note_2849437
	"2022-10-27 00:00:00",117500, "Tor Browser 11.5.6 fixes TLS fingerprint",      # https://blog.torproject.org/new-release-tor-browser-1156/
	# "2022-11-01 00:00:00",130000, "Orbot 16.6.3-RC-1-tor.0.4.7.10 fixes TLS fingerprint"               # https://github.com/guardianproject/orbot/releases/tag/16.6.3-RC-1-tor.0.4.7.10
) %>% mutate(date = lubridate::ymd_hms(date) %>% lubridate::as_date())

missing_dates_fill_na <- function(tb) {
	right_join(tb, tibble(date = seq.Date(min(tb$date), max(tb$date), "days")), by = c("date"))
}

# Return a one-letter abbreviation for the month, followed by a year for
# January only.
date_labels <- function(breaks) {
	mon <- substr(strftime(breaks, "%b"), 1, 1)
	year <- strftime(breaks, "%Y")
	ifelse(!is.na(breaks) & lubridate::month(breaks) == 1, paste(mon, year, sep = "\n"), mon)
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
			size = 0.25
		),
		geom_label(data = data, aes(x = x, y = ymax, label = label),
			fill = "#ffffffc0",
			label.size = 0,
			label.r = unit(0, "lines"),
			label.padding = unit(0.05, "lines"),
			family = FONT_FAMILY,
			size = 2.0,
			hjust = 1,
			nudge_x = -2,
			vjust = ifelse(data$ymin <= data$ymax, 1, 0),
			lineheight = 0.8
		)
	)
}

(function() {
	args <- commandArgs(trailingOnly = TRUE)
	if (length(args) != 2) {
		stop("usage: Rscript users-global.r userstats-bridge-transport.csv users-global.pdf")
	}
	bridge_transport_csv_path <<- args[[1]]
	output_path <<- args[[2]]
})()

bridge_transport <- read_csv(bridge_transport_csv_path, comment = "#") %>%
	filter(transport == "snowflake") %>%
	missing_dates_fill_na %>%
	mutate(users = users * frac / 100, frac = NULL)

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
	geom_line(data = bridge_transport, aes(x = date, y = users), size = LINE_SIZE) +

	scale_y_continuous(
		limits = c(0, 123000),
		breaks = 10000*0:ceiling(123000 / 10000),
		minor_breaks = NULL,
		labels = scales::comma
	) +
	scale_x_date(
		date_breaks = "1 month",
		minor_breaks = NULL,
		labels = date_labels
	) +
	coord_cartesian(xlim = DATE_LIMITS, expand = FALSE) +
	COMMON_THEME +
	labs(x = NULL, y = "Average simultaneous users")
ggsave(output_path, p, width = DOCUMENT_TEXTWIDTH, height = 2)
