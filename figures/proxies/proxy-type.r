# Makes a graph showing the number of unique Snowflake proxy IP addresses per
# day, by type.
#
# Usage:
#   Rscript proxy-type.r proxy-type.csv proxy-type.pdf

library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd(c(
	# "2019-07-01",
	"2021-01-01",
	"2023-05-30"
))

# Timeline of events relevant to proxy type measurement.
#
# 2019-12-03 WebExtension version 0.2.0 is released, broker starts recording proxy types.
# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/31157#note_2593925
#
# 2022-03-21 Snowflake begins to recognize "iptproxy" as a probe type.
# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40104
# https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/merge_requests/82
#
# 2022-05-03 IPtProxy 1.6.0 adds 'ProxyType: "iptproxy"'.
# https://github.com/tladesignz/IPtProxy/commit/c6ba25ef6ce8449476f734c626eadffdf55d0519
#
# 2022-06-21 Broker deployment; "unknown" disappears and "iptproxy" appears in proxy-type.csv.
# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40151
#
# 2022-07-05 Orbot 16.6.2 RC 1 upgrades to iPtProxy 1.6.0.
# https://github.com/guardianproject/orbot/releases/tag/16.6.2-RC-1-tor.0.4.7.8

(function() {
	args <- commandArgs(trailingOnly = TRUE)
	if (length(args) != 2) {
		stop("usage: Rscript proxy-type.r proxy-type.csv proxy-type.pdf")
	}
	proxy_type_csv_path <<- args[[1]]
	output_path <<- args[[2]]
})()

proxy_type <- read_csv(proxy_type_csv_path, col_types = cols(
	date = col_date(),
	type = col_character(),
	unique_ips = col_double(),
	coverage = col_double()
)) %>%
	# Unlike in the user graphs, here we do not compensate for days where
	# coverage < 1.0: because of deduplication, unique_ips does not scale
	# linearly with time like the number of concurrent users does.

	mutate(type = case_when(
		# Proxies did not report their type before 2020-12-03.
		# Visually, the "Unknown" series before that date matches up
		# with the "WebExtension" series after that date. There were
		# probably a relatively small number of standalone and web
		# bage, but go ahead and attribute all to WebExtension.
		# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/31157#note_2593925
		is.na(type) & date <= "2020-12-03" ~ "webext",
		# After that, attribute unknown proxy types until 2022-06-21 to
		# iptproxy. IPtProxy reported its type as "iptproxy", but this
		# value was not recognized by the broker until the deployment
		# of 2022-06-21.
		# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40151
		is.na(type) & date <= "2022-06-21" ~ "iptproxy",
		TRUE ~ type
	)) %>%

	# Put a label on the rows with type == NA.
	replace_na(list(type = "unknown")) %>%

	# Order the types by the value of unique_ips at the right side of the graph.
	mutate(type = fct_reorder2(type, date, unique_ips)) %>%

	# Fill in entirely missing dates with NA.
	group_by(type) %>%
	complete(date = seq.Date(min(date), max(date), "days")) %>%
	ungroup() %>%

	# Keep only the records within DATE_LIMITS.
	filter(lubridate::`%within%`(date, do.call(lubridate::interval, as.list(DATE_LIMITS)))) %>%

	# Make better labels for proxy types.
	mutate(type = fct_recode(type,
		"Browser\nextension" = "webext",
		"Orbot" = "iptproxy",
		"Standalone" = "standalone",
		"Web badge" = "badge",
		"Unknown" = "unknown"
	))

# To add a "total" series:
# proxy_type <- bind_rows(
# 	proxy_type,
# 	proxy_type %>%
# 		group_by(date) %>%
# 		summarize(
# 			unique_ips = sum(unique_ips, na.rm = TRUE),
# 			coverage = mean(coverage, na.rm = TRUE)
# 		) %>%
# 		mutate(type = "total")
# )

p <- ggplot() +
	# TODO: event annotations?

	# Data series.
	geom_line(
		data = proxy_type, aes(
			x = date,
			# Divide by coverage (what fraction of of a day each
			# unique_ips sample represents, between 0.0 and 1.0) to
			# remove illusory dips caused by partial days' data.
			y = unique_ips / coverage,
			color = type
		)
	) +

	# Draw the type labels in the right margin. theme(plot.margin) below
	# makes room for this.
	geom_text(
		data = proxy_type %>%
			group_by(type) %>%
				arrange(desc(date)) %>%
				slice(1) %>%
			ungroup() %>%
			mutate(date = max(date)) %>%
			mutate(y = place_no_overlap(unique_ips, -10000, 6500)),
		aes(
			x = date,
			y = y,
			color = type,
			label = type
		),
		position = position_nudge(x = 5),
		size = 2,
		lineheight = 0.8,
		hjust = 0, vjust = 0.5
	) +

	scale_y_continuous(
		minor_breaks = NULL,
		labels = scales::comma
	) +
	scale_x_date(
		date_breaks = "1 year",
		date_minor_breaks = "1 month",
		date_labels = "%Y"
	) +
	scale_color_brewer(palette = "Set1") +
	coord_cartesian(xlim = DATE_LIMITS, ylim = c(0, NA), expand = FALSE, clip = "off") +
	labs(x = NULL, y = "Unique proxy IP addresses") +

	# Make room for the margin labels added by geom_text above.
	theme(plot.margin = unit(c(0, 10, 0, 0), "mm")) +
	guides(color = "none")
ggsave(output_path, p, width = DOCUMENT_LINEWIDTH, height = 1.5)
