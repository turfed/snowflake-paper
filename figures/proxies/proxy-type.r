# Makes a graph showing the number of unique Snowflake proxy IP addresses per
# day, by type.
#
# Usage:
#   Rscript proxy-type.r [--hyphen-hack] proxy-type.csv proxy-type.pdf

library("argparse")
library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd(c(
	"2021-07-01",
	"2024-03-03"
))

# Timeline of events relevant to proxy type measurement. The tricky part is
# Orbot (IPtProxy) types. From 2021-02-23 to 2022-01-03, Orbot reported its
# type as "standalone" and it is not possible to separate it from actual
# standalone command-line proxies during this time. From 2022-01-03 to
# 2022-06-21, Orbot reported its type as "iptproxy", but the broker, not
# recognizing the type string, recorded it as unknown. From 2022-06-21 on, the
# broker properly records "iptproxy" as the type for Orbot proxies.
#
# 2019-12-03 WebExtension version 0.2.0 is released, broker starts recording proxy types.
# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/31157#note_2593925
#
# 2021-02-23 Orbot 16.4.1-BETA-2 adds Snowflake proxy support, reporting its type as "standalone".
# https://github.com/guardianproject/orbot/releases/tag/16.4.1-BETA-2-tor.0.4.4.6
#
#   2021-07-14 IPtProxy 1.1.0 changes its type from "standalone" to "iptproxy"—not released yet.
#   https://github.com/tladesignz/IPtProxy/commit/228e9e61e285ee548a42d6bee487577e44630695
#   2021-12-20 Orbot updates IPtProxy from 1.0.0 to 1.2.0—not released yet.
#   https://github.com/guardianproject/orbot/commit/57add48cd904afe94363219887cd142bb5cf6696
#   https://github.com/guardianproject/orbot/releases/tag/16.5.2-RC-1-tor.0.4.6.8
#
# 2022-01-03 Orbot 16.5.2-RC-5 is released, now reporting its type as "iptproxy".
# https://github.com/guardianproject/orbot/releases/tag/16.5.2-RC-5-tor.0.4.6.9
#
#   2022-03-21 Snowflake broker begins to recognize "iptproxy" as a probe type—not deployed yet.
#   https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40104
#   https://gitlab.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/-/merge_requests/82
#
# 2022-06-21 Broker deployment; broker starts recording the "iptproxy" type.
# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40151

(function() {
	parser <- ArgumentParser()
	parser$add_argument("--hyphen-hack", action="store_true", default=FALSE, help="convert hyphen to soft hyphen")
	parser$add_argument("proxy_type_csv_path", nargs=1)
	parser$add_argument("output_path", nargs=1)
	args <- parser$parse_args()
	hyphen_hack <<- args$hyphen_hack
	proxy_type_csv_path <<- args$proxy_type_csv_path
	output_path <<- args$output_path
})()

proxy_type <- read_csv(proxy_type_csv_path, col_types = cols(
	date = col_date(),
	type = col_character(),
	unique_ips = col_double(),
	coverage = col_double()
)) %>%
	# Assign unknown types according to date ranges.
	mutate(type = case_when(
		# Proxies did not report their type before 2020-12-03.
		# Visually, the "Unknown" series before that date matches up
		# with the "WebExtension" series after that date. There were
		# probably a relatively small number of standalone and web
		# badge, but go ahead and attribute all to WebExtension.
		# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/31157#note_2593925
		is.na(type) & date <= "2020-12-03" ~ "webext",
		# After that, attribute unknown proxy types until 2022-06-21 to
		# iptproxy. IPtProxy reported its type as "iptproxy", but this
		# value was not recognized by the broker until the deployment
		# of 2022-06-21.
		# https://bugs.torproject.org/tpo/anti-censorship/pluggable-transports/snowflake/40151
		is.na(type) & date <= "2022-06-21" ~ "iptproxy",
		TRUE ~ type
	))

# After attributing proxies without a known type to "webext" or "iptproxy" for
# certain date ranges above, there still remain a very small number of unknown
# types after 2022-06-21. To avoid having just a few specks of data on the
# graph, we filter the cases out and note that we have done so in the figure
# caption. But do a sanity check here to ensure we do not accidentally remove a
# lot of data.
(function() {
	num_unknown <- (
		proxy_type %>%
		group_by(type) %>%
		summarize(unique_ips = sum(unique_ips, na.rm = TRUE)) %>%
		filter(is.na(type))
	)$unique_ips
	if (num_unknown > 50) {
		stop(sprintf("unexpectedly high unknown count: %d", num_unknown))
	}
})()


proxy_type <- proxy_type %>%
	# Remove unknown types.
	filter(!is.na(type)) %>%

	# Unlike in the user graphs, here we do not compensate for days where
	# coverage < 1.0: because of deduplication, unique_ips does not scale
	# linearly with time like the number of concurrent users does.

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
		"Command-line" = "standalone",
		"Web badge" = "badge"
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
			mutate(y = place_no_overlap(unique_ips / coverage, -2000, 10000)),
		aes(
			x = date,
			y = y,
			color = type,
			# \u00ad (soft hyphen) is a hack to avoid the hypohen
			# turning into a minus sign: https://stackoverflow.com/a/48510383.
			label = if (hyphen_hack) gsub("-", "\u00ad", type) else type
		),
		position = position_nudge(x = 5),
		size = 2.5,
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
	scale_color_manual(values = c(
		"midnightblue",
		"firebrick",
		"orangered",
		"slateblue"
	)) +
	coord_cartesian(xlim = DATE_LIMITS, ylim = c(0, NA), expand = FALSE, clip = "off") +
	labs(x = NULL, y = "Unique IP addresses") +

	# Make room for the margin labels added by geom_text above.
	theme(plot.margin = unit(c(0.5, 15.5, 0, 0.5), "mm")) +
	guides(color = "none")
ggsave(output_path, p, width = DOCUMENT_LINEWIDTH, height = 1.25, dpi = 300)
