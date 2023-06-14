# Makes a graph showing the number of unique Snowflake proxy IP addresses per
# day, by NAT type.
#
# Usage:
#   Rscript proxy-nat-type.r proxy-nat-type.csv proxy-nat-type.pdf

library("tidyverse")

source("../common.r")

DATE_LIMITS <- lubridate::ymd(c(
	# "2019-07-01",
	"2021-01-01",
	"2023-05-30"
))

# TODO: date threshold where "untested" changed to "restricted" and "unknown" in August 2020.

(function() {
	args <- commandArgs(trailingOnly = TRUE)
	if (length(args) != 2) {
		stop("usage: Rscript proxy-nat-type.r proxy-nat-type.csv proxy-nat-type.pdf")
	}
	proxy_nat_type_csv_path <<- args[[1]]
	output_path <<- args[[2]]
})()

proxy_nat_type <- read_csv(proxy_nat_type_csv_path) %>%
	# Unlike in the user graphs, here we do not compensate for days where
	# coverage < 1.0: because of deduplication, unique_ips does not scale
	# linearly with time like the number of concurrent users does.

	# Put a label on the rows with type == NA.
	replace_na(list(nat_type = "untested")) %>%

	# Fill in entirely missing dates with NA.
	group_by(nat_type) %>%
	complete(date = seq.Date(min(date), max(date), "days")) %>%
	ungroup() %>%

	# Keep only the records within DATE_LIMITS.
	filter(lubridate::`%within%`(date, do.call(lubridate::interval, as.list(DATE_LIMITS)))) %>%

	# Make better labels for proxy types.
	mutate(nat_type = fct_recode(nat_type,
		"Restricted" = "restricted",
		"Unrestricted" = "unrestricted",
		"Unknown" = "unknown",
		"Untested" = "untested"
	)) %>%

	# Order the types by the value of unique_ips at the right side of the graph.
	mutate(nat_type = fct_reorder2(nat_type, date, unique_ips))

# To add a "total" series:
# proxy_type <- bind_rows(
# 	proxy_type,
# 	proxy_type %>%
# 		group_by(date) %>%
# 		summarize(
# 			unique_ips = sum(unique_ips, na.rm = TRUE),
# 			coverage = mean(coverage, na.rm = TRUE)
# 		) %>%
# 		mutate(nat_type = "total")
# )

p <- ggplot() +
	# TODO: event annotations?

	# Data series.
	geom_line(
		data = proxy_nat_type, aes(
			x = date,
			# Divide by coverage (what fraction of of a day each
			# unique_ips sample represents, between 0.0 and 1.0) to
			# remove illusory dips caused by partial days' data.
			y = unique_ips / coverage,
			color = nat_type
		)
	) +

	# Draw the type labels in the right margin. theme(plot.margin) below
	# makes room for this.
	geom_text(
		data = proxy_nat_type %>%
			group_by(nat_type) %>%
				arrange(desc(date)) %>%
				slice(1) %>%
			ungroup() %>%
			mutate(date = max(date)) %>%
			mutate(y = place_no_overlap(unique_ips, -10000, 4000)),
		aes(
			x = date,
			y = y,
			color = nat_type,
			label = nat_type,
		),
		position = position_nudge(x = 5),
		size = 2,
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
	coord_cartesian(xlim = DATE_LIMITS, expand = FALSE, clip = "off") +
	labs(x = NULL, y = "Unique proxy IP addresses") +

	# Make room for the margin labels added by geom_text above.
	theme(plot.margin = unit(c(0, 11, 0, 0), "mm")) +
	guides(color = "none")
ggsave(output_path, p, width = DOCUMENT_LINEWIDTH, height = 1.5)
