library("tidyverse")
library("cowplot")

# Return an abbreviation for the month, followed by a year for January only.
date_labels <- function(breaks) {
	strftime(breaks, ifelse(!is.na(breaks) & lubridate::month(breaks) == 1, "%b\n%Y", "%b"), tz = "UTC")
}

dns <- read_csv("dns_tm_cdn-sstatic-net.csv",
	col_types = cols_only(
		domain = col_character(),
		start_time = col_datetime(format = "%Y-%m-%d %H:%M:%OS UTC"),
		end_time = col_datetime(format = "%Y-%m-%d %H:%M:%OS UTC"),
		success = col_logical(),
		anomaly = col_logical()
	)
)

http <- read_csv(
	"http_tm_cdn-sstatic-net.csv",
	col_types = cols_only(
		domain = col_character(),
		start_time = col_datetime(format = "%Y-%m-%d %H:%M:%OS UTC"),
		end_time = col_datetime(format = "%Y-%m-%d %H:%M:%OS UTC"),
		outcome = col_factor()
	)
)

https <- read_csv(
	"https_tm_cdn-sstatic-net.csv",
	col_types = cols_only(
		domain = col_character(),
		start_time = col_datetime(format = "%Y-%m-%d %H:%M:%OS UTC"),
		end_time = col_datetime(format = "%Y-%m-%d %H:%M:%OS UTC"),
		outcome = col_factor()
	)
)

WANTED_FINGERPRINTS <- c(
	"7659DA0F96B156C322FBFF3ACCC9B9DC01C27C73" = "snowman",
	"5481936581E23D2D178105D44DB6915AB06BFB7F" = "snowflake-01",
	"91DA221A149007D0FD9E5515F5786C3DD07E4BB0" = "snowflake-02"
)

bridge_combined_multi <- read_csv("../users/userstats-bridge-combined-multi.csv") %>%
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

common <- list(
	scale_x_datetime(
		limits = c(
			min(dns$start_time, http$start_time, https$start_time),
			max(dns$start_time, http$start_time, https$start_time)
		),
		date_breaks = "1 month",
		minor_breaks = NULL,
		labels = date_labels
	),
	scale_color_manual(values = c(
		"expected/match" = "royalblue",
		"no anomaly" = "royalblue",
		"read/tcp.reset" = "coral",
		"anomaly" = "coral",
		"read/timeout" = "gray"
	)),
	guides(color = "none"),
	labs(x = NULL, y = NULL),
	theme_light()
)

dns_plot <- ggplot(dns %>%
	filter(success) %>%
	mutate(outcome = factor(if_else(anomaly, "anomaly", "no anomaly")))
) +
	geom_point(aes(start_time, outcome, color = outcome), alpha = 0.5) +
	scale_y_discrete(limits = c("no anomaly", "anomaly")) +
	labs(title = "Censored Planet DNS tests of cdn.sstatic.net in Turkmenistan") +
	common

http_plot <- ggplot(http) +
	geom_point(aes(start_time, outcome, color = outcome), alpha = 0.5) +
	scale_y_discrete(limits = c("expected/match", "read/tcp.reset", "read/timeout")) +
	labs(title = "Censored Planet HTTP tests of cdn.sstatic.net in Turkmenistan") +
	common

https_plot <- ggplot(https) +
	geom_point(aes(start_time, outcome, color = outcome), alpha = 0.5) +
	scale_y_discrete(limits = c("expected/match", "read/tcp.reset", "read/timeout")) +
	labs(title = "Censored Planet HTTPS tests of cdn.sstatic.net in Turkmenistan") +
	common

users_plot <- ggplot(bridge_combined %>%
	filter(country == "tm") %>%
	mutate(date = lubridate::as_datetime(date))
) +
	geom_line(aes(date, users)) +
	labs(title = "Number of Snowflake users in Turkmenistan") +
	common

p <- plot_grid(
	plotlist = list(dns_plot, http_plot, https_plot, users_plot),
	ncol = 1,
	rel_heights = c(3, 4, 4, 4),
	align = "v",
	axis = "lr"
)

ggsave("censoredplanet-snowflake.png", p, width = 8, height = 6, dpi = 200)
