library("tidyverse")

# Without this, things like month names in axis labels are locale-dependent.
Sys.setlocale("LC_ALL", "C")

# \the\textwidth → 505.89pt
DOCUMENT_TEXTWIDTH <- 505.89 / 72.27

# \the\textwidth → 247.94499pt
DOCUMENT_LINEWIDTH <- 247.94499 / 72.27

FONT_FAMILY <- "Times"
theme_set(
	theme_minimal(
		base_family = FONT_FAMILY,
		base_size = 9
	) +
	theme(plot.margin = margin(0, 0, 0, 0, "mm"))
)
# https://stackoverflow.com/a/48978417
update_geom_defaults("text", aes(family = FONT_FAMILY))
update_geom_defaults("label", aes(family = FONT_FAMILY))
update_geom_defaults("line", aes(size = 0.2))

# Returns a vector z that minimizes sum((z - y)^2), subject to the conditions
# that all elements of z are at least M, and the difference between any two
# elements of z is at least S.
place_no_overlap <- function(y, M, S) {
	# We deal with the input in reverse-sorted order, then undo the
	# permutation in the result vector when finished.
	o <- order(y, decreasing=TRUE)
	y <- y[o]
	# Our goal is to divide y into contiguous, noninteracting clumps,
	# adjacent elements of which are separated by exactly S. Then, we place
	# each clump to minimize its individual cost.
	#
	# We look at clumps of every possible size starting at the top of y,
	# and break off the clump whose highest point is the greatest, when the
	# clump is placed optimally.
	z <- c()
	while (length(y) > 0) {
		# The cost of placing a clump y[1:i], with the highest element
		# of the clump placed at point zp, is
		#   sum((zp - (0:(i-1))*S - y[1:i])^2).
		# (The highest element is placed as zp, the next highest at
		# zp-1*S, then zp-2*S, etc.) The derivative of this function is
		#   2*sum(zp - (0:(i-1))*S - y[1:i]) = 2*(i*zp - sum((0:(i-1))*S + y[i:1])),
		# which is equal to zero when
		#   zp = sum((0:(i-1))*S + y[i:1]) / i.
		zp <- cumsum((0:(length(y)-1))*S + y) / (1:length(y))
		# We must additionally enforce the M condition, which means
		# that the highest element of a clump of size i cannot be
		# placed lower than M + (i-1)*S.
		zp <- pmax(zp, M + (0:(length(y)-1))*S)
		# Find the clump which would be placed the highest.
		i <- which.max(zp)
		# Break off the top i elements as a clump and place them at zp,
		# zp-1*S, zp-2*S, etc.
		z <- c(z, zp[[i]] - (0:(i-1))*S)
		if (i < length(y))
			y <- y[(i+1):length(y)]
		else
			y <- c()
	}
	z[order(o)]
}
