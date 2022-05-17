# Without this, things like month names in axis labels are locale-dependent.
Sys.setlocale("LC_ALL", "C")

# \the\textwidth → 505.89pt
DOCUMENT_TEXTWIDTH <- 505.89 / 72.27

# \the\textwidth → 247.94499pt
DOCUMENT_LINEWIDTH <- 247.94499 / 72.27

FONT_FAMILY <- "Times"

COMMON_THEME <- theme_minimal() +
	theme(text = element_text(size = 9, family = FONT_FAMILY)) +
	theme(plot.margin = margin(0, 0, 0, 0, "mm"))
