# Without this, things like month names in axis labels are locale-dependent.
Sys.setlocale("LC_ALL", "C")

# \the\textwidth → 469.0pt
DOCUMENT_TEXTWIDTH <- 469.0 / 72.27

# \the\textwidth → 229.5pt
DOCUMENT_LINEWIDTH <- 229.5 / 72.27

FONT_FAMILY <- "Times"

COMMON_THEME <- theme_minimal() +
	theme(text = element_text(size = 9, family = FONT_FAMILY)) +
	theme(plot.margin = margin(0, 0, 0, 0, "mm"))
