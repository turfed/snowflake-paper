INKSCAPE = inkscape

rendezvous.pdf:

%.pdf: %.inkscape.svg
	$(INKSCAPE) --export-overwrite --export-type=pdf --export-filename="$@" "$<"
