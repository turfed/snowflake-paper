INKSCAPE = inkscape

rendezvous.pdf:

%.pdf: %.inkscape.svg
	$(INKSCAPE) --export-overwrite --export-type=pdf --export-filename="$@" "$<"

%.svg: %.inkscape.svg
	$(INKSCAPE) --export-overwrite --export-type=svg --export-filename="$@" "$<"

%.tmp.png: %.inkscape.svg
	$(INKSCAPE) --export-overwrite --export-type=png --export-filename="$@" --export-dpi 300 "$<"

%.png: %.tmp.png
	zopflipng -y -m "$<" "$@"

.DELETE_ON_ERROR:
