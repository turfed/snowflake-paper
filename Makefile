PDFLATEX = max_print_line=10000 pdflatex -halt-on-error -file-line-error -interaction=nonstopmode
BIBTEX = bibtex

FIGURES = \
	figures/rendezvous/rendezvous.pdf \
	figures/users/users-global.pdf \
	figures/users/users-ru.pdf \
	figures/proxies/proxy-type.pdf \
	figures/proxies/proxy-nat-type.pdf \
	figures/proxy-churn/proxy-count-decay.pdf

snowflake.pdf: *.tex *.bib $(FIGURES)

# Compile the TeX files to produce a PDF that does not have embedded fonts.
%.tmp.pdf: %.tex
	rm -f "$*.aux" "$*.bbl"
	$(PDFLATEX) -draftmode "$*"
	$(BIBTEX) "$*"
	$(PDFLATEX) -draftmode "$*"
	$(PDFLATEX) "$*"
	mv -v "$*.pdf" "$@"

# Embed fonts (PDF graphics produced by R do not have embedded fonts by default)
# https://www.usenix.org/legacy/events/samples/latex_tips.html
%.pdf: %.tmp.pdf
	gs -q -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="$@" -dEmbedAllFonts=true "$<"

.PHONY: clean
clean:
	rm -rf $(addprefix snowflake,.aux .ent .log .pdf .bbl .blg .out)

.DELETE_ON_ERROR:
