PDFLATEX = max_print_line=10000 pdflatex -halt-on-error -file-line-error -interaction=nonstopmode
BIBTEX = bibtex

export TZ = UTC

FIGURES = \
	figures/architecture/architecture.jpg \
	figures/rendezvous/rendezvous.pdf \
	figures/users/users-global.pdf \
	figures/users/users-ru.pdf \
	figures/proxies/proxy-type.pdf \
	figures/proxies/proxy-nat-type.pdf \
	figures/proxy-churn/proxy-count-decay.pdf

snowflake.pdf: snowflake.tex snowflake.bib snowflake.bst usenix-2020-09.sty $(FIGURES)

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
# TODO: gs adds Producer, CreationDate, and ModDate metadata, undoing the reproducible PDF settings in snowflake.tex.
%.pdf: %.tmp.pdf
	gs -q -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="$@" -dCompatibilityLevel=1.5 -dEmbedAllFonts=true -c '<</NeverEmbed []>> setdistillerparams' -f "$<"

.PHONY: bundle snowflake-paper.bundle snowflake-paper.zip
bundle: snowflake-paper.bundle snowflake-paper.zip snowflake.pdf

snowflake-paper.bundle:
	git bundle create "$@" main usenix2024 sec24fall-submission

snowflake-paper.zip:
	git archive --format=zip --output="$@" --prefix=snowflake-paper/ main

.PHONY: clean
clean:
	rm -f $(addprefix snowflake,.aux .ent .log .pdf .bbl .blg .out) \
		snowflake-paper.bundle snowflake-paper.zip

.DELETE_ON_ERROR:
