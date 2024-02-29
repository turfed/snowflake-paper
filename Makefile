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
	gs -q -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="$@" -dEmbedAllFonts=true "$<"

.PHONY: bundle snowflake-paper.bundle snowflake-paper.zip
bundle: snowflake-paper.bundle snowflake-paper.zip snowflake.pdf

snowflake-paper.bundle:
	git bundle create "$@" main usenix2024 sec24fall-submission

snowflake-paper.zip:
	git archive --format=zip --output="$@" --prefix=snowflake-paper/ main

ORIG = a653e7c0285529c8f2502ab2cb74e73660cf4dc9
PREV = 089128f6b7c89c94ad66d0a202da7cea6b8643cd
STAMP = $(shell git log -1 --pretty=format:%ad --date=format:%Y%m%d HEAD).$(shell git rev-parse --short=8 HEAD)
.PHONY: diff
diff: sec24fall-paper1998.$(STAMP).pdf \
	sec24fall-paper1998.$(STAMP).diff.pdf \
	sec24fall-paper1998.$(STAMP).cumul.pdf
.INTERMEDIATE: sec24fall-paper1998.orig.tex sec24fall-paper1998.prev.tex
sec24fall-paper1998.orig.tex:
	git show $(ORIG):snowflake.tex > "$@"
sec24fall-paper1998.prev.tex:
	git show $(PREV):snowflake.tex > "$@"
sec24fall-paper1998.$(STAMP).tex:
	git show HEAD:snowflake.tex > "$@"
sec24fall-paper1998.$(STAMP).diff.tex: sec24fall-paper1998.prev.tex sec24fall-paper1998.$(STAMP).tex
	latexdiff sec24fall-paper1998.prev.tex sec24fall-paper1998.$(STAMP).tex > "$@"
sec24fall-paper1998.$(STAMP).cumul.tex: sec24fall-paper1998.orig.tex sec24fall-paper1998.$(STAMP).tex
	latexdiff sec24fall-paper1998.orig.tex sec24fall-paper1998.$(STAMP).tex > "$@"

.PHONY: clean
clean:
	rm -f $(addprefix snowflake,.aux .ent .log .pdf .bbl .blg .out) \
		snowflake-paper.bundle snowflake-paper.zip

.DELETE_ON_ERROR:
