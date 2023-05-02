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

%.pdf: %.tex
	rm -f "$*.aux" "$*.bbl"
	$(PDFLATEX) -draftmode "$*"
	$(BIBTEX) "$*"
	$(PDFLATEX) -draftmode "$*"
	$(PDFLATEX) "$*"

.PHONY: clean
clean:
	rm -rf $(addprefix snowflake,.aux .ent .log .pdf .bbl .blg .out)

.DELETE_ON_ERROR:
