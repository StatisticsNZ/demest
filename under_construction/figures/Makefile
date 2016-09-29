
all: dag_hierarchical_model.png dag_estimate_model.png dag_estimate_counts.png

dag_hierarchical_model.png : dag_hierarchical_model.pdf
	convert -background white -alpha remove -trim -density 300 $< $@

dag_estimate_model.png : dag_estimate_model.pdf
	convert -background white -alpha remove -trim -density 300 $< $@

dag_estimate_counts.png : dag_estimate_counts.pdf
	convert -background white -alpha remove -trim -density 300 $< $@

dag_hierarchical_model.pdf : dag_hierarchical_model.tex
	pdflatex $<

dag_estimate_model.pdf : dag_estimate_model.tex
	pdflatex $<

dag_estimate_counts.pdf : dag_estimate_counts.tex
	pdflatex $<

.PHONY: clean
clean:
	rm -f *.aux *.bbl *.blg *.log *.bak *.aux *.synctex.gz *.pdf

.PHONY: cleanall
cleanall:
	rm -f *.aux *.bbl *.blg *.log *.bak *.aux *.synctex.gz *.pdf *.pdf



