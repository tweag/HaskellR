all:
	lhs2TeX --poly paper.lhs > paper.tex
	pdflatex --interaction=nonstopmode paper.tex
	bibtex paper
	pdflatex paper.tex

clean:
	rm paper.tex *.aux *.log *.ptb
