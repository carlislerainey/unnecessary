# making phonys
all: dag paper
paper: doc/unnecessary.pdf doc/unnecessary-appendix.pdf
dag: makefile-dag.png

# draw makefile dag
makefile-dag.png: makefile-dag.R Makefile
	Rscript $<

# do intuition and plot simulations
doc/figs/fig1-intuition.pdf doc/figs/fig2-intuition-sampling.pdf: R/intuition-sims.R
	Rscript $<
	rm Rplots.pdf
	
# do holland example
doc/figs/fig3-holland.pdf doc/tabs/tab1-top-5.tex doc/tabs/holland-medians.csv: R/holland.R data/Enforcement.dta
	Rscript $<
	rm Rplots.pdf

# compile manuscript	
doc/unnecessary.pdf: doc/unnecessary.tex doc/bibliography.bib doc/figs/fig1-intuition.pdf doc/figs/fig2-intuition-sampling.pdf doc/figs/fig3-holland.pdf doc/tabs/tab1-top-5.tex
  # cd into doc so that pdflatex runs in the doc directory
	cd doc; pdflatex unnecessary
	cd doc; bibtex unnecessary
	cd doc; pdflatex unnecessary 
	cd doc; pdflatex unnecessary 
	cd doc; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg
	
# compile appendix	
doc/unnecessary-appendix.pdf: doc/unnecessary-appendix.tex doc/bibliography.bib
  # cd into doc so that pdflatex runs in the doc directory
	cd doc; pdflatex unnecessary-appendix
	cd doc; bibtex unnecessary-appendix
	cd doc; pdflatex unnecessary-appendix
	cd doc; pdflatex unnecessary-appendix 
	cd doc; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg	

# cleaning phonys
cleanpaper: 	
	rm -f doc/unnecessary.pdf
	rm -f doc/unnecessary-appendix.pdf
	
cleandag: 	
	rm -f makefile-dag.png

cleanALL: cleanpaper cleandag
	rm -f doc/figs/fig1-intuition.pdf doc/figs/fig2-intuition-sampling.pdf
	rm -f doc/figs/fig3-holland.pdf doc/tabs/tab1-top-5.tex doc/tabs/holland-medians.csv

	