# phony
all: paper dag
paper: doc/unnecessary.pdf
dag: makefile-dag.png

# draw makefile dag
makefile-dag.png: makefile-dag.R Makefile
	Rscript $<

# do and plot poisson simulations
doc/figs/poisson-mcs.pdf: R/poisson-mcs.R
	Rscript $<

# do intuition and plot simulations
doc/figs/intuition-1.pdf doc/figs/intuition-2.pdf doc/figs/intuition-3.pdf doc/figs/intuition-4.pdf doc/figs/intuition-sampling.pdf: R/intuition-sims.R
	Rscript $<
	
# do logit simulations
data/nagler-fd-bias.rds: R/nagler-fd-sims.R data/scobit.dta
	Rscript $<
	
# plot logit simulations
doc/figs/nagler-fd-bias.pdf: R/plot-nagler-fd-bias.R data/nagler-fd-bias.rds
	Rscript $<	
	
# do ge example
doc/figs/ge-pr.pdf doc/figs/ge-fd.pdf: R/ge.R data/ge.csv
	Rscript R/ge.R

# compile manuscript	
doc/unnecessary.pdf: doc/unnecessary.tex doc/bibliography.bib doc/figs/intuition-1.pdf doc/figs/intuition-2.pdf doc/figs/intuition-3.pdf doc/figs/intuition-4.pdf doc/figs/intuition-sampling.pdf doc/figs/poisson-mcs.pdf doc/figs/nagler-fd-bias.pdf doc/figs/ge-pr.pdf doc/figs/ge-fd.pdf
  # cd into doc so that pdflatex runs in the doc directory
	cd doc; pdflatex unnecessary
	cd doc; bibtex unnecessary
	cd doc; pdflatex unnecessary 
	cd doc; pdflatex unnecessary 
	cd doc; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg
cleanpaper: 	
	rm -f doc/unnecessary.pdf
	
cleandag: 	
	rm -f makefile-dag.png

cleanALL: cleanpaper
	rm -f doc/figs/poisson-mcs.pdf
	rm -f doc/figs/intuition-*.pdf
	rm -f doc/figs/nagler-fd-bias.pdf data/nagler-fd-bias.rds data/nagler-fd-bias.csv
	rm -f doc/figs/ge-pr.pdf doc/figs/ge-fd.pdf