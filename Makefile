# phony
all: dag paper
paper: doc/unnecessary.pdf
dag: makefile-dag.png

# draw makefile dag
makefile-dag.png: makefile-dag.R Makefile
	Rscript $<

# do and plot poisson simulations
doc/figs/poisson-mcs.pdf: R/poisson-mcs.R
	Rscript $<

# do intuition and plot simulations
doc/figs/intuition.pdf doc/figs/intuition-sampling.pdf: R/intuition-sims.R
	Rscript $<
	
# do logit simulations
data/nagler-fd-bias.rds: R/nagler-fd-sims.R data/scobit.dta
	Rscript $<
	
# plot logit simulations
doc/figs/nagler-fd-bias.pdf: R/plot-nagler-fd-bias.R data/nagler-fd-bias.rds
	Rscript $<	
	
# do holland example
doc/figs/holland.pdf doc/tabs/top-5.tex: R/holland.R data/Enforcement.dta
	Rscript $<
	
# do and plot rmse simulations
doc/figs/rainey-2017-density.pdf doc/figs/rainey-2017-summary.pdf: R/rainey-2017-example.R
	Rscript $<

# do logit mse example	
doc/figs/logit-mse-me.pdf: R/logit-mse-me.R
	Rscript $<

# compile manuscript	
doc/unnecessary.pdf: doc/unnecessary.tex doc/bibliography.bib doc/figs/intuition.pdf doc/figs/intuition-sampling.pdf doc/figs/poisson-mcs.pdf doc/figs/nagler-fd-bias.pdf doc/figs/rainey-2017-density.pdf doc/figs/rainey-2017-summary.pdf doc/figs/logit-mse-me.pdf doc/figs/holland.pdf doc/tabs/top-5.tex
  # cd into doc so that pdflatex runs in the doc directory
	cd doc; pdflatex unnecessary
	cd doc; bibtex unnecessary
	cd doc; pdflatex unnecessary 
	cd doc; pdflatex unnecessary 
	cd doc; rm -f *.bbl *.log *.synctex.gz *.aux *.out *blg

# cleaning phonys
cleanpaper: 	
	rm -f doc/unnecessary.pdf
	
cleandag: 	
	rm -f makefile-dag.png

cleanALL: cleanpaper cleandag
	rm -f doc/figs/poisson-mcs.pdf
	rm -f doc/figs/intuition.pdf doc/figs/intuition-sampling.pdf
	rm -f doc/figs/nagler-fd-bias.pdf data/nagler-fd-bias.rds data/nagler-fd-bias.csv
	rm -f doc/figs/holland.pdf doc/tabs/top-5.tex
	rm -f doc/figs/rainey-2017-density.pdf doc/figs/rainey-2017-summary.pdf
	rm -f doc/figs/logit-mse-me.pdf

	