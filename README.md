This repository contains the manuscript and code for paper "A Careful Consideration of CLARIFY: Simulation-Induced Bias in Point Estimates of Quantities of Interest."

> Some work in political methodology recommends that applied researchers obtain point estimates of quantities of interest by simulating model coefficients, transforming these simulated coefficients into simulated quantities of interest, and then averaging the simulated quantities of interest (e.g., CLARIFY). But other work advises applied researchers to directly transform coefficient estimates to estimate quantities of interest. I point out that these two approaches are not interchangeable and examine their properties. I show that the simulation approach compounds the transformation-induced bias identified by Rainey (2017), adding bias with direction and magnitude similar to the transformation-induced bias. I refer to this easily-avoided additional bias as "simulation-induced bias." Even if researchers use simulation to estimate standard errors, they should directly transform maximum likelihood estimates of coefficient estimates to obtain point estimates of quantities of interest.

## Reproduction

To reproduce this work, simply run `make` in a terminal. This reproduces all computations and compiles the manuscript and appendix. It takes about three minutes to run on my desktop.

### Making Portions of the Project 

- `make` or `make all` makes the entire project (simulations, data analysis, manuscript, and appendix).
- `make paper` makes the manuscript and appendix.

### Cleaning the Directory

- `make cleanALL` deletes all files created by scripts. 
- `make cleanpaper` deletes the `.pdf` versions of the manuscript and appendix. 

To reproduce certain portions, see the R scripts below.

- `R/holland.R`: reproduces the re-analysis of Holland (2015).
- `R/intuition-sims.R`: reproduces the simulations for the "drastic, convex transformation."

### Dependencies

The figure below shows the dependencies.

![](makefile-dag.png)
