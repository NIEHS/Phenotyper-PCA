# Phenotyper PCA

Code companion to Analyzing and Interpreting Phenotyper Behavioral Data with Principal Components Analysis



Kathryn S. Konrad (1)

Leslie Aksu (2)

Dalisa Kendricks (2,3)

Keith R. Shockley (4)

Helen Cunny (3)

Jesse Cushman (2)



(1) DLH, LLC, 6720B Rockledge Drive, Suite 777, Bethesda, MD 20817, USA

(2) Neurobehavioral Core Laboratory, National Institute of Environmental Health Sciences, 111 T.W. Alexander Drive, Durham, NC 27709, USA

(3) Division of Translational Toxicology, National Institute of Environmental Health Sciences, 111 T.W. Alexander Drive, Durham, NC 27709, USA

(4) Division of Intramural Research, National Institute of Environmental Health Sciences, 111 T.W. Alexander Drive, Durham, NC 27709, USA



\# Where to start

Start with the analysis-input-variables.xlsx sheet. This will help you set up your analysis.

Then open the RProject and the Report.Rmd.

The Report.Rmd file calls the other .R files in the repository, described below.

Knit the Report, and it will produce an .html file and an Excel spreadsheet with your results.



\# preparation.R

This loads functions and the analysis-input-variables.xlsx sheet.



\# preparation data.R

This creates all sorts of color codes for the plots, runs ANOVAs and pairwise post-hoc tests on the original Phenotyper variables, and does most of the before-PCA work.



\# correlation.R

This file creates the standard and clustered heatmaps, filters out variables that are missing or that are too highly correlated with others, and prepares a dataset for PCA.



\# principal components analysis.R

This file runs the PCA and collects the outputs from it, namely the Scores, Loadings, and the Percent of Variability explained by each component.



\# functions.R

This file contains code used throughout the repository. The ANOVA, pairwise testing, plot generating code are all contained in this file, as well as some simple helper functions.





