---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'fitPlotR: An R package for Plotting probability distributions'
tags:
  - R 
  - Probability distribution 
authors:
  - name: Muhammad Osama
    orcid: 0009-0004-5952-8958
affiliations:
 - name: Department of Statistics, Quaid-i-Azam University, Islamabad 44000, Pakistan
   index: 1
date: 9 Feb 2026
year: 2026
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---


# Summary

The fitPlotR package provides various set of functions for visualizing univariate probability distributions. The package contains an integrated set of functions to visualize probability density functions, cumulative distribution functions, survival functions, hazard functions. The package includes various functions to describe the basic characteristics of a probability distribution, such as mean, median, mode, dispersion, and various higher-order moments of a probability distribution, such as skewness and kurtosis. In addition to diagnostic plots, the package includes functions for plotting fitted probability density functions, fitted cumulative distribution functions, quantile-quantile (QQ) plot and probability-probability (PP) plot, along with comprehensive graphical data overview. 
 
# Statement of Need 

Probability distributions are the spine of statistical analysis. These distributions help researchers to describe mathematical functions that can represent uncertainty or variability in reliability engineering, survival analysis, and various other fields. These functions can be well explained using graphical illustrations, where key theoretical functions, like probability density functions (PDFs), cumulative functions (CDFs), survival functions (SFs), and hazard functions (HFs) can be compared effectively. Visualization of these functions allows researchers to interpret the behavior of different distributions.

While R provides extensive tools for statistical computation and graphics, many existing packages primarily emphasize fitting probability distributions to empirical data or serve as general-purpose visualization frameworks (Wickham et al., 2025; Delignette-Muller & Dutang, 2015). Although these tools are powerful, they often require multiple manual steps to visualize theoretical distribution functions, particularly when working with user-defined or newly proposed models (Kay & Wiernik, 2025; O’Hara-Wild et al., 2026). As a result, standardized and reproducible visualization of theoretical probability distributions can be unnecessarily complex.

To address this gap, we present fitPlotR an R package for visualizing theoretical probability distributions in an easy standardized way. fitPlotR allows you to create visually appealing and fully customizable representations of important functions associated with the probability distribution (i.e. probability density, cumulative, survival, and hazard functions). By allowing the user to specify the model parameters directly, fitPlotR will create customizable plots for several types of continuous probability distributions. By providing all tools in one easy-to-use package that does not require a user to have empirical data or fit distribution parameters through complex programming, students, educators and research practitioners can focus on understanding the mathematical nature of distributions, create customized visualizations and easily reproduce them through the use of this package.

# Availability 

The fitPlotR package is open-source and freely available on CRAN https://CRAN.R-project.org/package=fitPlotR. It is distributed under the 	GPL-3 License.

# References

Wickham H et.al (2025). ggplot2 Create Elegant Data Visualizations Using the Grammar of Graphics. R package version	4.0.1 **URL** https://CRAN.R-project.org/package=ggplot2 



Delignette-Muller M-L et.al (2015). fitdistrplus Help to Fit of a Parametric Distribution to Non-Censored or Censored Data. R package version	1.2-6 URL https://CRAN.R-project.org/package=fitdistrplus


Kay M, Wiernik B M (2025). ggdist Visualizations of Distributions and Uncertainty. R package version 3.3.3 URL https://CRAN.R-project.org/package=ggdist  


O'Hara-Wild M et.al (2026). distributional Vectorised Probability Distributions. R package version 0.6.0 URL https://CRAN.R-project.org/package=distributional





