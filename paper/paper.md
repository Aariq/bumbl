---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'bumbl: An R package for modeling growth-switchpoint-decline in bumblebee colony size'
tags:
  - R
  - bumblebees
  - Bombus vosnesenskii
  - population dynamics
  - switchpoint
  - generalized linear models
authors:
  - name: Eric R. Scott
    orcid: 0000-0002-7430-7879
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Sylvana Finn
    # orcid: 0000-0000-0000-0000
    affiliation: 2
  - name: Elizabeth E. Crone
    orcid: 0000-0002-5287-221X
    affiliation: 2
affiliations:
 - name: Department of Wildlife Ecology and Conservation, University of Florida
   index: 1
 - name: Department of Biology, Tufts University
   index: 2
citation_author: Scott et. al.
date: 15 April 2021
year: 2021
bibliography: paper.bib
output: 
  bookdown::pdf_document2:
    keep_md: yes
    fig_caption: yes
  bookdown::word_document2:
    number_sections: no
    linenumbers: yes
    fig_caption: yes
    keep_md: yes
  rticles::joss_article: default
csl: apa.csl
journal: JOSS
---

# Statement of Need

Bumble bees (*Bombus* spp*.*) are important pollinators of wild plants and agricultural systems, and many species of bumble bees are in decline [@goulson2008]. Bumble bees are social bees which produce annual colonies. In early spring, queens emerge and establish nests. The colony then begins to increase exponentially with workers until at some point, the colony switches from growth to reproduction, when males and new queens are produced [@goulson2010bumblebees]. After this, the colony size declines as workers die and new queens and males disperse. This switch point is critical for the reproduction and viability of the colony and is therefore important for ecologists to examine. Standard generalized linear models (GLMs) are commonly used to estimate and forecast population-level growth, but these methods are not suited to analyze colony-level growth and decline because they cannot capture the sharp switchpoint from growth to reproduction. [@croneBumbleBeeColony2016] developed a method to estimate the switch from growth to reproduction by fitting a modified GLM with a switchpoint included.
Their method required repeated creation of dummy variables in the data for many values of the switchpoint in order to choose the value of the switchpoint that maximized the likelihood of the modified GLM.
This package automates the model fitting process and uses optimization for a more precise estimate of the switchpoint.
`bumbl` aims to make this method broadly accessible to bumble bee biologists, and others who are interested in similar growth-switchpoint-decline phenomena.

# Background

@croneBumbleBeeColony2016 describe a mathematical model for bumblebee colony growth, switch to reproduction, and decline.

$$
W_t = 
  \begin{cases}
  \lambda^tW_0 & t\leq\tau \\
  \lambda^{\tau}W_{0}\delta^{t-\tau} & t > \tau
  \end{cases}
$$

It defines the expected colony weight (or other proxies for size such as counts of workers) over time as having an initial size ($W_0$), a growth rate ($\lambda$), a switch from growth to decline at time $t = \tau$, and a rate of decline ($\delta$) after the switch to reproduction.
This can be then be log-linearized and fit as a single model by creating a variable $x$ that is 0 when $t \leq \tau$ and equal to $t - \tau$ when $t > \tau$

$$
\ln(W_t) = \ln(W_0) + \tau\ln(\lambda) + x\ln(\delta)
$$

The `bumbl()` function finds a value for $\tau$ that maximizes the likelihood of this model, then returns the maximum likelihood estimate for $\tau$ (`tau`), the estimated initial colony weight on a log scale ($\ln({W_0})$, `logN0`), the average colony growth rate on a log scale ($\ln(\lambda)$, `logLam`), the rate of decline after $\tau$ ($ln(\delta - \lambda)$, `decay`), and the estimated maximum weight of each colony, on a log scale (`logNmax`).

The object created by `bumbl()` has methods for both `plot()` and the `autoplot()` function from the `ggplot2` package [@wickhamGgplot2ElegantGraphics2016] to plot the fitted models.

# Example



The package includes a built-in dataset, `bombus`, that contains weekly weights of *Bombus vosnesenskii* colonies originally published in @croneBumbleBeeColony2016.
Here is an example analysis using just four colonies from this dataset.
The output from the `bumbl()` function is a `tibble` with one row for each colony and columns for parameters calculated from the models (Table \@ref(tab:table)).



\begin{table}

\caption{(\#tab:table)Data frame output by the `bumbl()` function showing estimates of switchpoint (`tau`, in weeks), estimated initial size (`logN0`), growth rate (`logLam`), decay rate (`decay`), and maximum size (`logNmax`)}
\centering
\begin{tabular}[t]{l|l|r|r|r|r|r}
\hline
colony & converged & tau & logN0 & logLam & decay & logNmax\\
\hline
104 & TRUE & 6.42 & 3.44 & 0.380 & -0.541 & 5.78\\
\hline
17 & TRUE & 6.36 & 3.39 & 0.407 & -0.662 & 5.83\\
\hline
20 & TRUE & 7.27 & 2.79 & 0.194 & -0.345 & 4.14\\
\hline
24 & TRUE & 6.23 & 4.06 & 0.167 & -0.391 & 5.06\\
\hline
\end{tabular}
\end{table}

Using the `autoplot()` function from the `ggplot2` package on the output object generates a faceted plot with raw data and best fit lines (Figure \@ref(fig:plot)).



![(\#fig:plot)Results of analysis by the `bumbl()` function as visualized by `ggplot2::autoplot()`.  Each facet represents one of the four colonies. Raw data are plotted as points with the red line representing the fitted values for those points.](paper_files/figure-latex/plot-1.pdf) 



# Acknowledgments

We thank the rOpenSci reviewers Rebecca Killick and Mark Padgham for helpful suggestions and contributions.
We thank Jeremy Hemberger and Genevieve Pugesek for beta testing and feedback on early versions of the package.

# References
