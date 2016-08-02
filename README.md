
<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

This package contains methods for converting bioconductor objects into tidy data frames, specifically PharmacoSet objects from the PharmacoGx package and MultiAssayExperiment objects from the MultiAssayExperiment package. These tidy data frames are then used to create plots, make interactive shiny applications, and perform modelling functions using packages from the 'Tidyverse': ggplot2, tidyr, dplyr, and broom. Subsetting is an integral part of the tidying functions, and happens at the native object level before tidying, which helps to improve performance.

Scope
-----

Note: it is not intended that these functions will be released as a package in its current form, since the biobroom package alreadys carries out similar operations, although not on the objects of interest here and not quite in the same way. Therefore it is likely that these functions will either be subsumed into biobroom, the PharmacoGx/MultiAssayExperiment packages, or form part of a the package that contains the visualisation and modelling functions.
