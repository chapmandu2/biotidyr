#' @title Tidy data from PharmacoGx and MultiAssayExperiment objects
#'
#' @description
#' The \pkg{PharmacoGx} and \pkg{MultiAssayExperiment} packages provide containers
#' for datasets where more than one data type is available for a given sample.
#' In the case of a \linkS4class{MultiAssayExperiment} object, this is only genomic data, whereas
#' a \linkS4class{PharmacoSet} object contains both genomic and dose response data.
#'
#' This package contains functions to subset, combine and reformat data from \linkS4class{MultiAssayExperiment},
#' \linkS4class{PharmacoSet} and also simple data frames.  The output of these operations is a data
#' frame in 'tidy' format which is appropriate for use in the 'tidyverse' suite of
#' packages including \pkg{ggplot2}, \pkg{dplyr}, \pkg{tidyr}, and \pkg{broom}
#'
#' @details
#'
#' To open the vignette type \code{vignette("Overview")}
#'
#' If you want to see the package in action before deciding whether or not you want to install it,
#' there WILL BE A YOUTUBE VID.
#'
#' @docType package
#' @name biotidyr-package
#' @aliases biotidyr
#' @author Phil Chapman <phil.chapman@cruk.manchester.ac.uk>
NULL
