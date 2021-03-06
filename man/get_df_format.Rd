% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_df_format.R
\name{get_df_format}
\alias{get_df_format}
\title{Determine format of data frame}
\usage{
get_df_format(df)
}
\arguments{
\item{df}{query data frame}
}
\value{
string of data frame type
}
\description{
Helper function to determine whether a supplied data frame is of any of the
\code{\link{data-formats}} defined in the package.
}
\examples{
data(dietlein_data)
get_df_format(dietlein_data)

data(example_tall_df)
get_df_format(example_tall_df)

data(example_gvg_df)
get_df_format(example_gvg_df)

data(example_rvg_df)
get_df_format(example_rvg_df)

data(mtcars)
get_df_format(mtcars)
}

