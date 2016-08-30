#' @title gather
#' @description S3 generic for \code{\link{gather.ExpressionSet}} and \code{\link{gather.PharmacoSet}}
#' defaulting to \code{\link[tidyr]{gather}} from the \pkg{tidyr} package for data.frames
#' @param x an ExpressionSet or PharmacoSet object
#' @param ... other arguments see \code{\link{gather.ExpressionSet}} and \code{\link{gather.PharmacoSet}}
#' @seealso \code{\link{gather.ExpressionSet}} \code{\link{gather.PharmacoSet}}
#' @export
gather <- function (x, ...) {
    UseMethod("gather", x)
}

gather.default <- function(x, ...) {
    tidyr::gather(x, ...)
}

#' @title gather_assay
#' @description S3 generic for \code{\link{gather_assay.PharmcoSet}}
#' @param x a PharmacoSet
#' @param ... other arguments see \code{\link{gather_assay.PharmcoSet}}
#' @seealso \code{\link{gather_assay.PharmcoSet}}
#' @export
gather_assay <- function (x, ...) {
    UseMethod("gather_assay", x)
}

#' @title gather_response
#' @description S3 generic for \code{\link{gather_response.PharmcoSet}} and \code{\link{gather_response.data.frame}}
#' @details gather_response
#' @param x a data.frame or PharmacoSet
#' @param ... other arguments see \code{\link{gather_response.PharmcoSet}} and \code{\link{gather_response.data.frame}}
#' @seealso \code{\link{gather_response.PharmcoSet}} \code{\link{gather_response.data.frame}}
#' @export
gather_response <- function (x, ...) {
    UseMethod("gather_response", x)
}

#' @title make_genetic_vs_genetic_df
#' @description S3 generic for \code{\link{make_genetic_vs_genetic_df.PharmcoSet}} and \code{\link{make_genetic_vs_genetic_df.data.frame}}
#' @param x a data.frame or PharmacoSet
#' @param ... other arguments see \code{\link{make_genetic_vs_genetic_df.PharmcoSet}} and \code{\link{make_genetic_vs_genetic_df.data.frame}}
#' @seealso \code{\link{make_genetic_vs_genetic_df.PharmcoSet}} \code{\link{make_genetic_vs_genetic_df.data.frame}}
#' @export
make_genetic_vs_genetic_df <- function (x, ...) {
    UseMethod("make_genetic_vs_genetic_df", x)
}

#' @title make_response_vs_genetic_df
#' @description S3 generic for \code{\link{make_response_vs_genetic_df.data.frame}}
#' @param x a data.frame
#' @param ... other arguments see \code{\link{make_response_vs_genetic_df.data.frame}}
#' @seealso \code{\link{make_response_vs_genetic_df.data.frame}}
#' @export
make_response_vs_genetic_df <- function (x, ...) {
    UseMethod("make_response_vs_genetic_df", x)
}


