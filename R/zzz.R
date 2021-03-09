#' @keywords internal
#'
#' @importFrom parallel detectCores
#' @importFrom future plan
#' @importFrom future multisession
.onAttach <- function(libname, pkgname) {
    n_workers <-
        parallel::detectCores()

    future::plan(future::multisession, workers = n_workers)
}
