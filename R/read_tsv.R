#' @keywords internal
#'
#' @importFrom furrr future_imap
#' @importFrom readr read_tsv
#' @importFrom furrr furrr_options
read_tsv <- function(x, y) {
    if (y == "metaphlan_bugs_list") {
        furrr::future_imap(x, ~ readr::read_tsv(.x, col_types = "?-?-", col_names = base::c("rowname", .y), comment = "#", progress = FALSE), .options = furrr::furrr_options(seed = NULL))
    } else {
        furrr::future_imap(x, ~ readr::read_tsv(.x, col_types = "??", col_names = base::c("rowname", .y), comment = "#", progress = FALSE), .options = furrr::furrr_options(seed = NULL))
    }
}
