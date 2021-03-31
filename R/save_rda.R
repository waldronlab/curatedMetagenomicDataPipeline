#' save a list of matrix objects
#'
#' When the output of \code{\link[curatedMetagenomicDataPipeline]{read_dir}} is
#' piped to `save_rda` (and `dir_path` is provided), an `rda` file ready to be
#' uploaded to AWS S3 will be saved at the location specified. Here, `dir_path`
#' should be the top-level directory (e.g. `~/uploads/`) in which an organized
#' directory structure will be created. That is, users should not provided a
#' nested directory path, existing after the first run, to `dir_path`.
#'
#' @param list_obj list object obtained from
#' \code{\link[curatedMetagenomicDataPipeline]{read_dir}}
#' @param dir_path path to a directory where the `rda` file will be saved
#'
#' @return the \code{\link[base]{NULL}} object will be returned invisibly
#' @export
#'
#' @seealso [read_dir()]
#'
#' @examples \dontrun{
#'
#' read_dir("~/AsnicarF_2017/genefamilies_relab/") %>%
#'     save_rda("~/uploads/")
#' }
#'
#' @importFrom purrr imap
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
#' @importFrom stringr str_c
save_rda <- function(list_obj, dir_path) {
    fn_envir <-
        base::environment()

    purrr::imap(list_obj, ~ base::assign(.y, .x, envir = fn_envir))

    obj_name <-
        base::names(list_obj)

    obj_path <-
        stringr::str_replace_all(obj_name, "\\.", "/") %>%
        base::dirname()

    new_path <-
        base::file.path(dir_path, obj_path) %>%
        stringr::str_replace_all("//", "/")

    base::dir.create(new_path, showWarnings = FALSE, recursive = TRUE)

    rda_name <-
        stringr::str_c(obj_name, ".rda")

    rda_path <-
        base::file.path(new_path, rda_name)

    base::save(list = obj_name, file = rda_path, compress = "bzip2")
}
