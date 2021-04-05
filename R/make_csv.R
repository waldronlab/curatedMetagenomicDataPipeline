#' make a csv file of metadata
#'
#' After running \code{\link[curatedMetagenomicDataPipeline]{save_rda}},
#' `make_csv` is used to create a `csv` file describing the `rda` files as
#' specified by \code{\link[ExperimentHubData]{makeExperimentHubMetadata}}.
#' Here, `dir_path` should be the date-specific directory (e.g.
#' `~/uploads/2021-04-05/`) that was created by
#' \code{\link[curatedMetagenomicDataPipeline]{save_rda}}. That is, users should
#' provide a `dir_path` down to the date-level, but not further. When run, an
#' `ExperimentHub` ready metadata `csv` file will be saved at the location
#' specified.
#'
#' @param dir_path path to a directory where the `csv` file will be saved
#'
#' @return a \code{\link[base]{data.frame}} of metadata will be returned
#' invisibly
#' @export
#'
#' @seealso [save_rda()]
#'
#' @examples \dontrun{
#'
#' make_csv("~/uploads/2021-04-05/")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom BiocManager version
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
#' @importFrom readr write_csv
make_csv <- function(dir_path) {
    rda_path <-
        base::dir(path = dir_path, pattern = "rda", recursive = TRUE)

    Title <-
        base::basename(rda_path) %>%
        stringr::str_remove("\\.rda$")

    Description <-
        stringr::str_remove(Title, "^([^A-Z]+\\.)") %>%
        stringr::str_replace("_", " ") %>%
        stringr::str_replace("\\.", " ") %>%
        stringr::str_replace("gene_families", "Gene Families") %>%
        stringr::str_replace("marker_abundance", "Marker Abundance") %>%
        stringr::str_replace("marker_presence", "Marker Presence") %>%
        stringr::str_replace("pathway_abundance", "Pathway Abundance") %>%
        stringr::str_replace("pathway_coverage", "Pathway Coverage") %>%
        stringr::str_replace("relative_abundance", "Relative Abundance")

    BiocVersion <-
        BiocManager::version() %>%
        base::as.character()

    Genome <-
        base::as.character(NA_character_)

    SourceType <-
        base::as.character("FASTQ")

    SourceUrl <-
        base::as.character(NA_character_)

    SourceVersion <-
        base::as.character(NA_character_)

    Species <-
        base::as.character(NA_character_)

    TaxonomyId <-
        base::as.character(NA_character_)

    Coordinate_1_based <-
        base::as.logical(NA)

    DataProvider <-
        base::as.character(NA_character_)

    Maintainer <-
        base::system.file("DESCRIPTION", package = "curatedMetagenomicDataPipeline") %>%
        base::read.dcf("Maintainer") %>%
        stringr::str_remove("<") %>%
        stringr::str_remove(">")

    RDataClass <-
        base::as.character("SummarizedExperiment")

    DispatchClass <-
        base::as.character("Rda")

    dir_name <-
        base::basename(dir_path)

    RDataPath <-
        stringr::str_c("curatedMetagenomicData/", dir_name, "/", rda_path)

    Tags <-
        base::as.character("curatedMetagenomicData")

    csv_name <-
        stringr::str_c(dir_name, ".csv")

    csv_path <-
        base::file.path(dir_path, csv_name) %>%
        stringr::str_replace_all("//", "/")

    base::data.frame(Title, Description, BiocVersion, Genome, SourceType, SourceUrl, SourceVersion, Species, TaxonomyId, Coordinate_1_based, DataProvider, Maintainer, RDataClass, DispatchClass, RDataPath, Tags) %>%
        readr::write_csv(csv_path)
}
