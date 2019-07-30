#' @title
#' All packages' versioning
#'
#' @description
#' Returns a data.table of versions of all packages attached to your current R session, or used in
#' a simulation - including git commit if a package was installed from github and similars.
#'
#' @param simList           simList object resulting from a SpaDES call. Default is `NULL`.
#'
#' @param filePath          Character string. If passed, the table is written to the specified file path,
#'                          as an `RDS` file. Default is `NULL`.
#'
#' @details If you don't provide it, the function will return the information regarding your
#'          current R session's informartion. When running a simulation, it is automatically
#'          be created by the `spades()` and attached as the invisible object `.packagesVersions`.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom reproducible Require
#' @rdname checkPackagesVersions

checkPackagesVersions <- function(simList = NULL, filePath = NULL){
  if (is.null(simList)){
    attachedPackages <- sessionInfo()
    allPackages <- c(attachedPackages$basePkgs, names(attachedPackages$otherPkgs))
    packagesVersions <- data.table::rbindlist(lapply(X = allPackages, FUN = function(pkg){
      sha  <- remotes:::local_sha(pkg)
      pkgV <- data.table::data.table(package = pkg, version = sha, repository = ifelse(nchar(sha)==40, "git", "CRAN"))
    }))
    if (!is.null(filePath)) saveRDS(object = packagesVersions, file = filePath)
    return(packagesVersions)
  } else {
    if (!is.null(filePath)) saveRDS(object = packagesVersions, file = filePath)
    return(simList[[".packagesVersions"]])
  }
}
