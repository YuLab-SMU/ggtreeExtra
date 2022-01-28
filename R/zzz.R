#' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://yulab-smu.top/treedata-book/", "\n\n")

    citation <- paste0("If you use ", pkgname,
                       " in published research, please cite the paper:\n\n",
                       ggtreeExtra_citations())

    packageStartupMessage(paste0(strwrap(pillar::style_subtle(paste0(msg, citation))), collapse="\n"))
}
