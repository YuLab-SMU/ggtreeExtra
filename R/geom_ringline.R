##' add the grid line to external ring layer of tree
##'
##' geom_ringline is designed to add grid line to external ring 
##' layer of tree, it only be added after `geom_fruit`.
##' @param colour character, the color of ring line, default is "grey50".
##' @param size numeric, the size of ring line, default is 0.1.
##' @param alpha numeric, the colour transparency of line, default is 1.
##' @param lineend character, Line end style (round, butt, square),
##' default is "butt".
##' @param linejoin character, Line join style (round, mitre, bevel),
##' default is "round".
##' @param nbreak integer, this is only efficient when axis x is continuous, 
##' default is 4. 
##' @param addgrid logical, whether add the grid line of y in external 
##' ring layer of tree, default is FALSE.
##' @author Shuangbin Xu
##' @export
##' @examples
##' library(ggtree)
##' library(ggplot2)
##' library(ggstar)
##' set.seed(1024)
##' tr <- rtree(100)
##' dd = data.frame(id=tr$tip.label, value=abs(rnorm(100)))
##' dt = data.frame(id=tr$tip.label, group=c(rep("A",50),rep("B",50)))
##' p <- ggtree(tr, layout="circular")
##'
##' p1 <- p +
##'       geom_fruit(
##'           data=dt,
##'           geom=geom_star,
##'           mapping=aes(y=id, fill=group),
##'           size=2.5,
##'           starstroke=0
##'       )
##' p2 <- p1 +
##'       geom_fruit(
##'           data=dd,
##'           geom=geom_bar,
##'           mapping=aes(x=value, y=id),
##'           orientation="y",
##'           stat="identity"
##'       ) +
##'       geom_ringline()
geom_ringline <- function(colour="grey50",
                          size=0.1,
                          alpha=1,
                          lineend="butt",
                          linejoin="round",
                          nbreak=4,
                          addgrid=FALSE
                         ){
    structure(
        list(
            size=size,
            colour=colour,
            alpha=alpha,
            lineend=lineend,
            linejoin=linejoin,
            nbreak=nbreak,
            addgrid=addgrid
        ),
        class="fruit_ringline"
    )
}
