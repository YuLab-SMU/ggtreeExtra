##' plot tree associated data in another method.
##'
##'
##' 'geom_totree()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' 
##' and other parameters, and align the graph with the tree 'p' side by side. 
##' @title geom_totree
##' @param mapping aes mapping for 'geom' 
##' @param data data to plot by 'geom', the column contained tree tip labels 
##' should be as y in mapping.
##' @param geom geom function to plot the data.
##' @param offset numeric, distance between panels, the ratio of distance 
##' to tree, default is 0.03.
##' @param pwidth numeric, the width of new geom layer, this is the ratio of the 
##' new geom to tree, default is 0.2.
##' @param addbrink logical, whether add the brink of different layers, 
##' default is FALSE.
##' @param linesize numeric, the size of line of brink when 'addbrink' 
##' is TRUE, default is 0.1.
##' @param linecol character, the color of line of brink when 'addbrink'
##' is TRUE, default is "grey50".
##' @param ... additional parameters for 'geom'
##' @return ggplot object
##' @export
##' @author Shuangbin Xu and Guangchuang Yu
##' @examples
##' library(ggtree)
##' library(ggplot2)
##' library(ggstar)
##' tr <- rtree(100)
##' dd = data.frame(id=tr$tip.label, value=abs(rnorm(100)))
##' dt = data.frame(id=tr$tip.label, group=c(rep("A",50),rep("B",50)))
##' p <- ggtree(tr, layout="circular")
##' p <- p + geom_totree(data=dt,
##'                   geom=geom_star,
##'                   mapping=aes(y=id, fill=group),
##'                   size=2.5,
##'                   starstroke=0)
##' p <- p + geom_totree(data=dd, 
##'                   geom=geom_bar, 
##'                   mapping=aes(x=value, y=id),
##'                   orientation="y",
##'                   stat="identity",
##'                   position=position_stackx())  
geom_totree <- function(mapping=NULL, data, geom, 
                     offset=0.03, pwidth=0.2, 
                     addbrink=FALSE,
                     linesize=0.1,
                     linecol="grey50",
                     ...){
    calls <- match.call()
    params <- list(...)
    structure(list(data = data,
                   geom = geom, 
                   mapping = mapping,
                   params = params,
                   offset = offset,
                   pwidth = pwidth,
                   addbrink=addbrink,
                   linesize=linesize,
                   linecol=linecol,
                   geomname=as.character(as.list(calls)[["geom"]])), 
              class = 'add_plot')
}
