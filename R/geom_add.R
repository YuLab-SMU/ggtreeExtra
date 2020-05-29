##' plot tree associated data in another method.
##'
##'
##' 'geom_add()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' and other parameters,
##' and align the graph with the tree 'p' side by side. 
##' @title geom_add
##' @param mapping aes mapping for 'geom' 
##' @param data data to plot by 'geom', the column contained tree tip labels 
##' should be as y in mapping.
##' @param geom geom function to plot the data.
##' @param offset numeric, distance between panels, the ratio of distance 
##' to tree, default is 0.03.
##' @param pratio numeric, the ratio of new geom to tree, default is 0.2.
##' @param ... additional parameters for 'geom'
##' @return ggplot objec
##' @export
##' @author Shuangbin Xu and Guangchuang Yu
##' @examples
##' library(ggtree)
##' library(ggplot2)
##' tr <- rtree(10)
##' dd = data.frame(id=tr$tip.label, value=abs(rnorm(10)))
##' p <- ggtree(tr)
##' p <- p + geom_add(data=dd, 
##'                    geom=geom_bar, 
##'                    mapping=aes(x=value, y=id),
##'                    orientation="y",
##'                    stat="identity",
##'                    position=position_stackx())
geom_add <- function(mapping=NULL, data, geom, 
                     offset=0.03, pratio=0.2, ...){
    params <- list(...)
    structure(list(data = data,
                   geom = geom, 
                   mapping = mapping,
                   params = params,
                   offset = offset,
                   pratio = pratio), 
              class = 'add_plot')
}
