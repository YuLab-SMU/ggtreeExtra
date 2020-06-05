##' plot tree associated data in another method.
##'
##'
##' 'geom_add()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' 
##' and other parameters, and align the graph with the tree 'p' side by side. 
##' @title geom_add
##' @param mapping aes mapping for 'geom' 
##' @param data data to plot by 'geom', the column contained tree tip labels 
##' should be as y in mapping.
##' @param geom geom function to plot the data.
##' @param offset numeric, distance between panels, the ratio of distance 
##' to tree, default is 0.03.
##' @param pratio numeric, the ratio of new geom to tree, default is 0.2.
##' @param addbrink logical, whether add the brink of different layers, 
##' default is FALSE.
##' @param linesize numeric, the size of line of brink when 'addbrink' 
##' is TRUE, default is 0.1.
##' @param linecol character, the color of line of brink when 'addbrink'
##' is TRUE, default is "grey50".
##' @param tippoint logical, whether add the tip point layer, 
##' only for geom_point, geom_star or geom_symbol layer, default is FALSE.
##' @param alignpoint logical, whether adjust the shape angle, default is FALSE,
##' this parameter only for the geom_star.
##' @param ... additional parameters for 'geom'
##' @return ggplot object
##' @export
##' @author Shuangbin Xu and Guangchuang Yu
##' @examples
##' library(ggtree)
##' library(ggplot2)
##' tr <- rtree(10)
##' dd = data.frame(id=tr$tip.label, value=abs(rnorm(10)))
##' p <- ggtree(tr)
##' p <- p + geom_add(data=dd, 
##'                   geom=geom_bar, 
##'                   mapping=aes(x=value, y=id),
##'                   orientation="y",
##'                   stat="identity",
##'                   position=position_stackx())
geom_add <- function(mapping=NULL, data, geom, 
                     offset=0.03, pratio=0.2, 
                     addbrink=FALSE,
                     linesize=0.1,
                     linecol="grey50",
                     tippoint=FALSE,
                     alignpoint=FALSE,...){
    params <- list(...)
    structure(list(data = data,
                   geom = geom, 
                   mapping = mapping,
                   params = params,
                   offset = offset,
                   pratio = pratio,
                   addbrink=addbrink,
                   linesize=linesize,
                   linecol=linecol,
                   tippoint=tippoint,
                   alignpoint=alignpoint), 
              class = 'add_plot')
}
