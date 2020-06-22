##' plot tree associated data in another method.
##'
##'
##' 'geom_fruit()' automatically re-arranges the input 'data' according to the tree structure,
##' visualizes the 'data' on specific 'panel' using the 'geom' function with aesthetic 'mapping' 
##' and other parameters, and align the graph with the tree 'p' side by side. 
##' The default position parameters is 'auto'. If you want to set manually. You can use 
##' 'position_stackx()' or 'position_dodgex()' for 'geom_bar', 'position_identityx()' for 
##' 'geom_tile', 'geom_point', 'geom_star', 'geom_symbol' or other layers using 'identity' 
##' position in 'ggplot2', and 'position_dodgex()' or 'position_dodgex2()' for 'geom_boxplot'
##' 'geom_violin' or other layers using 'dodge' position in 'ggplot2'.
##'
##' @title geom_fruit
##' @rdname geom_fruit
##' @param p tree view
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
##' @param position Position adjustment, either as a string, or the result of a
##' call to a position adjustment function, default is 'auto'.
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
##'
##' p1 <- p + geom_fruit(data=dt,
##'                   geom=geom_star,
##'                   mapping=aes(y=id, fill=group),
##'                   size=2.5,
##'                   starstroke=0)
##' p2 <- p1 + geom_fruit(data=dd, 
##'                   geom=geom_bar, 
##'                   mapping=aes(x=value, y=id),
##'                   orientation="y",
##'                   stat="identity")  
##'
##' p3 <- fruit_plot(p=p, 
##'                  data=dt, 
##'                  geom=geom_star,
##'                  mapping=aes(y=id, fill=group),
##'                  size=2.5, starstroke=0)
##'
##' p4 <- fruit_plot(p=p3,
##'                  data=dd,
##'                  geom=geom_bar,
##'                  mapping=aes(x=value, y=id),
##'                  orientation="y",
##'                  stat="identity")
geom_fruit <- function(mapping=NULL, data, geom, 
                       offset=0.03, pwidth=0.2, 
                       addbrink=FALSE,
                       linesize=0.1,
                       linecol="grey50",
                       position="auto",
                       ...){
    geomname <- as.character(as.list(match.call())[["geom"]])
    if (geomname=="geom"){
        calls <- evalq(match.call(), parent.frame(1))
        geomname <- as.character(as.list(calls)[["geom"]])
    }
    params <- list(...)
    obj <- structure(list(data = data,
                          geom = geom, 
                          mapping = mapping,
                          params = params,
                          offset = offset,
                          pwidth = pwidth,
                          addbrink=addbrink,
                          linesize=linesize,
                          linecol=linecol,
                          position=position,
                          geomname=geomname), 
                     class = 'fruit_plot')
    obj <- choose_pos(object=obj)
}

#' @rdname geom_fruit
#' @export
fruit_plot <- function(p, data, geom, 
                        mapping=NULL, offset=0.03,
                        pwidth=0.2, addbrink=FALSE,
                        linesize=0.1,
                        linecol="grey50", 
                        position="auto",
                        ...){

    p <- p + geom_fruit(data=data, geom=geom,
                        mapping=mapping, offset=offset,
                        pwidth=pwidth, addbrink=addbrink,
                        linesize=linesize, linecol=linecol,
                        position=position,
                        ...)
    return(p)
}

##' add the layers to the same position out of ggtree.
##'
##' @title geom_fruit_list
##' @param fruit the layer of geom_fruit.
##' @param ..., another layers of geom_fruit.
##' @return ggplot object
##' @export
##' @author Shuangbin Xu and GuangChuang Yu
geom_fruit_list <- function(fruit, ...){
    if(!all(unlist(lapply(list(fruit, ...), function(x)inherits(x, "fruit_plot"))))){
        stop("The all fruit layers should be fruit_plot class.")
    }
    obj <- structure(list(fruit, ...), class="layer_fruits")
}

