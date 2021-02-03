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
##' 'geom_violin' or other layers using 'dodge' position in 'ggplot2'. The axis line and text 
##' can be added using 'axis.params=list(axis="x",...)', and the grid line also can be added
##' using 'grid.params=list(...)'.
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
##' @param position Position adjustment, either as a string, or the result of a
##' call to a position adjustment function, default is 'auto'.
##' @param grid.params list, the parameters to control the attributes of grid lines, 
##' default is NULL.
##' @param axis.params list, the parameters to control the attributes of pseudo axis.
##' @param ... additional parameters for 'geom'
##' 
##' grid.params control the attributes of grid line, it can be referred to the
##' following parameters:
##'     \itemize{
##'         \item \code{vline} logical, whether add the vertical line, default is FALSE.
##'         \item \code{color} color of line, default is grey.
##'         \item \code{size} the width of line, default is 0.2.
##'         \item \code{alpha} the colour transparency of line, default is 1.
##'         \item \code{lineend} Line end style (round, butt, square), default is "butt".
##'         \item \code{linejoin} Line end style (round, butt, square), default is "round".
##'         \item \code{linetype} Type of line, default is 1.
##'     } 
##'
##' axis.params control the attributes of pseudo axis, it can be referred to the
##' following parameters:
##'     \itemize{
##'         \item \code{axis} character, add the pseudo axis, "none" don't display axis (default), 
##'         "x" display the x axis, "y" display the (y) axis of tree tip, "xy" display the two axis.
##'         \item \code{text} vector, the text of axis x, default is NULL, it is only valid when
##'         the text of axis is single and x is discrete.
##'         \item \code{vjust} numeric, A numeric specifying vertical justification, default is 0.5.
##'         \item \code{hjust} numeric, A numeric specifying horizontal justification, default is 0.5.
##'         \item \code{text.angle} numeric, the angle of axis text, default is 0.
##'         \item \code{text.size} numeric, the size of axis text, default is 0.8.
##'         \item \code{title} character, the title of panel or x-axis label, default is NULL, it is only
##'          valid when "x" axis exists.
##'         \item \code{title.size} numeric, the size of title text, default is 3.
##'         \item \code{title.height} numeric, the height of title text position more than tree, default is 0.1,
##'          it is relative to height of tree.
##'         \item \code{title.angle} numeric, the angle of title text, default is 0.
##'         \item \code{title.color} character, the color of title text, default is "black".
##'         \item \code{nbreak} numeric, meaning the number of axis to break, 
##'          integer giving the _desired_ number of intervals. Non-integer values are rounded down.
##'          It is only valid when x is continuous, default is 4.
##'         \item \code{line.size} numeric, the size of axis line, default is 0.2.
##'         \item \code{line.color} character, the color of axis color, default is "grey".
##'         \item \code{line.alpha} numeric, the colour transparency of line, default is 1.
##'     }
##'
##' @return ggplot object
##' @export
##' @author Shuangbin Xu and Guangchuang Yu
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
##'       )  
##'
##' p3 <- fruit_plot(
##'            p=p, 
##'            data=dt, 
##'            geom=geom_star,
##'            mapping=aes(y=id, fill=group),
##'            size=2.5, 
##'            starstroke=0
##'       )
##'
##' p4 <- fruit_plot(
##'            p=p3,
##'            data=dd,
##'            geom=geom_bar,
##'            mapping=aes(x=value, y=id),
##'            orientation="y",
##'            stat="identity"
##'       )
##' p <- p %<+% dd %<+% dt
##' p5 <- p + 
##'       geom_fruit(
##'           geom = geom_star,
##'           mapping = aes(y=id, fill=group),
##'           size = 2.5,
##'           starstroke = 0
##'       ) +
##'       geom_fruit(
##'           geom = geom_bar,
##'           mapping = aes(x=value, y=id),
##'           orientation = "y",
##'           stat = 'identity'
##'       )
geom_fruit <- function(mapping, 
                       data=NULL, 
                       geom, 
                       offset=0.03, 
                       pwidth=0.2, 
                       position="auto",
                       grid.params=NULL,
                       axis.params=list(
                                       axis="none",
                                       text.angle=0,
                                       text.size=0.8,
                                       text=NULL,
                                       title = NULL,
                                       title.size = 3,
                                       title.height = 0.1,
                                       title.angle = 0,
                                       title.color = "black",
                                       nbreak=4,
                                       line.size=0.2,
                                       line.color="grey",
                                       line.alpha=1,
                                       ...
                                   ),
                       ...){
    geomname <- as.character(as.list(match.call())[["geom"]])
    if (geomname=="geom"){
        calls <- evalq(match.call(), parent.frame(1))
        geomname <- as.character(as.list(calls)[["geom"]])
    }
    default.grid.params <- list(color="grey",
                                size=0.2,
                                alpha=1,
                                lineend="butt",
                                linejoin="round",
                                vline=FALSE)
    default.axis.params <- list(axis="none",
                                text.angle=0, 
                                text.size=0.8,
                                text=NULL, 
                                title = NULL,
                                title.size = 3,
                                title.height = 0.1,
                                title.angle = 0,
                                title.color = "black",
                                line.size=0.2, 
                                line.color="grey",
                                nbreak=4,
                                line.alpha=1)
    params <- list(...)
    grid.params <- reset_params(defaultp=default.grid.params, 
                                inputp=substitute(grid.params))
    axis.params <- reset_params(defaultp=default.axis.params, 
                                inputp=substitute(axis.params))
    axis.params <- confuse_params(axis.params)
    axis.dot.params <- extract_dot_params(
                           defaultp=default.axis.params,
                           inputp=axis.params
                       ) 
    grid.dot.params <- extract_dot_params(
                           defaultp=default.grid.params,
                           inputp=grid.params
                       )
    # for MSA
    if (geomname == "geom_msa"){
        if (missing(mapping)){
            mapping <- aes_(x = ~position, y = ~name, fill = ~I(color))
        }
    }
    obj <- structure(
             list(
               data = data,
               geom = geom, 
               mapping = mapping,
               params = params,
               offset = offset,
               pwidth = pwidth,
               position=position,
               geomname=geomname,
               grid.params=grid.params,
               axis.params=axis.params,
               grid.dot.params=grid.dot.params,
               axis.dot.params=axis.dot.params
             ), 
             class = 'fruit_plot'
           )
    obj <- choose_pos(object=obj)
}

#' @rdname geom_fruit
#' @export
fruit_plot <- function(p, 
                       data=NULL, 
                       geom, 
                       mapping, 
                       offset=0.03,
                       pwidth=0.2, 
                       position="auto",
                       ...){

    p <- p + 
         geom_fruit(
             data=data, 
             geom=geom,
             mapping=mapping, 
             offset=offset,
             pwidth=pwidth, 
             position=position,
             ...
         )
    return(p)
}

##' add the layers to the same position out of ggtree.
##'
##' @title geom_fruit_list
##' @param fruit the layer of geom_fruit.
##' @param ..., another layers of geom_fruit, or scales.
##' @return ggplot object
##' @export
##' @author Shuangbin Xu and GuangChuang Yu
##' @examples
##' library(ggplot2)
##' library(ggtree)
##' library(ggstar)
##' library(ggnewscale)
##' set.seed(1024)
##' tr <- rtree(100)
##' dt <- data.frame(id=tr$tip.label, value=abs(rnorm(100)), group=c(rep("A",50),rep("B",50)))
##' df <- dt
##' dtf <- dt
##' colnames(df)[[3]] <- "group2"
##' colnames(dtf)[[3]] <- "group3"
##' 
##' p <- ggtree(tr, layout="fan", open.angle=0)
##' # first circle
##' p1 <- p + 
##'       geom_fruit(
##'           data=dt,
##'           geom=geom_bar,
##'           mapping=aes(y=id, x=value, fill=group),
##'           orientation="y",
##'           stat="identity"
##'       ) + 
##'       new_scale_fill()
##' # second circle
##' fruitlist <- geom_fruit_list(
##'                  geom_fruit(
##'                      data = df,
##'                      geom = geom_bar,
##'                      mapping = aes(y=id, x=value, fill=group2),
##'                      orientation = "y",
##'                      stat = "identity",
##'                      addbrink=FALSE
##'                  ),
##'                  scale_fill_manual(values=c("blue", "red")),
##'                  new_scale_fill(),
##'                  geom_fruit(
##'                      data = dt,
##'                      geom = geom_star,
##'                      mapping = aes(y=id, x=value, fill=group),
##'                      size = 2.5,
##'                      color = NA,
##'                      starstroke = 0
##'                  )
##'              )
##' 
##' p2 <- p1 + fruitlist + new_scale_fill() 
##' # third circle
##' p3 <- p2 + 
##'       geom_fruit(
##'           data=dtf,
##'           geom=geom_bar,
##'           mapping = aes(y=id, x=value, fill=group3),
##'           orientation = "y",
##'           stat = "identity"
##'       ) +
##'       scale_fill_manual(values=c("#00AED7", "#009E73"))
##' p3 
geom_fruit_list <- function(fruit, ...){
    if(!inherits(fruit, "fruit_plot")){
        stop("The first layers should be 'fruit_plot' class.")
    }
    obj <- structure(list(fruit, ...), class="layer_fruits")
}

