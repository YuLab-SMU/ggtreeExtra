##' add the axis of layer of geom_fruit.
##'
##' 'geom_axis_text' was designed to add the axis of layer of geom_fruit using
##' 'geom_text'. So the dot parameters can refer the arguments of 'geom_text',
##'  except `data` and `mapping`. 
##' 
##' @title geom_axis_text
##' @param angle numeric, the angle of text, default is 0 .
##' @param size numeric, the size of text, default is 0.8 .
##' @param nlayer integer, the specified layer of geom_fruit, default is NULL,
##' which mean the last layer.
##' @param text character, the text of axis x for single tile layer, 
##' default is NULL.
##' @param nbreak integer, when the axis x is continuous, default is 4.
##' @param ... additional parameters for 'geom_text'.
##' @return text object of ggplot2.
##' @export
##' @author Shuangbin Xu
##' @examples
##' library(ggplot2)
##' library(ggtree)
##' library(ggstar)
##' set.seed(1024)
##' tr <- rtree(100)
##' dd <- data.frame(id=tr$tip.label, value=abs(rnorm(100)))
##' dt <- data.frame(id=tr$tip.label, group=c(rep("A", 50), rep("B", 50)))
##' p <- ggtree(tr, layout="fan", open.angle=10)
##' p <- p %<+% dd %<+% dt
##' p1 <- p + geom_fruit(
##'               geom=geom_star,
##'               mapping=aes(y=id, fill=group),
##'               size=2.5,
##'               starstroke=0
##'           ) +
##'           geom_fruit(
##'               geom=geom_bar,
##'               mapping=aes(x=value, y=id),
##'               orientation="y",
##'               stat="identity"
##'           ) +
##'           geom_axis_text(angle=-45, hjust=0)
geom_axis_text <- function(angle=0, 
                           size=0.8, 
                           nlayer=NULL, 
                           text=NULL, 
                           nbreak=4, 
                           ...){
    params <- list(...)
    structure(
        list(
          angle = angle, 
          size = size,
          nlayer = nlayer,
          text = text,
          nbreak = nbreak,
          params = params
        ), 
        class="fruit_axis_text"
    )
}
