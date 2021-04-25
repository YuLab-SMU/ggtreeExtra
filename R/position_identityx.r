#' adjust identity position which can be shifted vertically or horizontally.
#'
#' @family position adjustments
#' @param vexpand numeric, distance to be shifted vertically for geoms that 
#' have a position, default is NA.
#' @param hexpand numeric, distance to be shifted horizontally for geoms that
#' have a position, default is NA.
#' @return position method.
#' @importFrom ggplot2 ggproto
#' @author Shuangbin Xu
#' @export
#' @examples
#' library(ggplot2)
#' library(patchwork)
#' p <- ggplot(mtcars, aes(x=wt, y=mpg))
#' p1 <- p + geom_point(position=position_identityx()) + ylim(0, 50)
#' # whole point layer was shifted vertically (distance=5).
#' # the label of axis y should be subtracted 5 to get the true value..
#' p2 <- p + geom_point(position=position_identityx(vexpand=5)) + ylim(0, 50)
#' # whole point layer was shifted horizontally (distance=5).
#' # the label of axis x should be subtracted 5 to get the true value.
#' p3 <- ggplot(mtcars, aes(y=wt, x=mpg)) +
#'       geom_point(position=position_identityx(hexpand=5)) + xlim(0, 50)
#' p4 <- p1 + p2 + p3
#' p4
position_identityx <- function(hexpand=NA, vexpand=NA) {
    ggproto(NULL, PositionIdentityx, hexpand=hexpand, vexpand=vexpand)
}

#' PositionIdentityx
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Position
PositionIdentityx <- ggproto("PositionIdentityx", Position,
                             vexpand=NA,
                             hexpand=NA,
  setup_params = function(self, data){
     list(hexpand=self$hexpand,
          vexpand=self$vexpand)
  },
  compute_layer = function(self, data, params, layout) {
    if (!is.na(params$vexpand)){
       data$y <- data$y + params$vexpand
       if (all(c("ymin", "ymax") %in% colnames(data))){
           data$ymin <- data$ymin + params$vexpand
           data$ymax <- data$ymax + params$vexpand
       }
       if ("yend" %in% colnames(data)){
           data$yend <- data$yend + params$vexpand
       }
    }
    if (!is.na(params$hexpand)){
       data$x <- data$x + params$hexpand
       if (all(c("xmin", "xmax") %in% colnames(data))){
           data$xmin <- data$xmin + params$hexpand
           data$xmax <- data$xmax + params$hexpand
       }
       if ("xend" %in% colnames(data)){
           data$xend <- data$xend + params$hexpand
       }
    }
    data #<- data.frame(data, check.names=FALSE)
  }
)
