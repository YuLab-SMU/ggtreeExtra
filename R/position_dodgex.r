#' Dodge overlapping objects side-to-side which can be shifted verically or horizontally.
#'
#' Dodging preserves the vertical position of an geom while adjusting the
#' horizontal position. `position_dodgex2` is a special case of `position_dodgex`
#' for arranging box plots, which can have variable widths. `position_dodgex2`
#' also works with bars and rectangles. But unlike `position_dodgex`,
#' `position_dodgex2` works without a grouping variable in a layer.
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. 
#' @param hexpand numeric, Horizon expand for geoms that have a position, default is NA.
#' @param vexpand numeric, Vertical expand for geoms that have a position, default is NA.
#' @param preserve Should dodging preserve the total width of all elements
#'    at a position, or the width of a single element?
#' @family position adjustments
#' @return position methods 
#' @export
#' @examples
#' library(ggplot2)
#' library(patchwork)
#' iris$ID <- rep(c(rep("test1", 15), rep("test2", 15), rep("test3", 20)),3)
#' p <- ggplot(iris, aes(x=Species,y=Petal.Length,fill=ID))
#' p1 <- p + geom_bar(stat="identity",position=position_dodgex())
#' p2 <- p + geom_bar(stat="identity",position=position_dodgex(vexpand=5))
#' p3 <- ggplot(iris, aes(x=Petal.Length, y=Species, fill=ID)) +
#'       geom_bar(stat="identity", orientation="y",
#'                position=position_dodgex(hexpand=5))
#' p4 <- p1 + p2 + p3
#' p4
#' p5 <- p + geom_boxplot(position=position_dodgex2())
#' p6 <- p + geom_boxplot(position=position_dodgex2(vexpand=5))
#' p7 <- ggplot(iris, aes(x=Petal.Length, y=Species, fill=ID)) +
#'       geom_boxplot(orientation="y",
#'                    position=position_dodgex2(hexpand=5))
#' p8 <- p5 + p6 + p7
#' p8
position_dodgex <- function(width = NULL, hexpand=NA, vexpand=NA,
                            preserve = c("total", "single")) {
  ggproto(NULL, PositionDodgex,
    width = width,
    hexpand = hexpand,
    vexpand = vexpand,
    preserve = match.arg(preserve)
  )
}

#' PositionDodgex
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
PositionDodgex <- ggproto("PositionDodgex", Position,
  width = NULL,
  hexpand = NA,
  vexpand = NA,
  preserve = "total",
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_dodge(width = ?)`")
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      ns <- vapply(panels, function(panel) max(table(panel$xmin)), double(1))
      n <- max(ns)
    }

    list(
      width = self$width,
      hexpand = self$hexpand,
      vexpand = self$vexpand,
      n = n,
      flipped_aes = flipped_aes
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if (!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
      data$x <- (data$xmin + data$xmax) / 2
    }
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    collided <- collide(
      data,
      params$width,
      name = "position_dodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE
    )
    data <- flip_data(collided, params$flipped_aes)
    data <- pos_dodgex(data = data,
                       hexpand = params$hexpand,
                       vexpand = params$vexpand)
  }
)

pos_dodgex <- function(data, hexpand, vexpand){
   if (!is.na(vexpand)){
      if (all(c("ymin", "ymax") %in% colnames(data))){
          data$ymin <- data$ymin + vexpand
          data$ymax <- data$ymax + vexpand
      }
      if (all(c("lower", "middle", "upper") %in% colnames(data))){
          data$lower <- data$lower + vexpand
          data$middle <- data$middle + vexpand
          data$upper <- data$upper + vexpand
      }
      if(all(c("density", "violinwidth") %in% colnames(data))){
          data$y <- data$y + vexpand
      }
      vhexpand <- vexpand
   }
   if (!is.na(hexpand)){
      if (all(c("xmin", "xmax") %in% colnames(data))){
          data$xmin <- data$xmin + hexpand
          data$xmax <- data$xmax + hexpand
      }
      if (all(c("xlower", "xmiddle", "xupper") %in% colnames(data))){
          data$xlower <- data$xlower + hexpand
          data$xmiddle <- data$xmiddle + hexpand
          data$xupper <- data$xupper + hexpand
      }
      if(all(c("density", "violinwidth") %in% colnames(data))){
          data$x <- data$x + hexpand 
      }
      vhexpand <- hexpand
   }
   if (!is.na(hexpand) || !is.na(vexpand)){
      if ("outliers" %in% colnames(data)){
         data$outliers <- get_outliers(data$outliers, vhexpand=vhexpand)
      }
      if (all(c("notchupper", "notchlower") %in% colnames(data))){
         data$notchupper <- data$notchupper + vhexpand
         data$notchlower <- data$notchlower + vhexpand
      }
   }
   data #<- data.frame(data, check.names=FALSE)
}

get_outliers <- function(outliers, vhexpand){
   y <- lapply(outliers, function(x){
      if (!is.null(x)){
          unlist(lapply(x, function(t) t + vhexpand))
      }
   })
}
