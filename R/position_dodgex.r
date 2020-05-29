#' Dodge overlapping objects side-to-side
#'
#' Dodging preserves the vertical position of an geom while adjusting the
#' horizontal position. `position_dodge2` is a special case of `position_dodge`
#' for arranging box plots, which can have variable widths. `position_dodge2`
#' also works with bars and rectangles. But unlike `position_dodge`,
#' `position_dodge2` works without a grouping variable in a layer.
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. 
#' @param hexpand numeric, Horizon expand for geoms that have a position, default is NA.
#' @param vexpand numeric, Vertical expand for geoms that have a position, default is NA.
#' @param preserve Should dodging preserve the total width of all elements
#'    at a position, or the width of a single element?
#' @family position adjustments
#' @export
position_dodgex <- function(width = NULL, hexpand=NA, vexpand=NA,
                            preserve = c("total", "single")) {
  ggproto(NULL, PositionDodgex,
    width = width,
    hexpand = hexpand,
    vexpand = vexpand,
    preserve = match.arg(preserve)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
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

pos_dodgex <- function(vexpand, hexpand, data){
   if (!is.na(vexpand)){
      if (all(c("ymin", "ymax") %in% colnames(data))){
          data$ymin <- data$ymin + vexpand
          data$ymax <- data$ymax + vexpand
      }
   }
   if (!is.na(hexpand)){
      if (all(c("xmin", "xmax") %in% colnames(data))){
          data$xmin <- data$xmin + hexpand
          data$xmax <- data$xmax + hexpand
      }
   }
   data <- data.frame(data, check.names=FALSE)
}
