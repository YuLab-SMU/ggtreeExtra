#' Randomly jitter the points in a ridgeline plot which can be shifted horizontally
#'
#' This is a position adjustment specifically for 'geom_density_ridges()' and related geoms. It
#' only jitters the points drawn by these geoms, if any. If no points are present, the plot
#' remains unchanged. The effect is similar to [`position_jitter()`]: points are randomly shifted up and down
#' and/or left and right. It add 'hexpand' that can control shift horizontally. 
#'
#' @param width Width for horizontal jittering. By default set to 0.
#' @param height Height for vertical jittering, applied in both directions (up and down). By default 0.2.
#' @param yoffset Vertical offset applied in addition to jittering.
#' @param hexpand numeric, distance to be shifted horizontally for geoms that
#' have a position, default is NA.
#' @param adjust_vlines If `TRUE`, adjusts vertical lines (as are drawn for
#'   quantile lines, for example) to align with the point cloud.
#' @param seed Random seed. If set to NULL, the current random number generator is used.
#'   If set to NA, a new random random seed is generated. If set to a number, this
#'   number is used as seed for jittering only.
#' @seealso Other position adjustments for ridgeline plots: [`position_points_sinax`], [`position_raincloudx`]
#' @export
position_points_jitterx <- function(width = 0, height = 0.2, yoffset = 0, hexpand=NA, adjust_vlines = FALSE, seed = NULL) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionPointsJitterx,
    width = width,
    height = height,
    yoffset = yoffset,
    hexpand = hexpand,
    adjust_vlines = adjust_vlines,
    seed = seed
  )
}

#' PositionPointsJitterx
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
PositionPointsJitterx <- ggproto("PositionPointsJitterx", Position,
  required_aes = c("x", "ymin", "ymax"),

  setup_params = function(self, data) {
    list(
      width = self$width %||% 0,
      height = self$height %||% 0.2,
      yoffset = self$yoffset %||% 0,
      hexpand = self$hexpand,
      adjust_vlines = self$adjust_vlines %||% FALSE,
      seed = self$seed
    )
  },

  compute_layer = function(data, params, panel) {
    # if there's no datatype aesthetic then we're done by default
    if (!"datatype" %in% names(data)) {
        if (!is.na(params$hexpand)){
            data$x <- data$x + params$hexpand
        }
        return(data)
    }

    points <- data$datatype == "point"
    with_seed_null(params$seed, {
      if (params$width > 0) {
        data$x[points] <- data$x[points] - params$width +
          2 * params$width * runif(sum(points))
      };
      data$ymin[points] <- data$ymin[points] + params$yoffset - params$height +
        2 * params$height * runif(sum(points))
    })

    # do we need to adjust vertical lines as well?
    if (!params$adjust_vlines) {
        if (!is.na(params$hexpand)){
            data$x <- data$x + params$hexpand
        }
        return(data) # no, we're done
    }

    vlines <- data$datatype == "vline"
    data$ymin[vlines] <- data$ymin[vlines] + params$yoffset - params$height
    data$ymax[vlines] <- data$ymin[vlines] + 2 * params$height
    if (!is.na(params$hexpand)){
        data$x <- data$x + params$hexpand
    }
    data
  }
)


#' Create a cloud of randomly jittered points below a ridgeline plot which can be shifted horizontally
#'
#' This is a position adjustment specifically for 'geom_density_ridges()' and related geoms. It
#' only jitters the points drawn by these geoms, if any. If no points are present, the plot
#' remains unchanged. The effect is similar to [`position_points_jitterx()`], only that by default the
#' points lie all underneath the baseline of each individual ridgeline. 
#' It add 'hexpand' that can control shift horizontally.
#'
#' The idea for this position adjustment comes from Micah Allen, who proposed this type of plot in
#' a [blog post](https://micahallen.org/2018/03/15/introducing-raincloud-plots/) on March 15, 2018.
#' 
#'
#' @param width Width for horizontal jittering. By default set to 0.
#' @param height Total height of point cloud. By default 0.4.
#' @param ygap Vertical gap between ridgeline baseline and point cloud.
#' @param hexpand numeric, distance to be shifted horizontally for geoms that
#' have a position, default is NA.
#' @param adjust_vlines If `TRUE`, adjusts vertical lines (as are drawn for
#'   quantile lines, for example) to align with the point cloud.
#' @param seed Random seed. See [`position_points_jitterx`].
#' @seealso Other position adjustments for ridgeline plots: [`position_points_jitterx`], [`position_points_sinax`]
#' @export
position_raincloudx <- function(width = 0, height = 0.4, ygap = 0.05, hexpand = NA, adjust_vlines = FALSE, seed = NULL) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionRaincloudx,
          width = width,
          height = height,
          ygap = ygap,
          hexpand = hexpand,
          adjust_vlines = adjust_vlines,
          seed = seed
  )
}

#' PositionRaincloudx
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
PositionRaincloudx <- ggproto("PositionRaincloudx", PositionPointsJitterx,
  required_aes = c("x", "ymin", "ymax"),

  setup_params = function(self, data) {
    height <- (self$height %||% 0.4)/2
    yoffset <- -height - (self$ygap %||% 0.05)
    list(
      width = self$width %||% 0,
      height = height,
      yoffset = yoffset,
      hexpand = self$hexpand,
      adjust_vlines = self$adjust_vlines %||% FALSE,
      seed = self$seed
    )
  }
)
