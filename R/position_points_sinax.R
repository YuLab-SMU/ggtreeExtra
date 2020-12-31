#' adjust ridgeline plot position which can be shifted vertically or horizontally.
#'
#' This is a position adjustment specifically for 'geom_density_ridges()', but it
#' add 'hexpand' that can control shift horizontally. 
#'
#' @family position adjustments
#' @param rel_min numeric, the relative minimum value at which a point can be placed.
#' @param rel_max numeric, the relative maximum value at which a point can be placed.
#' @param seed numeric, Random seed, if set to NULL, the current random number
#' generator is used. If set to NA, a new random random seed is generated. 
#' If set to a number, this number is used as seed for jittering only,
#' default is NULL.
#' @param hexpand numeric, distance to be shifted horizontally for geoms that
#' have a position, default is NA.
#' @return position method.
#' @importFrom ggplot2 ggproto
#' @author Shuangbin Xu
#' @export
position_points_sinax <- function(rel_min = 0.02, rel_max = 0.98, seed = NULL, 
                                 hexpand=NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionPointsSinax,
    rel_min = rel_min,
    rel_max = rel_max,
    hexpand = hexpand,
    seed = seed
  )
}

#' PositionPointsSinax
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Position
PositionPointsSinax <- ggproto("PositionPointsSinax", Position,
  required_aes = c("x", "ymin", "ymax"),

  setup_params = function(self, data) {
    list(
      rel_min = self$rel_abc %||% 0.02,
      rel_max = self$rel_max %||% 0.98,
      hexpand = self$hexpand,
      seed = self$seed
    )
  },

  compute_layer = function(data, params, panel) {
    # if there's no datatype aesthetic then we're done by default
    if (!"datatype" %in% names(data)) {
      return(data)
    }

    points <- data$datatype == "point"

    with_seed_null(params$seed,
      data$ymin[points] <- data$ymin[points] +
        (params$rel_min + (params$rel_max - params$rel_min) * runif(sum(points))) *
          (data$ymax[points] - data$ymin[points])
    )
    if (!is.na(params$hexpand)){
       data$x <- data$x + params$hexpand
    }
    data 
  }
)

