#' Jitter points to avoid overplotting, and the whole points can be shifted vertically or horizontally
#'
#' This is the extension of 'position_jitter' of ggplot2, points are randomly shifted 
#' up and down and/or left and right. In addition, the whole points layer can be shifted
#' by the 'hexpand' or 'vexpand' parameter. Counterintuitively adding random noise to a 
#' plot can sometimes make it easier to read. Jittering is particularly useful for small 
#' datasets with at least one discrete position.
#'
#' @param width,height Amount of vertical and horizontal jitter. The jitter
#'   is added in both positive and negative directions, so the total spread
#'   is twice the value specified here.
#'
#'   If omitted, defaults to 40% of the resolution of the data: this means the
#'   jitter values will occupy 80% of the implied bins. Categorical data
#'   is aligned on the integers, so a width or height of 0.5 will spread the
#'   data so it's not possible to see the distinction between the categories.
#' @param hexpand,vexpand The distance to be shifted vertically or horizontally,
#'   default is NA.
#' @param seed A random seed to make the jitter reproducible.
#'   Useful if you need to apply the same jitter twice, e.g., for a point and
#'   a corresponding label.
#'   The random seed is reset after jittering.
#'   If `NA` (the default value), the seed is initialised with a random value;
#'   this makes sure that two subsequent calls start with a different seed.
#'   Use `NULL` to use the current random seed and also avoid resetting
#' @export
#' @examples
#' library(ggtree)
#' library(treeio)
#' library(ggplot2)
#' set.seed(1024)
#' tr <- rtree(10)
#' 
#' df <- data.frame(id=tr$tip.label, group=rep(c("A", "B"),5))
#' dat <- data.frame(id=rep(tr$tip.label, 8), value=rnorm(80, 0.5, 0.15))
#' dt <- merge(dat, df, by.x="id", by.y="id")
#' p1 <- ggtree(tr) %<+% df +
#'       geom_tiplab(
#'           align=TRUE,
#'           linesize=.1,
#'           size=3
#'       ) 
#' 
#' gf1 <- geom_fruit(data=dat,
#'                   geom=geom_boxplot,
#'                   mapping=aes(x=value, y=id),
#'                   orientation="y",
#'                   offset=0.1,
#'                   pwidth=0.9
#' 
#'          )
#' set.seed(1024)
#' gf2 <- geom_fruit(
#'           data=dat,
#'           geom=geom_point,
#'           mapping=aes(x=value, y=id, color=group),
#'           offset=0.1,
#'           pwidth=0.9,
#'           position= position_jitterx(width=0.1),
#'           axis.params=list(axis="x", text.size=2),
#'           grid.params=list()
#'           )
#' 
#' p2 <- p1 + geom_fruit_list(gf1, gf2)
#' p2
position_jitterx <- function(width = NULL, height = NULL, hexpand=NA, vexpand=NA, seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionJitterx,
    width = width,
    height = height,
    seed = seed,
    hexpand=NA, 
    vexpand=NA
  )
}

#' PositionJitterx
#' @format NULL
#' @usage NULL
#' @export
PositionJitterx <- ggproto("PositionJitterx", Position,
  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    list(
      width = self$width %||% (resolution(data$x, zero = FALSE) * 0.4),
      height = self$height %||% (resolution(data$y, zero = FALSE) * 0.4),
      hexpand = self$hexpand,
      vexpand = self$vexpand,
      seed = self$seed
    )
  },

  compute_layer = function(self, data, params, layout) {
    trans_x <- if (params$width > 0) function(x) jitter(x, amount = params$width)
    trans_y <- if (params$height > 0) function(x) jitter(x, amount = params$height)

    data <- with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
    if (!is.na(params$vexpand)){
        data$y <- data$y + params$vexpand
    }
    if (!is.na(params$hexpand)){
        data$x <- data$x + params$hexpand
    }
    data
  }
)
