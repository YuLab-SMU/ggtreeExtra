#' Simultaneously dodge and jitter, and the whole layer can be shifted vertically or horizontally 
#'
#' This is primarily used for aligning points generated through
#' 'geom_point()' with dodged boxplots (e.g., a 'geom_boxplot()' with
#' a fill aesthetic supplied). And the points can be shifted vertically 
#' or horizontally with 'hexpand' or 'vexpand' arguments.
#'
#' @param jitter.width degree of jitter in x direction. Defaults to 40% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param dodge.width the amount to dodge in the x direction. Defaults to 0.75,
#'   the default `position_dodge()` width.
#' @inheritParams position_jitterx
#' @export
position_jitterdodgex <- function(jitter.width = NULL, jitter.height = 0,
                                 dodge.width = 0.75, hexpand=NA, vexpand=NA, 
                                 seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionJitterdodgex,
    jitter.width = jitter.width,
    jitter.height = jitter.height,
    dodge.width = dodge.width,
    hexpand = hexpand,
    vexpand = vexpand,
    seed = seed
  )
}

#' PositionJitterdodgex
#' @format NULL
#' @usage NULL
#' @export
PositionJitterdodgex <- ggproto("PositionJitterdodgex", Position,
  jitter.width = NULL,
  jitter.height = NULL,
  dodge.width = NULL,

  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    width <- self$jitter.width %||% (resolution(data$x, zero = FALSE) * 0.4)
    # Adjust the x transformation based on the number of 'dodge' variables
    dodgecols <- intersect(c("fill", "colour", "linetype", "shape", "size", "alpha"), colnames(data))
    if (length(dodgecols) == 0) {
      abort("`position_jitterdodge()` requires at least one aesthetic to dodge by")
    }
    # returns NULL for numeric, i.e. non-dodge layers
    ndodge <- lapply(data[dodgecols], levels)  
    ndodge <- length(unique(unlist(ndodge)))

    list(
      dodge.width = self$dodge.width,
      jitter.height = self$jitter.height,
      jitter.width = width / (ndodge + 2),
      hexpand = self$hexpand,
      vexpand = self$vexpand,
      seed = self$seed,
      flipped_aes = flipped_aes
    )
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    data <- collide(data, params$dodge.width, "position_jitterdodge", pos_dodge,
      check.width = FALSE)

    trans_x <- if (params$jitter.width > 0) function(x) jitter(x, amount = params$jitter.width)
    trans_y <- if (params$jitter.height > 0) function(x) jitter(x, amount = params$jitter.height)

    data <- with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
    data <- flip_data(data, params$flipped_aes)
    if (!is.na(params$vexpand)){
        data$y <- data$y + params$vexpand
        data$ymax <- data$ymax + params$vexpand
    }
    if (!is.na(params$hexpand)){
        data$x <- data$x + params$hexpand
        data$xmax <- data$xmax + params$hexpand
    }
    data
  }
)
