#' @export
#' @rdname position_dodgex
#' @param padding Padding between elements at the same position. Elements are
#'   shrunk by this proportion to allow space between them. Defaults to 0.1.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
position_dodgex2 <- function(width = NULL, preserve = c("total", "single"),
                             hexpand=NA, vexpand=NA,
                             padding = 0.1, reverse = FALSE) {
  ggproto(NULL, PositionDodgex2,
    width = width,
    preserve = match.arg(preserve),
    hexpand=hexpand, vexpand=vexpand,
    padding = padding,
    reverse = reverse
  )
}

#' PositionDodgex2
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
PositionDodgex2 <- ggproto("PositionDodgex2", PositionDodgex,
  preserve = "total",
  padding = 0.1,
  reverse = FALSE,

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_dodgex2(width = ?)`")
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      if ("x" %in% names(data)) {
        # Point geom
        groups <- lapply(panels, function(panel) table(panel$x))
      } else {
        # Interval geom
        groups <- lapply(panels, find_x_overlaps)
      }
      n_groups <- vapply(groups, max, double(1))
      n <- max(n_groups)
    }

    list(
      width = self$width,
      n = n,
      hexpand = self$hexpand,
      vexpand = self$vexpand,
      padding = self$padding,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    collided <- collide2(
      data,
      params$width,
      name = "position_dodgex2",
      strategy = pos_dodge2,
      n = params$n,
      padding = params$padding,
      check.width = FALSE,
      reverse = params$reverse
    )
    data <- flip_data(collided, params$flipped_aes)
    data <- pos_dodgex(data = data,
                       hexpand = params$hexpand,
                       vexpand = params$vexpand)
  }
)

