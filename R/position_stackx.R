#' Stack overlapping objects which can be shifted vertically or horizontally
#'
#' @param vjust Vertical adjustment for geoms that have a position
#'   (like points or lines), not a dimension (like bars or areas). Set to
#'   `0` to align with the bottom, `0.5` for the middle,
#'   and `1` (the default) for the top.
#' @param vexpand numeric, distance to be shifted vertically for geoms that 
#' have a position, default is NA.
#' @param hexpand numeric, distance to be shifted horizontally for geoms that 
#' have a position, default is NA.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
#' @return position method.
#' @importFrom ggplot2 ggproto
#' @author Shuangbin Xu
#' @export
#' @examples
#' library(ggplot2)
#' library(patchwork)
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' # 
#' p1 <- ggplot(df, aes(x=trt,  y=outcome)) +
#'       geom_bar(stat="identity",
#'                position=position_stackx())
#'
#' p2 <- ggplot(df, aes(x=trt, y=outcome)) +
#'       geom_bar(stat="identity",
#'                position=position_stackx(vexpand=5))
#' 
#' p3 <- ggplot(df, aes(x=outcome, y=trt)) +
#'       geom_bar(stat="identity",
#'                orientation="y",
#'                position=position_stackx(hexpand=5))
#' p <- p1 + p2 + p3
#' p
position_stackx <- function(vjust = 1, hexpand=NA, vexpand=NA, reverse = FALSE) {
    ggproto(NULL, PositionStackx, vjust = vjust, reverse = reverse,
          hexpand=hexpand, vexpand=vexpand)
}

#' PositionStackx
#' @importFrom ggplot2 ggproto Position has_flipped_aes flip_data
#' @importFrom ggplot2 remove_missing
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @author Shuangbin Xu
PositionStackx <- ggproto("PositionStackx", Position,
  type = NULL,
  vjust = 1,
  fill = FALSE,
  reverse = FALSE,
  hexpand = NA,
  vexpand = NA,

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    list(
      var = self$var %||% stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse,
      flipped_aes = flipped_aes,
      hexpand = self$hexpand,
      vexpand = self$vexpand
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }

    data$ymax <- switch(params$var,
      y = data$y,
      ymax = ifelse(data$ymax == 0, data$ymin, data$ymax)
    )

    data <- remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_stackx"
    )
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    if (is.null(params$var)) {
      return(data)
    }
    
    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE

    neg <- data[negative, , drop = FALSE]
    pos <- data[!negative, , drop = FALSE]

    if (any(negative)) {
      neg <- collide(neg, NULL, "position_stackx", pos_stack,
        vjust = params$vjust,
        fill = params$fill,
        reverse = params$reverse
      )
    }
    if (any(!negative)) {
      pos <- collide(pos, NULL, "position_stackx", pos_stack,
        vjust = params$vjust,
        fill = params$fill,
        reverse = params$reverse
      )
    }

    data <- rbind(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))),]
    data <- flip_data(data, params$flipped_aes)
    if (!is.na(params$vexpand)){
        data$ymin <- data$ymin + params$vexpand
        data$ymax <- data$ymax + params$vexpand
    }
    if (!is.na(params$hexpand)){
        data$xmin <- data$xmin + params$hexpand
        data$xmax <- data$xmax + params$hexpand
    }
    data #<- data.frame(data, check.names=FALSE)

  }
)
