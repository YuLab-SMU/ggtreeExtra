#' adjust identity position.
#'
#' @family position adjustments
#' @param vexpand numeric, Vertical expand for geoms that have a position, default is NA.
#' @param hexpand numeric, Horizon expand for geoms that have a position, default is NA.
#' @importFrom ggplot2 ggproto
#' @export
position_identityx <- function(hexpand=NA, vexpand=NA) {
    ggproto(NULL, PositionIdentityx, hexpand=hexpand, vexpand=vexpand)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto Position
#' @export
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
    }
    if (!is.na(params$hexpand)){
       data$x <- data$x + params$hexpand
       if (all(c("xmin", "xmax") %in% colnames(data))){
           data$xmin <- data$xmin + params$hexpand
           data$xmax <- data$xmax + params$hexpand
       }
    data <- data.frame(data, check.names=FALSE)
    }
  }
)
