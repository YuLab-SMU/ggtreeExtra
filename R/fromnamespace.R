#' @importFrom utils getFromNamespace
stack_var <- getFromNamespace("stack_var", "ggplot2")

collide <- getFromNamespace("collide", "ggplot2")

collide2 <- getFromNamespace("collide2", "ggplot2")

pos_stack <- getFromNamespace("pos_stack", "ggplot2")

pos_dodge <- getFromNamespace("pos_dodge", "ggplot2")

"%||%" <- getFromNamespace("%||%", "ggplot2")

find_x_overlaps <- getFromNamespace("find_x_overlaps", "ggplot2")

pos_dodge2 <- getFromNamespace("pos_dodge2", "ggplot2")

get_layout <- getFromNamespace("get_layout", "ggtree")

# from ggplot2
with_seed_null <- function(seed, code) {
    if (is.null(seed)) {
        code
    } else {
        withr <- "withr"
        require(withr, character.only=TRUE)
        with_seed <- eval(parse(text="with_seed"))
        with_seed(seed, code)
    }
}
