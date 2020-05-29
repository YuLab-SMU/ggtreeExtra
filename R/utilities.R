#' @importFrom utils getFromNamespace
stack_var <- getFromNamespace("stack_var", "ggplot2")

collide <- getFromNamespace("collide", "ggplot2")

pos_stack <- getFromNamespace("pos_stack", "ggplot2")

pos_dodge <- getFromNamespace("pos_dodge", "ggplot2")

"%||%" <- getFromNamespace("%||%", "ggplot2")


normxy <- function(refnum, targetnum, na.rm=TRUE, ratio=0.38){
    rmax <- max(refnum, na.rm=na.rm) * ratio
    rmin <- min(refnum[refnum!=0], na.rm=na.rm)
    tmax <- max(targetnum, na.rm=na.rm)
    tmin <- min(targetnum, na.rm=na.rm)
    k <- (rmax - rmin)/(tmax - tmin)
    newnum <- k*(targetnum - tmin) + rmin
    return(newnum)
}
