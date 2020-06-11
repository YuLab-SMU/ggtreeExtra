#' @importFrom utils getFromNamespace
stack_var <- getFromNamespace("stack_var", "ggplot2")

collide <- getFromNamespace("collide", "ggplot2")

collide2 <- getFromNamespace("collide2", "ggplot2")

pos_stack <- getFromNamespace("pos_stack", "ggplot2")

pos_dodge <- getFromNamespace("pos_dodge", "ggplot2")

"%||%" <- getFromNamespace("%||%", "ggplot2")

find_x_overlaps <- getFromNamespace("find_x_overlaps", "ggplot2")

pos_dodge2 <- getFromNamespace("pos_dodge2", "ggplot2")

normxy <- function(refnum, targetnum, na.rm=TRUE, 
                   keepzero=FALSE, ratio=0.38){
    refnum <- checkref(refnum)
    rmax <- max(refnum, na.rm=na.rm) * ratio
    if (!keepzero){
        rmin <- min(refnum[refnum!=0], na.rm=na.rm)
    }else{
        rmin <- min(refnum, na.rm=na.rm)
    }
    tmax <- max(targetnum, na.rm=na.rm)
    tmin <- min(targetnum, na.rm=na.rm)
    k <- (rmax - rmin)/(tmax - tmin)
    newnum <- k*(targetnum - tmin) + rmin
    newnum[targetnum==0] <- 0
    return(newnum)
}

checkref <- function(refnum, n=5, step=40){
    rmin <- min(refnum, na.rm=TRUE)
    rmax <- max(refnum, na.rm=TRUE)
    if (length(refnum)<=5){
       tmpstep <- (rmax - rmin)/step
       refnum <- seq(from=rmin, to=rmax, by=tmpstep)
    }
    return(refnum)
}
