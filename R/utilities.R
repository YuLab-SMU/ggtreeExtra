#normgroupxy <- function(refda, targetda, group, targetid, orientation, 
#                        na.rm=TRUE, keepzero=FALSE, ratio=0.38){
#    refda1 <- split(refda, refda[[group]])
#    newval <- lapply(refda1, function(dd) orientation *
#                     normxy(refnum=refda$x, targetnum=dd[[targetid]], ratio=ratio))
#    for (i in seq_len(length(newval))){
#        refda1[[i]][[paste0("new_", targetid)]] <- newval[[i]]
#    }
#    refda <- do.call("rbind", refda1)
#    return(refda)
#}

#' @importFrom stats var
normxy <- function(refnum, targetnum, na.rm=TRUE, 
                   keepzero=FALSE, ratio=0.38){
    refnum <- checkref(refnum)
    rmax <- max(refnum, na.rm=na.rm) * ratio
    if (!keepzero){
        if (var(targetnum)==0){
            return (rep(rmax, length(targetnum)))
        }
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
    if (length(refnum)<=50){
       tmpstep <- (rmax - rmin)/step
    }else{
       tmpstep <- (rmax - rmin)/length(refnum)
    }
    refnum <- seq(from=rmin, to=rmax, by=tmpstep)
    return(refnum)
}

reset_params <- function(defaultp, inputp){
    intdi <- intersect(names(inputp), names(defaultp))
    setd <- setdiff(names(defaultp), names(inputp))
    seti <- setdiff(names(inputp), names(defaultp))
    intdi <- inputp[match(intdi, names(inputp))]
    setd <- defaultp[match(setd, names(defaultp))]
    seti <- inputp[match(seti, names(inputp))]
    newp <- c(intdi, setd, seti)
    return(newp)
}

extract_dot_params <- function(defaultp, inputp){
    dotname <- setdiff(names(inputp), names(defaultp))
    dotp <- inputp[match(dotname, names(inputp))]
    return (dotp)
}

confuse_params <- function(inputp){
    if (!is.null(inputp$line.colour)){
        inputp$line.color <- inputp$line.colour
        inputp$line.colour <- NULL    
    }
    if (!is.null(inputp$line.col)){
        inputp$line.color <- inputp$line.col
        inputp$line.col <- NULL
    }
    return(inputp)
}
