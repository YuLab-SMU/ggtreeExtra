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
    target_sign <- sign(targetnum)
    targetnum <- abs(targetnum)
    if (all(refnum <= 0, na.rm=TRUE)){
        refnum <- abs(refnum)
        if (all(target_sign <=0)){
            orientation <- 1
        }else{
            orientation <- -1
        }
    }else{
        if (all(target_sign <= 0)){
            orientation <- -1
        }else{
            orientation <- 1
        }
    }
    refnum <- checkref(refnum)
    rmax <- max(refnum, na.rm=na.rm) * ratio
    if (!keepzero){
        #if (var(targetnum)==0){
        #    return (rep(rmax, length(targetnum)))
        #}
        rmin <- min(refnum[refnum!=0], na.rm=na.rm)
    }else{
        rmin <- min(refnum, na.rm=na.rm)
    }
    tmax <- max(targetnum, na.rm=na.rm)
    tmin <- min(targetnum, na.rm=na.rm)
    k <- (rmax - rmin)/(tmax - tmin)
    newnum <- k*(targetnum - tmin) + rmin
    newnum[targetnum==0] <- 0
    newnum <- target_sign * newnum
    if (all(target_sign <=0 ) && orientation==-1){
        return(newnum)
    }else if(all(target_sign <= 0) && orientation==1){
        newnum <- -1 * newnum
        return(newnum)
    }else{
        newnum <- orientation * newnum
        return(newnum)
    }
}

checkref <- function(refnum, n=5, step=2000){
    rmin <- min(refnum, na.rm=TRUE)
    rmax <- max(refnum, na.rm=TRUE)
    if (length(refnum)<=500){
       tmpstep <- (rmax - rmin)/step
    }else{
       tmpstep <- (rmax - rmin)/length(refnum)
    }
    refnum <- seq(from=rmin, to=rmax, by=tmpstep)
    return(refnum)
}

#' @importFrom rlang get_expr
reset_params <- function(defaultp, inputp){
    if (is.null(get_expr(inputp))){
        return(NULL)
    }
    inputp <- parse_list_input(input=inputp)
    intdi <- intersect(names(inputp), names(defaultp))
    setd <- setdiff(names(defaultp), names(inputp))
    seti <- setdiff(names(inputp), names(defaultp))
    intdi <- inputp[match(intdi, names(inputp))]
    setd <- defaultp[match(setd, names(defaultp))]
    seti <- inputp[match(seti, names(inputp))]
    newp <- c(intdi, setd, seti)
    return(newp)
}

#' @importFrom rlang get_env
parse_list_input <- function(input){
    expr <- get_expr(input)
    env <- get_env(input)
    inputp <- as.list(expr)
    inputp <- inputp[nchar(names(inputp)) > 0]
    #inputp[[1]] <- NULL
    #inputp <- inputp[unname(unlist(lapply(inputp, function(x)nchar(x)>0 && x!="...")))]
    for (i in seq_len(length(inputp))){
        if (is.symbol(inputp[[i]])){
            values <- as.vector(inputp[[i]], mode="character")
            inputp[[i]] <- get(values, envir = env)
        }
    }
    return (inputp)
}

extract_dot_params <- function(defaultp, inputp){
    if (is.null(inputp)){
        return(NULL)
    }
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
    #if (!is.null(inputp$text.colour)){
    #    inputp$text.color <- inputp$text.colour
    #    inputp$text.colour <- NULL
    #}
    #if (!is.null(inputp$text.col)){
    #    inputp$text.color <- inputp$text.col
    #    inputp$text.col <- NULL
    #}
    if (!is.null(inputp$title.colour)){
        inputp$title.color <- inputp$title.colour
        inputp$title.colour <- NULL
    }
    if (!is.null(inputp$title.col)){
        inputp$title.col <- inputp$title.col
        inputp$title.col <- NULL
    }
    return(inputp)
}

ggtreeExtra_citations <- function(){
    paste(
        "S Xu, Z Dai, P Guo, X Fu, S Liu, L Zhou, W Tang, T Feng, M Chen, L Zhan, T Wu, E Hu, Y Jiang, X Bo, G Yu.",
        "ggtreeExtra: Compact visualization of richly annotated phylogenetic data.",
        "Molecular Biology and Evolution 2021, 38(9):4039-4042. doi: 10.1093/molbev/msab166\n"
        )
}
