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

normxy <- function(refnum, targetnum, na.rm=TRUE, 
                   keepzero=FALSE, ratio=0.38){
    target_sign <- sign(targetnum)
    targetnum <- abs(targetnum)
    if (all(refnum <= 0, na.rm = TRUE)){
        refnum <- abs(refnum)
        if (all(target_sign <=0, na.rm = TRUE)){
            orientation <- 1
        }else{
            orientation <- -1
        }
    }else{
        if (all(target_sign <= 0, na.rm = TRUE)){
            orientation <- -1
        }else{
            orientation <- 1
        }
    }
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
    newnum <- target_sign * newnum
    if (all(target_sign <=0, na.rm = TRUE) && orientation==-1){
        return(newnum)
    }else if(all(target_sign <= 0, na.rm = TRUE) && orientation==1){
        newnum <- -1 * newnum
        return(newnum)
    }else{
        newnum <- orientation * newnum
        return(newnum)
    }
}

checkref <- function(refnum){
    ZAP <- .Machine$double.xmin
    rmin <- min(refnum, na.rm=TRUE)
    rmax <- max(refnum, na.rm=TRUE)
    refnum <- c(rmin, ZAP, rmax)
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

check_orientation <- function(x){
    xx <- do.call(x, list())
    return('orientation' %in% xx$geom$extra_params)
}


#' @importFrom rlang quo_get_expr
.convert_to_name <- function(x){
    x <- quo_get_expr(x)
    if (inherits(x, 'call')){
        x <- gsub("^\"|\"$", "", rlang::as_label(x))
    }
    if (grepl("\\(*.\\)$", x)){
        x <- gsub("\\(*.\\)", "", x)
    }else{
        x <- rlang::as_name(x)
    }
    if (!(grepl('^geom_', x) && is.layer(x))){
        cli::cli_abort(c("The {.arg geom} argument should be a name of geometric function defined in {.pkg ggplot2}",
                         "or other {.pkg ggplot2-extension}, for example, 
                         {.code geom = geom_col} or {.code geom = geom_tile} etc."
                         ))
    }
    return(x)
}

is.layer <- function(x){
    x <- inherits(do.call(x, list()), 'Layer')
    return(x)
}
