#' @importFrom ggplot2 aes_string geom_segment
#' @importFrom stats aggregate
#' @importFrom stats as.formula
build_grid <- function(dat, xid, position, grid.params, grid.dot.params, y.range){
    newxid <- paste0("new_", xid)
    yr <- y.range
    daline3 <- dat[,"y",drop=FALSE]
    if(is.numeric(dat[[xid]]) && all(dat[[xid]]>=0, na.rm=TRUE) && all(dat[[xid]]<0, na.rm=TRUE)){
        flagrev <- TRUE
    }else{
        flagrev <- FALSE
    }
    if (!is.null(attr(dat, "axis_breaks"))){
        dat <- attr(dat, "axis_breaks")
    }else{
        dat <- dat[,match(c(xid, newxid),colnames(dat))]
    }
    daline1 <- create_text_data(data=dat, origin=xid, newxid=newxid, flagrev=flagrev)
    daline1 <- data.frame(x=daline1[[newxid]],xend=daline1[[newxid]])
    xr <- range(daline1$x)
    daline1$y <- yr[1]/10
    daline1$yend <- yr[2] + 1
    daline2 <- data.frame(y=c(yr[1]/10, yr[2] + 1),
                          yend=c(yr[1]/10, yr[2] + 1))
    daline2$x <- xr[1]
    daline2$xend <- xr[2]
    if (!grid.params$vline){
        daline1 <- rbind(daline1, daline2)
    }else{
        daline3 <- rbind(data.frame(y=yr[1]/10),daline3)#,data.frame(y=yr[2]+1))
        daline3$x <- xr[1]
        daline3$xend <- xr[2]
        daline3$yend <- daline3$y
        daline1 <- rbind(daline1, daline3)
    }
    obj1 <- list(size=grid.params$size,
                 colour=grid.params$color,
                 alpha=grid.params$alpha,
                 lineend=grid.params$lineend,
                 linejoin=grid.params$linejoin,
                 inherit.aes=grid.params$inherit.aes)
    obj1$data <- daline1
    obj1$mapping <- aes(x=!!sym("x"), xend=!!sym("xend"), y=!!sym("y"), yend=!!sym("yend"))
    obj1$position <- position_identityx(hexpand=position$hexpand)
    obj1 <- c(obj1, grid.dot.params)
    obj1 <- do.call("geom_segment", obj1)
    return (obj1)
}

#' @importFrom ggplot2 geom_text
build_axis <- function(dat, xid, text, position, axis.params, axis.dot.params, y.range){
    newxid <- paste0("new_", xid)
    yr <- y.range
    if(is.numeric(dat[[xid]]) &&all(dat[[xid]]>=0, na.rm=TRUE) && all(dat[[xid]]<0, na.rm=TRUE)){
        flagrev <- TRUE
    }else{
        flagrev <- FALSE
    }
    if (!is.null(attr(dat, "axis_breaks"))){
        dat <- attr(dat, "axis_breaks")
    }else{
        dat <- dat[,match(c(xid, newxid),colnames(dat))]
    }
    dat <- create_text_data(data=dat, origin=xid, newxid=newxid, flagrev=flagrev)
    if (nrow(dat)==1 && !is.null(text)){
        dat[[xid]] <- text
    }
    obj <- list(size=axis.params$text.size, angle=axis.params$text.angle, inherit.aes=axis.params$inherit.aes)
    obj$data <- dat
    obj$mapping <- aes(x=!!sym(newxid), y=0, label=!!sym(xid))
    obj$position <- position_identityx(hexpand=position$hexpand)
    obj <- c(obj, axis.dot.params)
    obj <- do.call("geom_text", obj)
    if (!is.null(axis.params$title)){
        yindex <- ifelse(is.null(axis.params$title.height), 0.1, axis.params$title.height)
        titledat <- data.frame(x=mean(dat[[newxid]], na.rm=TRUE), y=yr[2]*(1+yindex), label=axis.params$title)
        titleobj <- list(size=axis.params$title.size, angle=axis.params$title.angle, 
                         color=axis.params$title.color, inherit.aes=axis.params$inherit.aes)
        titleobj$data <- titledat
        titleobj$mapping <- aes(x=!!sym("x"), y=!!sym("y"), label=!!sym("label"))
        titleobj$position <- position_identityx(hexpand=position$hexpand)
        titleobj <- do.call("geom_text", titleobj)
        obj <- list(obj, titleobj)
    }
    if (nrow(dat)==1){
        dat2 <- data.frame(x=dat[[newxid]]-yr[1]/8, xend=dat[[newxid]]+yr[1]/8)
    }else{
        dat2 <- data.frame(x=min(dat[[newxid]]),
                           xend=max(dat[[newxid]]))
    }
    obj2 <- list(
                size=axis.params$line.size, 
                colour=axis.params$line.color,
                alpha=axis.params$line.alpha,
                inherit.aes=axis.params$inherit.aes
            )
    dat2$y <- yr[1]/10
    dat2$yend <- yr[1]/10
    obj2$mapping <- aes(x=!!sym("x"),xend=!!sym("xend"),y=!!sym("y"), yend=!!sym("yend"))
    obj2$position <- position_identityx(hexpand=position$hexpand)
    if (nrow(dat)==1){
        dat3 <- data.frame(x=c(dat[[newxid]]-yr[1]/8, dat[[newxid]], dat[[newxid]]+yr[1]/8),
                           xend=c(dat[[newxid]]-yr[1]/8, dat[[newxid]], dat[[newxid]] + yr[1]/8))
    }else{
        dat3 <- data.frame(x=dat[[newxid]],xend=dat[[newxid]])
    }   
    dat3$y <- yr[1]/10
    dat3$yend <- yr[1]/25
    if (axis.params$axis=="x"){
        dat3 <- rbind(dat2, dat3)
        obj2$data <- dat3
        obj2 <- do.call("geom_segment", obj2)
        obj <- list(obj2, obj) 
    }else if(axis.params$axis=="y"){
        sign_da4 <- sign(min(dat[[newxid]]))
        da_xend <- sign_da4 * min(abs(dat[[newxid]]))
        dat4 <- data.frame(x=0, xend=0, y=yr[1]/10, yend=yr[2]+1)
        obj2$data <- dat4
        obj2 <- do.call("geom_segment", obj2)
        obj <- list(obj2)
    }else if(axis.params$axis=="xy"){
        sign_da4 <- sign(min(dat[[newxid]]))
        da_xend <- sign_da4 * min(abs(dat[[newxid]]))
        dat4 <- data.frame(x=0, xend=0, y=yr[1]/10, yend=yr[2]+1)
        dat2 <- rbind(dat2, dat3, dat4)
        obj2$data <- dat2
        obj2 <- do.call("geom_segment", obj2)
        obj <- list(obj2, obj)
    }
    return(obj)
}

#' @importFrom stats aggregate
#' @importFrom stats as.formula
get_continuous_norm <- function(refdata, data, orientation, xid, position, geomname, ratio, nbreak, limits = NULL){
    if (inherits(position, "PositionStackx") || geomname %in% stackpos){
        if (inherits(position, "PositionStackx")){
            dat <- aggregate(as.formula(paste0(". ~","label")), data[,c(xid, "label")], sum)
        }else{
            dat <- data[,c(xid, "label")]
        }
        if (!any(dat[[xid]]==0)){
            dabreaks <- pretty(c(0, dat[[xid]]), n=nbreak)
        }else{
            dabreaks <- pretty(dat[[xid]], n=nbreak)
        }
    }else{
        dabreaks <- pretty(data[[xid]], n=nbreak)
    }
    tmpdanorm <- orientation * normxy(refnum=refdata, targetnum=c(dabreaks,data[[xid]]), ratio=ratio)
    data[[paste0("new_",xid)]] <- tmpdanorm[-seq_len(length(dabreaks))]
    dabreaks <- data.frame(v1=dabreaks, v2=tmpdanorm[seq_len(length(dabreaks))])
    colnames(dabreaks) <- c(xid, paste0("new_",xid))
    if (is.null(limits)){
        limits <- max(data[[xid]], na.rm = TRUE)
    }
    index.keep <- extract_interval(dabreaks[[xid]], limits)
    dabreaks <- dabreaks[index.keep,,drop=FALSE]
    attr(data, "axis_breaks") <- dabreaks
    newxexpand <- max(abs(tmpdanorm), na.rm=TRUE)
    return(list(data, newxexpand))
}

#' @importFrom rlang quo_text 
extract_interval <- function(x, limits){
    if (length(limits)>1){
        limits <- gsub("c\\(","",gsub(")", "", quo_text(limits))) %>%
            strsplit(split=",") %>% 
            unlist() %>% 
            as.numeric()
        return((x >= min(limits, na.rm = TRUE)) & (x <= max(limits, na.rm = TRUE)))
    }else{
        return(x <= max(limits, na.rm = TRUE))
    }
}
