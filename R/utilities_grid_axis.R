#' @importFrom ggplot2 aes_string geom_segment
#' @importFrom stats aggregate
#' @importFrom stats as.formula
build_grid <- function(dat, xid, position, grid.params, grid.dot.params){
    newxid <- paste0("new_", xid)
    yr <- range(dat$y)
    daline3 <- dat[,"y",drop=FALSE]
    if(is.numeric(dat[[xid]]) && all(dat$x>=0, na.rm=TRUE) && all(dat[[xid]]<0, na.rm=TRUE)){
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
                 linejoin=grid.params$linejoin)
    obj1$data <- daline1
    obj1$mapping <- aes_string(x="x", xend="xend", y="y", yend="yend")
    obj1$position <- position_identityx(hexpand=position$hexpand)
    obj1 <- c(obj1, grid.dot.params)
    obj1 <- do.call("geom_segment", obj1)
    return (obj1)
}

build_axis <- function(dat, xid, text, position, axis.params, axis.dot.params){
    newxid <- paste0("new_", xid)
    yr <- range(dat$y)
    if(is.numeric(dat[[xid]]) &&all(dat$x>=0, na.rm=TRUE) && all(dat[[xid]]<0, na.rm=TRUE)){
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
    obj <- list(size=axis.params$text.size, angle=axis.params$text.angle)
    obj$data <- dat
    obj$mapping <- aes_string(x=newxid, y=0, label=xid)
    obj$position <- position_identityx(hexpand=position$hexpand)
    obj <- c(obj, axis.dot.params)
    obj <- do.call("geom_text", obj)
    if (!is.null(axis.params$title)){
        yindex <- ifelse(is.null(axis.params$title.height), 0.1, axis.params$title.height)
        titledat <- data.frame(x=mean(dat[[newxid]], na.rm=TRUE), y=yr[2]*(1+yindex), label=axis.params$title)
        titleobj <- list(size=axis.params$title.size, angle=axis.params$title.angle, color=axis.params$title.color)
        titleobj$data <- titledat
        titleobj$mapping <- aes_(x=~x, y=~y, label=~label)
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
                alpha=axis.params$line.alpha
            )
    dat2$y <- yr[1]/10
    dat2$yend <- yr[1]/10
    obj2$mapping <- aes_string(x="x",xend="xend",y="y", yend="yend")
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
get_continuous_norm <- function(refdata, data, orientation, xid, position, ratio, nbreak){
    if (inherits(position, "PositionStackx")){
        dat <- aggregate(as.formula(paste0(". ~","label")), data[,c(xid, "label")], sum)
        dabreaks <- pretty(dat[[xid]], n=nbreak)
        if (!0 %in% dabreaks){
            dabreaks <- c(0, dabreaks)
        }
    }else{
        dabreaks <- pretty(data[[xid]], n=nbreak)
    }
    tmpdanorm <- orientation * normxy(refnum=refdata, targetnum=c(dabreaks,data[[xid]]), ratio=ratio)
    data[[paste0("new_",xid)]] <- tmpdanorm[-seq_len(length(dabreaks))]
    dabreaks <- data.frame(v1=dabreaks, v2=tmpdanorm[seq_len(length(dabreaks))])
    colnames(dabreaks) <- c(xid, paste0("new_",xid))
    attr(data, "axis_breaks") <- dabreaks
    newxexpand <- max(abs(tmpdanorm), na.rm=TRUE)
    return(list(data, newxexpand))
}
