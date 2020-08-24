#' @importFrom ggplot2 aes_string
#' @importFrom stats aggregate
#' @importFrom stats as.formula
build_grid <- function(dat, xid, hexpand, position, grid.params, grid.dot.params){
    newxid <- paste0("new_", xid)
    yr <- range(dat$y)
    daline2 <- dat[,"y",drop=FALSE]
    dat <- dat[,match(c(xid, newxid, "label"),colnames(dat))]
    if (inherits(position, "PositionStackx")){
        dat <- aggregate(as.formula(paste0(". ~","label")), dat, sum)
    }
    dat <- dat[,match(c(xid, newxid),colnames(dat))]
    daline1 <- create_text_data(data=dat, origin=xid, newxid=newxid, nbreak=grid.params$nbreak)
    daline1$y <- yr[1]/10
    daline1$yend <- Inf
    obj1 <- list(size=grid.params$size,
                 colour=grid.params$color,
                 alpha=grid.params$alpha,
                 lineend=grid.params$lineend,
                 linejoin=grid.params$linejoin)
    obj1$data <- daline1
    obj1$mapping <- aes_string(x=newxid, xend=newxid, y="y", yend="yend")
    obj1$position <- position_identityx(hexpand=hexpand)
    obj1 <- c(obj1, grid.dot.params)
    obj1 <- do.call("geom_segment", obj1)
    if (grid.params$add.vline){
        xr <- range(daline1[[newxid]])
        daline2 <- rbind(data.frame(y=yr[1]/10),daline2,data.frame(y=Inf))
        daline2$x <- xr[1]
        daline2$xend <- xr[2]
        obj2 <- list(size=grid.params$size,
                 colour=grid.params$color,
                 alpha=grid.params$alpha,
                 lineend=grid.params$lineend,
                 linejoin=grid.params$linejoin)
        obj2$data <- daline2
        obj2$mapping <- aes_string(x="x",xend="xend",y="y",yend="y")
        obj2$position <- position_identityx(hexpand=hexpand)
        obj2 <- c(obj2, grid.dot.params)
        obj2 <- do.call("geom_segment", obj2)
        obj1 <- list(obj1, obj2)
    }
    return (obj1)
}

build_axis <- function(dat, xid, text, hexpand, position, axis.params, axis.dot.params){
    newxid <- paste0("new_", xid)
    yr <- range(dat$y)
    dat <- dat[,match(c(xid, newxid, "label"),colnames(dat))]
    if (inherits(position, "PositionStackx")){
        dat <- aggregate(as.formula(paste0(". ~","label")), dat, sum)
    }
    dat <- dat[,match(c(xid, newxid),colnames(dat))]
    dat <- create_text_data(data=dat, origin=xid, newxid=newxid, nbreak=axis.params$nbreak)
    if (nrow(dat)==1 && !is.null(text)){
        dat[[xid]] <- text
    }
    print(hexpand)
    print(dat)
    obj <- list(size=axis.params$text.size, angle=axis.params$text.angle)
    obj$data <- dat
    obj$mapping <- aes_string(x=newxid, y=0, label=xid)
    obj$position <- position_identityx(hexpand=hexpand)
    obj <- c(obj, axis.dot.params)
    if (nrow(dat)==1){
        dat2 <- data.frame(x=dat[[newxid]]-yr[1]/8, xend=dat[[newxid]]+yr[1]/8)
    }else{
        dat2 <- data.frame(x=min(dat[[newxid]])-min(dat[[newxid]])/2,
                           xend=max(dat[[newxid]])+min(dat[[newxid]])/2)
    }
    obj2 <- list(
                size=axis.params$line.size, 
                colour=axis.params$line.color,
                alpha=axis.params$line.alpha
            )
    obj2$data <- dat2
    obj2$mapping <- aes_string(x="x",xend="xend",y=yr[1]/10, yend=yr[1]/10)
    obj2$position <- position_identityx(hexpand=hexpand)
    if (nrow(dat)==1){
        dat3 <- data.frame(x=c(dat[[newxid]]-yr[1]/8, dat[[newxid]], dat[[newxid]]+yr[1]/8),
                           xend=c(dat[[newxid]]-yr[1]/8, dat[[newxid]], dat[[newxid]] + yr[1]/8))
    }else{
        dat3 <- data.frame(x=dat[[newxid]],xend=dat[[newxid]])
    }   
    dat3$y <- yr[1]/10
    dat3$yend <- yr[1]/25
    obj3 <- list(size=axis.params$line.size, 
                 colour=axis.params$line.color, 
                 alpha=axis.params$line.alpha)
    obj3$data <- dat3
    obj3$mapping <- aes_string(x="x",xend="xend",y="y", yend="yend")
    obj3$position <- position_identityx(hexpand=hexpand)
    obj <- do.call("geom_text", obj)
    obj2 <- do.call("geom_segment", obj2)
    obj3 <- do.call("geom_segment", obj3)
    obj <- list(obj2, obj3, obj)
}


