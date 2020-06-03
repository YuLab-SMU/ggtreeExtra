##' @method ggplot_add add_plot
##' @importFrom utils modifyList
##' @importFrom ggplot2 aes_ aes_string geom_vline
##' @importFrom rlang as_name
##' @author Shuangbin Xu
##' @export
ggplot_add.add_plot <-  function(object, plot, object_name){
    yid <- as_name(object$mapping$y)
    if ("x" %in% names(object$mapping)){
        xid <- as_name(object$mapping$x)
        if (xid == "x"){
            object$data[["xtmp"]] <- object$data$x
            xid <- "xtmp"
            object$mapping <- modifyList(object$mapping,aes_string(x=xid))
        }
    }else{
        object$data$xtmp <- 0
        xid <- "xtmp"
        object$mapping <- modifyList(object$mapping,aes_string(x=xid))
    }
    offset <- get_offset(plot$data$x, object$offset)
    if ("xmaxtmp" %in% colnames(plot$data)){
        hexpand2 <- max(plot$data$xmaxtmp) + offset
    }else{
        hexpand2 <- max(plot$data$x) + offset
    }
    dat <- data.frame(plot$data, check.names=FALSE)[plot$data$isTip, c("y", "label")]
    dat <- merge(dat, object$data, by.x="label", by.y=yid)
    if (is.numeric(dat[[xid]]) & !all(dat[[xid]]==0)){
        if ("xmaxtmp" %in% colnames(plot$data)){
            plot$data$xmaxtmp <- plot$data$xmaxtmp + hexpand2
        }else{
            plot$data$xmaxtmp <- plot$data$x + hexpand2
        }
        dat[[paste0("new_",xid)]] <- normxy(refnum=plot$data$x, 
                                            targetnum=dat[[xid]],
                                            ratio=object$pratio)
    }else{
        if (!is.numeric(dat[[xid]])){
            if (!is.factor(dat[[xid]])){
                dat[[xid]] <- factor(dat[[xid]], levels=unique(as.vector(dat[[xid]])))
            }
            dat[[paste0(xid,"_bp")]] <- as.numeric(dat[[xid]])
            dat[[paste0("new_", xid)]] <- normxy(refnum=plot$data$x,
                                                 targetnum=dat[[paste0(xid,"_bp")]],
                                                 keepzero=TRUE,
                                                 ratio=object$pratio) + offset
            dat <- dat[order(-dat$y, dat[[paste0("new_", xid)]]),,drop=FALSE]
            newxexpand <- max(dat[[paste0("new_", xid)]], na.rm=TRUE)
        }else{
            if (object$tippoint){
                dat[[paste0("new_", xid)]] <- data.frame(plot$data, check.names=FALSE)[match(dat$label,plot$data$label),"x"]
            }else{
                dat[[paste0("new_", xid)]] <- 0
            }
            newxexpand <- 0
        }
        if ("xmaxtmp" %in% colnames(plot$data)){
            plot$data$xmaxtmp <- plot$data$xmaxtmp + newxexpand + offset
        }else{
            plot$data$xmaxtmp <- plot$data$x + newxexpand + offset
        }
    }
    if ("hexpand" %in% names(object$params$position)){
        if (is.na(object$params$position$hexpand)){
            object$params$position$hexpand <- hexpand2
        }
    }
    object$mapping = modifyList(object$mapping, aes_string(x=paste0("new_",xid)))
    mapping = modifyList(object$mapping, aes_(y=~y))
    params <- c(list(data=dat, mapping=mapping), object$params)
    obj <- do.call(object$geom, params)
    if (object$addbrink){
        obj <- list(obj, geom_vline(xintercept=hexpand2, 
                                    color=object$linecol, 
                                    size=object$linesize))
    }
    ggplot_add(obj, plot, object_name)
}

get_offset <- function(vnum, ratio){
    offset <- ratio*(max(vnum, na.rm=TRUE) - min(vnum, na.rm=TRUE))
}
