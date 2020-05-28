##' @method ggplot_add add_plot
##' @importFrom utils modifyList
##' @importFrom ggplot2 aes_ aes_string
##' @importFrom rlang as_name
##' @author Shuangbin Xu
##' @export
ggplot_add.add_plot <-  function(object, plot, object_name){
    yid <- as_name(object$mapping$y)
    if ("x" %in% names(object$mapping)){
        xid <- as_name(object$mapping$x)
    }else{
        object$data$xtmp <- 0
        xid <- "xtmp"
        object$mapping <- modifyList(object$mapping,aes_(x=~xtmp))
    }
    if ("xmaxtmp" %in% colnames(plot$data)){
        hexpand2 <- max(plot$data$xmaxtmp) + object$offset
    }else{
        hexpand2 <- max(plot$data$x) + object$offset
    }
    dat <- data.frame(plot$data)[plot$data$isTip, c("y", "label")]
    dat <- merge(dat, object$data, by.x="label", by.y=yid)
    if ("hexpand" %in% names(object$params$position)){
        if (is.na(object$params$position$hexpand)){
            object$params$position$hexpand <- hexpand2
        }
    }
    if (is.numeric(dat[[xid]]) & !all(dat[[xid]]==0)){
        if ("xmaxtmp" %in% colnames(plot$data)){
            plot$data$xmaxtmp <- plot$data$xmaxtmp + hexpand2
        }else{
            plot$data$xmaxtmp <- plot$data$x + hexpand2
        }
    }else{
        if ("width" %in% names(object$params)){
            width <- object$params$width
        }else{
            width <- 0.1
        }
        tmp <- width * length(unique(dat[,xid])) + object$offset
        if ("xmaxtmp" %in% colnames(plot$data)){
            plot$data$xmaxtmp <- plot$data$xmaxtmp + tmp 
        }else{
            plot$data$xmaxtmp <- plot$data$x + tmp
        }
        if (!is.numeric(dat[,xid])){
            if (!is.factor(dat[[xid]])){
                dat[[xid]] <- factor(dat[[xid]], levels=unique(as.vector(dat[[xid]])))
            }
            xtmp <- as.numeric(dat[[xid]]) * width
            dat[[paste0(xid,"_bp")]] <- xtmp 
            dat <- dat[order(-dat$y, dat[[paste0(xid, "_bp")]]),,drop=FALSE]
	    object$mapping = modifyList(object$mapping, aes_string(x=paste0(xid,"_bp")))
        }
    }
    mapping = modifyList(object$mapping, aes_(y=~y))
    params <- c(list(data=dat, mapping=mapping), object$params)
    obj <- do.call(object$geom, params)
    #obj <- list(obj, geom_vline(xintercept=c(hexpand2, max(plot$data$xmaxtmp))))
    ggplot_add(obj, plot, object_name)
}
