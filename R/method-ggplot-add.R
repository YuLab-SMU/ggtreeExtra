##' @method ggplot_add fruit_plot
##' @importFrom utils modifyList
##' @importFrom ggplot2 aes aes_ aes_string geom_vline scale_color_manual
##' @importFrom rlang as_name
##' @author Shuangbin Xu
##' @export
ggplot_add.fruit_plot <- function(object, plot, object_name){
    res <- set_mapping(object=object, plot=plot)
    object <- res[[1]]
    plot <- res[[2]]
    xid <- res[[3]]
    yid <- as_name(object$mapping$y)
    layout <- get("layout", envir = plot$plot_env)
    offset <- get_offset(plot$data$x, object$offset)
    if ("xmaxtmp" %in% colnames(plot$data)){
        hexpand2 <- max(plot$data$xmaxtmp, na.rm=TRUE) + offset
    }else{
        hexpand2 <- max(plot$data$x, na.rm=TRUE) + offset
    }
    dat <- build_new_data(newdat=object$data, origindata=plot$data, yid=yid)
    if (is.numeric(dat[[xid]]) & !all(dat[[xid]]==0)){
        dat[[paste0("new_",xid)]] <- normxy(refnum=plot$data$x, 
                                            targetnum=dat[[xid]],
                                            ratio=object$pwidth)
        newxexpand <- max(dat[[paste0("new_", xid)]], na.rm=TRUE)
    }else{
        if (!is.numeric(dat[[xid]])){
            if (!is.factor(dat[[xid]])){
                dat[[xid]] <- factor(dat[[xid]], levels=unique(as.vector(dat[[xid]])))
            }
            dat[[paste0(xid,"_bp")]] <- as.numeric(dat[[xid]])
            dat[[paste0("new_", xid)]] <- normxy(refnum=plot$data$x,
                                                 targetnum=dat[[paste0(xid,"_bp")]],
                                                 keepzero=TRUE,
                                                 ratio=object$pwidth) + offset
            dat <- dat[order(-dat$y, dat[[paste0("new_", xid)]]),,drop=FALSE]
            newxexpand <- max(dat[[paste0("new_", xid)]], na.rm=TRUE)
        }else{
            if (!"hexpand" %in% names(object$params$position)){
                dat[[paste0("new_", xid)]] <- data.frame(plot$data, check.names=FALSE)[match(dat$label,plot$data$label),"x"]
            }else{
                dat[[paste0("new_", xid)]] <- 0
            }
            newxexpand <- 0
        }
    }
    if ("xmaxtmp" %in% colnames(plot$data)){
        plot$data$xmaxtmp <- plot$data$xmaxtmp + newxexpand + offset
    }else{
        plot$data$xmaxtmp <- plot$data$x + newxexpand + offset
    }
    if ("hexpand" %in% names(object$params$position)){
        if (is.na(object$params$position$hexpand)){
            object$params$position$hexpand <- hexpand2
        }
    }
    dat$angle <- adjust_angle(layout=layout, angle=dat$angle)
    if (object$geomname=="geom_star"){
        object$mapping = modifyList(object$mapping, aes_(angle=~angle))
    }
    if (object$geomname %in% c("geom_boxplot", "geom_violin")){
        object$mapping = modifyList(object$mapping, aes(color=factor(eval(parse(text="y")))))
    }
    object$mapping = modifyList(object$mapping, aes_string(x=paste0("new_",xid)))
    mapping = modifyList(object$mapping, aes_(y=~y))
    params <- c(list(data=dat, mapping=mapping), object$params)
    obj <- do.call(object$geom, params)
    if (object$geomname %in% c("geom_boxplot", "geom_violin")){
        obj <- list(obj, scale_color_manual(values=c(rep("black", length(dat$y)))), new_scale_color())
    }
    if (object$addbrink){
        obj <- list(obj, geom_vline(xintercept=hexpand2, 
                                    color=object$linecol, 
                                    size=object$linesize)) 
    }
    ggplot_add(obj, plot, object_name)
}

##' @method ggplot_add layer_fruits
##' @author Shuangbin Xu
##' @export
ggplot_add.layer_fruits <- function(object, plot, object_name){
    offset <- get_offset(plot$data$x, object[[1]]$offset)
    if ("xmaxtmp" %in% colnames(plot$data)){
        hexpand2 <- max(plot$data$xmaxtmp, na.rm=TRUE) + offset
    }else{
        hexpand2 <- max(plot$data$x, na.rm=TRUE) + offset
    }
    n = 0
    for (o in object){
        n = n + 1
        if (inherits(o, "fruit_plot")){
            o[["params"]][["position"]][["hexpand"]] <- hexpand2
        }
        plot <- plot + o
        if ("xmaxtmp" %in% colnames(plot$data) && n == 1){
            tmpxmax <- plot$data$xmaxtmp
        }
        if (!"xmaxtmp" %in% colnames(plot$data)){
            tmpxmax <- plot$data$x + hexpand2
        }
    }
    plot$data$xmaxtmp <- tmpxmax
    plot
}


##' @method ggplot_add fruit_axis_text
##' @author Shuangbin Xu
##' @importFrom rlang as_name
##' @export
ggplot_add.fruit_axis_text <- function(object, plot, object_name){
    if (is.null(object$nlayer)){
        nlayer <- extract_num_layer(plot=plot, num=length(plot$layer))
    }else{
        nlayer <- object$nlayer + 2 
    }
    xid <- as_name(plot$layer[[nlayer]]$mapping$x)
    orixid <- sub("new_", "", xid)
    dat <- plot$layer[[nlayer]]$data[,c(xid, orixid),drop=FALSE]
    dat <- creat_text_data(data=dat, origin=orixid, newxid=xid, nbreak=object$nbreak)
    dat[[xid]] <- dat[[xid]] + plot$layer[[nlayer]]$position$hexpand
    obj <- list(size=object$size, angle=object$angle)
    obj$data <- dat
    obj$mapping <- aes_string(x=xid, y=0, label=orixid)
    obj <- c(obj, object$params)
    attr(plot$layer[[nlayer]], "AddAxisText") <- TRUE
    if (nlayer > 2 && "hexpand" %in% names(plot$layer[[nlayer]]$position)){
        obj <- do.call("geom_text", obj)
        plot <- plot + obj
        return(plot)
    }else{
        return(plot)
    }
}

creat_text_data <- function(data, origin, newxid, nbreak){
    if (!is.numeric(data[[origin]]) || sum(diff(data[[origin]])) == diff(range(data[[origin]]))){
        data <- data[!duplicated(data),,drop=FALSE]
    }else{
        originx <- range(data[[origin]], na.rm=TRUE)
        originx <- seq(originx[1], originx[2], length.out=nbreak)
        newx <- range(data[[newxid]], na.rm=TRUE)
        newx <- seq(newx[1], newx[2], length.out=nbreak)
        tmpdigits <- max(attr(regexpr("(?<=\\.)0+", originx, perl = TRUE), "match.length"))
        if (tmpdigits<=0){
            tmpdigits <- 1
        }else{
            tmpdigits <- tmpdigits + 1
        }
        originx <- round(originx, digits=tmpdigits)
        data <- data.frame(v1=newx, v2=originx)
        colnames(data) <- c(newxid, origin)
    }
    return (data)
}

extract_num_layer <- function(plot, num){
    if (inherits(plot$layer[[num]]$geom, "GeomText") && !"hexpand" %in% names(plot$layer[[num]]$position) && num >=3){
        num <- num - 1
        extract_num_layer(plot=plot, num=num)
    }else if("AddAxisText" %in% names(attributes(plot$layer[[num]])) && "hexpand" %in% names(plot$layer[[num]]$position) && num >= 3){
        num <- num - 1
        extract_num_layer(plot=plot, num=num)
    }else{
        return(num)
    }
}

set_mapping <- function(object, plot){
    if (is.null(object$data)){
        object$mapping <- modifyList(object$mapping, aes_(y=~y))
        if ("x" %in% names(object$mapping)){
            xid <- as_name(object$mapping$x)
            if (xid == "x"){
                plot$data[["xtmp"]] <- plot$data$x
                xid <- "xtmp"
                object$mapping <- modifyList(object$mapping,aes_string(x=xid))
            }
        }else{
            plot$data$xtmp <- 0
            xid <- "xtmp"
            object$mapping <- modifyList(object$mapping,aes_string(x=xid))
        }
    }else{
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
    }
    return (list(object, plot, xid))
}

get_offset <- function(vnum, ratio){
    offset <- ratio*(max(vnum, na.rm=TRUE) - min(vnum, na.rm=TRUE))
}

build_new_data <- function(newdat, origindata, yid){
    if (!is.null(newdat) && inherits(newdat, "data.frame")){
        origindata <- origindata[origindata$isTip, colnames(origindata) %in% c("y", "label", "angle")]
        commonnames <- intersect(colnames(newdat), colnames(origindata))
        commonnames <- commonnames[commonnames!=yid]
        if (length(commonnames) > 0){
            warning_wrap("The following column names/name: ", paste0(commonnames, collapse=", "),
                         " are/is the same to tree data, the tree data column names are : ",
                         paste0(colnames(origindata), collapse=", "), ".")
        }
        dat <- merge(origindata, newdat, by.x="label", by.y=yid)
    }else{
        dat <- origindata[origindata$isTip,,drop=TRUE]
    }
    return(dat)
}

adjust_angle <- function(layout, angle){
    if (layout!="rectangular"){
        angle <- 90 - angle
    }else{
        angle <- 90
    }
    return(angle)
}

choose_pos <- function(object){
    geomname <- object$geomname
    if (is.character(object$position) && object$position=="auto"){
        if (geomname %in% c("geom_boxplot", "geom_violin")){
            object$params <- c(object$params, position=position_dodgex())
        }
        if (geomname %in% c("geom_point", "geom_star", "geom_symbol", "geom_tile")){
            object$params <- c(object$params, position=position_identityx())
        }
        if (geomname=="geom_bar"){
            object$params <- c(object$params, position=position_stackx())
        }
    }else{
        object$params <- c(object$params, position=object$position)
    }
    return(object)
}

#' @importFrom utils getFromNamespace
warning_wrap <- getFromNamespace("warning_wrap", "ggplot2")
