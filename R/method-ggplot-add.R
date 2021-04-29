##' @method ggplot_add fruit_plot
##' @importFrom utils modifyList
##' @importFrom ggplot2 aes aes_ aes_string scale_color_manual
##' @importFrom rlang as_name quo_name
##' @importFrom ggnewscale new_scale_color
##' @author Shuangbin Xu
##' @export
ggplot_add.fruit_plot <- function(object, plot, object_name){
    #if (!is.null(object$mapping$subset)){
    #    if (is.null(object$data)){
    #        stop("When the subset is provided in mapping, the data also should be provided!")
    #    }else{
    #        object$data <- subset(object$data, eval(parse(text=quo_name(object$mapping$subset))))
    #        object$mapping <- object$mapping[names(object$mapping)!="subset"]
    #    }
    #}
    res <- set_mapping(object=object, plot=plot)
    object <- res[[1]]
    plot <- res[[2]]
    xid <- res[[3]]
    if (!is.null(object$mapping$subset)){
        object$data <- subset(object$data, eval(parse(text=quo_name(object$mapping$subset))))
        object$mapping <- object$mapping[names(object$mapping)!="subset"]
    }
    yid <- as_name(object$mapping$y)
    #layout <- get("layout", envir = plot$plot_env)
    layout <- get_layout(plot)
    flagreverse <- check_reverse(plot=plot)
    if (layout=="inward_circular" || flagreverse){
        orientation <- -1
    }else{
        orientation <- 1
    }
    offset <- get_offset(plot$data$x, object$offset)
    if ("xmaxtmp" %in% colnames(plot$data)){
        hexpand2 <- max(abs(plot$data$xmaxtmp), na.rm=TRUE) + offset
    }else{
        hexpand2 <- max(abs(plot$data$x), na.rm=TRUE) + offset
    }
    dat <- build_new_data(newdat=object$data, origindata=plot$data, yid=yid)
    if (is.numeric(dat[[xid]]) & !all(dat[[xid]]==0)){
        normres <- get_continuous_norm(refdata=plot$data$x, 
                                       data=dat, 
                                       orientation=orientation,
                                       xid=xid, 
                                       position=object$params$position, 
                                       ratio=object$pwidth,
                                       nbreak=object$axis.params$nbreak)
        dat <- normres[[1]]
        newxexpand <- normres[[2]]
    }else{
        if (!is.numeric(dat[[xid]])){
            if (!is.factor(dat[[xid]])){
                dat[[xid]] <- factor(dat[[xid]], levels=sort(unique(as.vector(dat[[xid]]))))
            }
            dat[[paste0(xid,"_bp")]] <- as.numeric(dat[[xid]])
            dat[[paste0("new_", xid)]] <- orientation * 
                                          normxy(refnum=plot$data$x, targetnum=dat[[paste0(xid,"_bp")]],
                                                 keepzero=TRUE, ratio=object$pwidth)
            if (orientation > 0){
                dat[[paste0("new_", xid)]] <- dat[[paste0("new_", xid)]] + offset
            }
            dat <- dat[order(-dat$y, dat[[paste0("new_", xid)]]),,drop=FALSE]
            newxexpand <- max(abs(dat[[paste0("new_", xid)]]), na.rm=TRUE)
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
            if (orientation < 0){
                hexpand2 <- abs(hexpand2)
            }
            object$params$position$hexpand <- hexpand2
        }
    }
    tmpangle <- dat$angle
    if (object$geomname=="geom_star"){
        dat$angle <- adjust_angle(layout=layout, angle=tmpangle)
        object$mapping = modifyList(object$mapping, aes_(angle=~angle))
    }
    if (object$geomname=="geom_text"){
        dat$angle <- adjust_text_angle(layout=layout, angle=tmpangle)
        object$mapping = modifyList(object$mapping, aes_(angle=~angle))
    }
    if (object$geomname %in% c(dodpos, densitypos)){
        object$mapping = modifyList(object$mapping, aes(color=factor(eval(parse(text="y")))))
        plot <- plot + new_scale_color()
    }
    object$mapping = modifyList(object$mapping, aes_string(x=paste0("new_",xid)))
    mapping = modifyList(object$mapping, aes_(y=~y))
    params <- c(list(data=dat, mapping=mapping, inherit.aes=object$inherit.aes), object$params)
    obj <- do.call(object$geom, params)
    if (object$axis.params$axis != "none"){
        obj.axis <- build_axis(dat=dat,
                               xid=xid,
                               text=object$axis.params$text,
                               position=object$params$position,
                               axis.params=object$axis.params,
                               axis.dot.params=object$axis.dot.params)
        obj <- list(obj, obj.axis)
    }
    if (!is.null(object$grid.params)){
        obj.grid <- build_grid(dat=dat,
                               xid=xid,
                               position=object$params$position,
                               grid.params=object$grid.params,
                               grid.dot.params=object$grid.dot.params)
        obj <- list(obj.grid, obj)
    }
    # because original y is continuous, but y of box plot density plot is discrete
    # to combine them, should map y to group or color, but sometimes group box 
    # or density plot is also a demand, so group should not be mapped, 
    # only left color.
    if (object$geomname %in% c(dodpos, densitypos)){
        obj <- list(obj, scale_color_manual(values=c(rep("black", length(dat$y))), guide="none"), new_scale_color())
    }
    ggplot_add(obj, plot, object_name)
}

##' @method ggplot_add layer_fruits
##' @author Shuangbin Xu
##' @export
ggplot_add.layer_fruits <- function(object, plot, object_name){
    offset <- get_offset(plot$data$x, object[[1]]$offset)
    if ("xmaxtmp" %in% colnames(plot$data)){
        hexpand2 <- max(abs(plot$data$xmaxtmp), na.rm=TRUE) + offset
    }else{
        hexpand2 <- max(abs(plot$data$x), na.rm=TRUE) + offset
    }
    n = 0
    for (o in object){
        n = n + 1
        if (inherits(o, "fruit_plot")){
            offset.i <- get_offset(plot$data$x, o$offset)
            if (offset != offset.i && n != 1){
                warning_wrap("The 'offset' argument of geom_fruit layers in geom_fruit_list is different. 
                              Please keep it consistent if you want to display these layers on the same position.")
                hexpand2 <- max(abs(plot$data$x), na.rm=TRUE) + offset.i
            }
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

create_text_data <- function(data, origin, newxid, flagrev){
    if (!is.numeric(data[[origin]]) || sum(diff(data[[origin]])) == diff(range(data[[origin]]))){
        data <- data[!duplicated(data),,drop=FALSE]
    }else{
        if (flagrev){
            data[[origin]] <- rev(data[[origin]])
        }
    }
    return (data)
}

set_mapping <- function(object, plot){
    if (is.null(object$data)){
        object$mapping <- modifyList(object$mapping, aes_(y=~y))
        object$data <- plot$data[plot$data$isTip,,drop=FALSE]
    }
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
    return (list(object, plot, xid))
}

get_offset <- function(vnum, ratio){
    offset <- ratio*(max(vnum, na.rm=TRUE) - min(vnum, na.rm=TRUE))
}

build_new_data <- function(newdat, origindata, yid){
    if (inherits(newdat, "data.frame") && ! all(colnames(origindata) %in% colnames(newdat))){
        origindata <- origindata[origindata$isTip, !colnames(origindata) %in% c("parent", "node", "branch.length", "isTip", "x", "branch")]
        commonnames <- intersect(colnames(newdat), colnames(origindata))
        commonnames <- commonnames[commonnames!=yid]
        if (length(commonnames) > 0){
            warning_wrap("The following column names/name: ", paste0(commonnames, collapse=", "),
                         " are/is the same to tree data, the tree data column names are : ",
                         paste0(colnames(origindata), collapse=", "), ".")
        }
        dat <- merge(origindata, newdat, by.x="label", by.y=yid)
    }else{
        dat <- newdat
    }
    return(dat)
}

adjust_angle <- function(layout, angle){
    if (!layout %in% c("rectangular", "slanted")){
        angle <- 90 - angle
    }else{
        angle <- 90
    }
    return(angle)
}

adjust_text_angle <- function(layout, angle){
    if (!layout %in% c("rectangular", "slanted")){
        angle <- unlist(lapply(angle, function(i)
                               {if (i>90 && i<270){
                                   i <- i - 180}
                               return(i)}))
    }else{
        angle <- 0
    }
    return(angle)
}

choose_pos <- function(object){
    geomname <- object$geomname
    if (is.character(object$position) && object$position=="auto"){
        if (geomname %in% dodpos){
            object$params <- c(object$params, position=position_dodgex())
        }
        if (geomname %in% idepos){
            object$params <- c(object$params, position=position_identityx())
        }
        if (geomname %in% stackpos){
            object$params <- c(object$params, position=position_stackx())
        }
        if (geomname %in% densitypos){
            object$params <- c(object$params, position=position_points_sinax())
        }
    }else{
        object$params <- c(object$params, position=object$position)
    }
    return(object)
}

dodpos <- c("geom_boxplot", "geom_violin", "geom_boxplot_pattern", 
            "geom_violin_pattern", "geom_boxploth", "geom_violinh")

idepos <- c("geom_dots", "geom_dotsinterval", "geom_pointinterval",
            "geom_slab", "geom_slabinterval", "geom_image", "geom_phylopic",
            "geom_point", "geom_tile", "geom_text", "geom_label", "geom_raster",
            "geom_plot", "geom_table", "geom_star", "geom_symbol", 
            "geom_tile_pattern", "geom_scatterpie",
            "geom_text_repel", "geom_label_repel",
            "geom_msa", "geom_richtext", "geom_ridgeline",
            "geom_ridgeline_gradient")

stackpos <- c("geom_bar", "geom_barh", "geom_bar_pattern", "geom_col", "geom_colh", "geom_col_pattern")

densitypos <- c("geom_density_ridges", "geom_density_ridges2", "geom_density_ridges_gradient")

check_reverse <- function(plot){
    flag <- unlist(lapply(plot$scales$scales, 
                          function(x){
                           inherits(x, "ScaleContinuousPosition") && x$aesthetics[1]=="x"
                          }))
    if (!all(flag)){return(FALSE)}
    flag <- plot$scales$scales[[which(flag)]]$trans$name=="reverse" && inherits(plot$coordinates, "CoordPolar")
    if (is.na(flag)){return(FALSE)}
    return(flag)
}

#' @importFrom utils getFromNamespace
warning_wrap <- getFromNamespace("warning_wrap", "ggplot2")
