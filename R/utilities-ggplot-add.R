check_subset_aes <- function(object){
    if (!is.null(object$mapping$subset)){
        object$data <- subset(object$data, eval(parse(text=quo_name(object$mapping$subset))))
        object$mapping <- object$mapping[names(object$mapping)!="subset"] 
    }
    return(object)
}

defaults <- function (x, y){
    c(x, y[setdiff(names(y), names(x))])
}

#' @importFrom rlang quo_text
compute_aes <- function(object, plot){
    if("x" %in% names(object$mapping) && !quo_text(object$mapping$x) %in% colnames(object$data)){
        mappingx <- object$mapping
        reqaes <- extract_requierd_aes(object)
        mappingx <- mappingx[names(mappingx) %in% reqaes]
        geomobj <- do.call(object$geom, list(mappingx))
        if (utils::packageVersion("ggplot2") > '3.3.3'){
            # the 3.3.4 > version of ggplot2 introduces computed_mapping argument. 
            if (isTRUE(object$inherit.aes)) {
                geomobj$computed_mapping <- defaults(geomobj$mapping, plot$mapping)
                class(geomobj$computed_mapping) <- "uneval"
            } else {
                geomobj$computed_mapping <- geomobj$mapping
            }
        }
        data_after_cal <- suppressWarnings(geomobj$compute_aesthetics(data=object$data, plot=plot))
        object$data[["x"]] <- data_after_cal[["x"]]
        object$mapping <- modifyList(object$mapping, aes_string(x="x"))
	}
    return (object)
}

extract_requierd_aes <- function(object){
    geomobj <- do.call(object$geom, list())
    reqaes <- geomobj$geom$required_aes
    if (any(grepl("\\|", reqaes))){
        flagindex <- grepl("\\|", reqaes)
        tmpreqaes <- reqaes[flagindex]
        reqaes <- reqaes[!flagindex]
        tmpreqaes <- unlist(strsplit(tmpreqaes, "\\|"))
        reqaes <- c(reqaes, tmpreqaes)
    }
    return(reqaes)
}

set_mapping <- function(object){
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
    return (list(object, xid))
}

.set_pwidth2width <- function(x){
    if (x$geomname == 'geom_tile' && !'width' %in% names(x$params)){
        cli::cli_warn(c("The column of {.field x} aesthetic only have one unique value with {.code geom = geom_tile},",
                        "and the {.arg width} of {.fn geom_tile} is not provided, the {.arg pwidth} will be as {.arg width}."))
        x$params$width <- x$pwidth
    }
    return(x)
}

.generate_colour_warning <- function(x){
    index <- which(.get_colour_aes_num(x))
    if (length(index)>0){
        message <- vector()
        for (i in index){
            old.nm <- names(x$labels)[i]
            new.nm <- paste0(names(x$labels)[i], '_new')
            message <- append(message, c("The {.fn new_scale_color} was used in the internal.",
                            "The aesthetics : {.field {old.nm}} was renamed to {.field {new.nm}},",
                            "please specify the {.code aesthetics=\"{new.nm}\"} in the {.fn scale_color_manual}",
                            " or {.fn scale_color_continuous} etc. to adjust the {.code colour}."
                            ))
        }
        cli::cli_inform(message)
    }
}

.get_colour_aes_num <- function(x){
    index <- grepl("colour", names(x$labels))
    index2 <- grepl("factor\\(eval\\(parse\\(text", x$labels)
    index[index2] <- FALSE
    return(index)
}

