check_subset_aes <- function(object){
    if (!is.null(object$mapping$subset)){
        object$data <- subset(object$data, eval(parse(text=quo_name(object$mapping$subset))))
        object$mapping <- object$mapping[names(object$mapping)!="subset"]                                                                                                                                   
    }
    return(object)
}

#' @importFrom rlang quo_text
compute_aes <- function(object, plot){
    if("x" %in% names(object$mapping) && !quo_text(object$mapping$x) %in% colnames(object$data)){
        mappingx <- object$mapping
        reqaes <- extract_requierd_aes(object)
        mappingx <- mappingx[names(mappingx) %in% reqaes]
        geomobj <- do.call(object$geom, list(mappingx))
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
