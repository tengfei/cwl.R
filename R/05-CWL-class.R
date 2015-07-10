#' Class CWL
#'
#' Define CWL class and generic methods
#'
#' @aliases CWL-class
#' 
#' @importFrom jsonlite toJSON
#'
#' @export CWL
#' @exportClass CWL
CWL <- setRefClass("CWL",
                   methods = list(
                       getFields = function(values) {
                           'return fields as a list, used for following conversion,
                            does not assume the value is primitive type.
                           '
                           ## from Martin's code
                           ## http://stackoverflow.com/questions/18713847/return-a-list-of-fields-of-a-reference-class
                           flds = names(getRefClass()$fields())
                           if (!missing(values))
                               flds = flds[flds %in% values]
                           result = setNames(vector("list", length(flds)), flds)
                           for (fld in flds)
                               result[[fld]] = .self[[fld]]
                           result
                       },
                       toList = function(...){
                           'Convert object to a list of simple data types'
                           ## simple assumption here
                           ## need to be override to make sure everything is list
                           res <- .self$getFields()
                           res <- lapply(res, function(x){
                                   asList(x) ## until it's not s4 or cwl or SimpleLi
                           })
                           return(res)
                       },
                       toYAML = function(...){
                           'Covert object to YAML'
                           l <- .self$toList()
                           as.yaml(l, ...)
                       },
                       toJSON = function(...){
                           'Covert object to JSON'
                           l <- .self$toList()
                           jsonlite::toJSON(l, ...)
                       },
                       show = function(format = c("YAML", "JSON"), ...){
                           'pretty print YAML (default) or JSON format of an object'

                           format <- match.arg(format)
                           switch(format,
                                  YAML = {
                                      writeLines(toYAML(...))
                                  },
                                  JSON = {
                                      prettify(.self$toJSON(...))
                                  })
                       }                       
                   ))





#' Convert a object slots/fields to a list
#'
#' Doesn't like \code{as.list}, only fields and slots are converted,
#' prepare a object to be conveted to YAML/JSON.
#'
#' @param object object, could be S4/R5 object, for example, class CWL, SimpleList. 
#' @param ... other parameters passed
#'
#' @export 
#' @docType methods
#' @rdname asList-methods
setGeneric("asList", function(object, ...) standardGeneric("asList"))

#' @rdname asList-methods
#' @aliases asList,ANY-method
setMethod("asList", "ANY", function(object, ...){
    if(isS4(object)){
        ## get slots as list
        res <- getFields(object)
        res <- lapply(res, function(x){
            asList(x)
        })
    }else{
        res <- object
    }
    return(res)
})

getFields <- function(x, values){
    .nms <- slotNames(x)
    if (!missing(values))
        .nms <- .nms[.nms %in% values]
    res <- setNames(vector("list", length(.nms)), .nms)
    res
    for (nm in .nms)
        res[[nm]] <- slot(x, nm)
    res
}

#' @rdname asList-methods
#' @aliases asList,CWL-method
setMethod("asList", "CWL", function(object, ...){
    object$toList(...)
})

#' @rdname asList-methods
#' @aliases asList,SimpleList-method
setMethod("asList", "SimpleList", function(object, ...){
    if(length(object)){
        res <- lapply(object, asList)         
    }else{
        res <- ""
    }
    res
})

#' Convert a object slots/fields to a YAML
#'
#' @param object object, could be S4/R5 object, for example, class CWL, SimpleList. 
#' @param ... other parameters passed to \code{as.yaml}
#'
#' @export 
#' @docType methods
#' @rdname asYAML-methods
setGeneric("asYAML", function(object, ...) standardGeneric("asYAML"))


#' @rdname asYAML-methods
#' @aliases asYAML,ANY-method
#' @importFrom yaml as.yaml
setMethod("asYAML", "ANY", function(object, ...){
    as.yaml(asList(object), ...)
})

#' Convert a object slots/fields to a JSON
#'
#' @param object object, could be S4/R5 object, for example, class CWL, SimpleList. 
#' @param ... other parameters passed to \code{toJSON}
#'
#' @export 
#' @docType methods
#' @rdname asJSON-methods
setGeneric("asJSON", function(object, ...) standardGeneric("asJSON"))

#' @rdname asJSON-methods
#' @aliases asJSON,ANY-method
setMethod("asJSON", "ANY", function(object, ...){
    jsonlite::toJSON(asList(object), ...)
})



