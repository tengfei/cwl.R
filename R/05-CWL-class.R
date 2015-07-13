#' Class CWL
#'
#' Define CWL class and generic methods, no fields defeind.
#'
#' @aliases CWL-class
#' 
#' @importFrom yaml as.yaml
#' @importFrom jsonlite toJSON prettify
#'
#' @export CWL
#' @exportClass CWL
#'
#' @examples
#' ## no fields, only to provide methods to be extended
#' x <- CWL()
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
                           ## try to convert 
                           ## err <- try(yaml::as.yaml(l, ...),
                           ##            silent = TRUE)
                           ## if(inherits(err, "try-error"){
                           ##     warning("toYAML not implemented for class",
                           ##             class(.self))
                           ## })
                           yaml::as.yaml(l, ...)

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
                                      err <- try(writeLines(toYAML(...)),
                                                 silent = TRUE)
                                      if(inherits(err, "try-error")){
                                          methods::showDefault(.self)                                                            }
                                  },
                                  JSON = {
                                      err <- try(jsonlite::prettify(
                                          .self$toJSON(...)), silent = TRUE)
                                      if(inherits(err, "try-error")){
                                          methods::showDefault(.self)                                                            }
                                  })
                       }                       
                   ))





#' Convert a object slots/fields to a list, json, yaml file
#'
#' Doesn't like \code{as.list}, only fields and slots are converted,
#' prepare a object to be conveted to YAML/JSON.
#'
#' @param object object, could be S4/R5 object, for example, class CWL, SimpleList. 
#' @param ... other parameters passed to as.yaml or toJSON.
#'
#' @export 
#' @docType methods
#' @rdname as-methods
#'
#' @examples
#' ## define a S4 object
#' A <- setClass("A", slots = list(a = "character", b = "numeric"))
#' ## define a reference object which extends 'CWL' class
#' B <- setRefClass("B", fields = list(x = "character", y = "A"), contains = "CWL")
#' ## new instances
#' a <- A(a = "hello", b = 123)
#' b <- B(x = "world", y = a)
#' 
#' ## show
#' b
#' b$show("JSON")
#' b$show("YAML")
#' 
#' ## You can convert slots/fields into a list
#' asList(a)
#' asList(b)
#' b$toList()
#' 
#' ##asYAML
#' asYAML(a)
#' asYAML(b)
#' b$toYAML()
#' 
#' ##asJSON
#' asJSON(a)
#' asJSON(b)
#' b$toJSON()
setGeneric("asList", function(object, ...) standardGeneric("asList"))

#' @rdname as-methods
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

#' @rdname as-methods
#' @aliases asList,CWL-method
setMethod("asList", "CWL", function(object, ...){
    object$toList(...)
})

#' @rdname as-methods
#' @aliases asList,SimpleList-method
setMethod("asList", "SimpleList", function(object, ...){
    if(length(object)){
        res <- lapply(object, asList)         
    }else{
        res <- ""
    }
    res
})

#' @docType methods
#' @export asYAML
#' @rdname as-methods
#' @aliases asYAML
setGeneric("asYAML", function(object, ...) standardGeneric("asYAML"))


#' @rdname as-methods
#' @aliases asYAML,ANY-method
setMethod("asYAML", "ANY", function(object, ...){
    as.yaml(asList(object), ...)
})

#' @docType methods
#' @export asJSON
#' @aliases asJSON
#' @rdname as-methods
setGeneric("asJSON", function(object, ...) standardGeneric("asJSON"))

#' @rdname as-methods
#' @aliases asJSON,ANY-method
setMethod("asJSON", "ANY", function(object, ...){
    jsonlite::toJSON(asList(object), ...)
})



