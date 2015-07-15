#' List Class generator.
#'
#' Extends IRanges SimpleList class and return constructor.
#' 
#' @param elementType [character]
#' @param suffix [character] default is "List"
#' @param contains [character] class name.
#' @param where environment.
#' 
#' @return S4 class constructor
setListClass <- function(elementType = NULL, suffix = "List",
                         contains = NULL, where = topenv(parent.frame())){
    stopifnot(is.character(elementType))
    name <- paste0(elementType, suffix)
    setClass(name, contains = c("SimpleList", contains), where = where,
             prototype = prototype(elementType = elementType))
    setMethod("show", name, function(object){
        lapply(object, show)
    })
    ## constructor
    function(...){
        listData <- .dotargsAsList(...)
        S4Vectors:::new_SimpleList_from_list(name, listData)
    }
}



## Function from IRanges
.dotargsAsList <- function(...) {
    listData <- list(...)
  if (length(listData) == 1) {
      arg1 <- listData[[1]]
      if (is.list(arg1) || is(arg1, "List"))
        listData <- arg1
      ## else if (type == "integer" && class(arg1) == "character")
      ##   listData <- strsplitAsListOfIntegerVectors(arg1) # weird special case
  }
  listData
}



deType <- function(x){
    ## string
    str_type <- c('STRING', 'STR', '<string>', '<str>', 'str', "character",
                  "string", "String")
    ## int
    int_type <- c('INTEGER', 'INT', '<integer>', '<int>', 'int',
                  "integer", "Integer")
    ## float
    float_type <- c('FLOAT', '<float>', 'float', 'Float')
    ## File
    file_type <- c('FILE', '<file>', 'File', 'file')
    ## enum
    enum_type <- c('ENUM', '<enum>', 'enum', "Enum")

    res <- ""
    
    if(x %in% str_type){
        res <- "string"
    }

    if(x %in% int_type){
        res <- "int"
    }
    if(x %in% float_type){
        res <- "float"
    }
    if(x %in% file_type){
        res <- "File"
    }
    if(x %in% enum_type){
        res <- "enum"
    }
    
   res
}


#' add \code{#} prefix to id
#'
#' add \code{#} prefix to id
#'
#' @param x (character) with \code{#} or not.
#'
#' @return a character with \code{#} prefix.
#'
#' @export addIdNum
#' @examples
#' addIdNum("bam")
addIdNum <- function(x){
    .first <- substr(x, 1, 1)
    if(.first != "#"){
        return(paste0("#", x))
    }else{
        return(x)
    }
}

## ## How to de-bioc dependencies
## ## to do define a
## ## Acknowledgement to S4 vectors, to reduce the dependency, use simple approach
## ## So far I only need things:
## ## 1. validation of types
## ## 2. lappy



## SList <- setRefClass("SList", 
##                      fields = list(elementType = "character",
##                          listData = "list"))

## setMethod("lapply", "SList",
##           function(X, FUN, ...)
##               lapply(as.list(X), FUN = FUN, ...))

## setMethod("length", "SList", function(x) length(as.list(x)))

## setMethod("names", "SList", function(x) names(as.list(x)))

## setReplaceMethod("names", "SimpleList",
##                  function(x, value) {
##                      names(x@listData) <- value
##                      x
##                  })

## setListClass <- function(elementType = NULL, suffix = "List",
##                          contains = NULL, where = topenv(parent.frame())){
##     stopifnot(is.character(elementType))
##     name <- paste0(elementType, suffix)
##     res <- setRefClass(name, contains = c("SList", contains),
##                        where = where)
##     ## setMethod("show", name, function(object){
##     ##     lapply(object, show)
##     ## })
##     function(...){
##         x <- .dotargsAsList(...)
##         ## validation
##         if (!is.list(x)) 
##             stop("'x' must be a list")
##         if (is.array(x)) {
##             tmp_names <- names(x)
##             dim(x) <- NULL
##             names(x) <- tmp_names
##         }
##         class(x) <- "list"
##         if (!all(sapply(x,
##                         function(xi)
##                             extends(class(xi),
##                                     elementType)))) 
##             stop("all elements in 'x' must be ", elementType, 
##                  " objects")
##         res(elementType = elementType, listData = x)
##     }
## }

## A <- setClass("A", slots = list(x = "character", y = "numeric"))
## a <- A(x = "a", y = 123)

## AList <- setListClass("A")
## AList()$elementType
## lapply(AList(a, a, a), is)


