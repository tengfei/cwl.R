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








