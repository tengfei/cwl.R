#' DSC list
#'
#' Contains DataypeSingleEnum, Schema, character
#'
#' @param \dots element or list of the element.
#' @export DSCList
#' @exportClass DSCList
#'
#' @return a DSCList
#' @aliases DSCList DSCList-class
#'
#' @examples
#' DSCList("test", DatatypeEnum(), Schema())
DSCList <- setListClass("DSC")

#' @rdname as-methods
#' @aliases asList,DSCList-method
setMethod("asList", "DSCList", function(object, ...){
    if(length(object)){
        res <- lapply(object, asList)         
    }else{
        res <- list()
    }
    res <- rapply(res, function(x){
        class(x) <- c(class(x), "DSCList")
        x
    }, how = "replace")
    ## hack
    
    if(length(res) == 1 || all(sapply(res, is.character)))
        res <- unlist(res)
    class(res) <- c(class(res), "DSCList")
    res
})


##----------------------------------------------------------------------
## Schema
##----------------------------------------------------------------------

#' SchemaList
#' 
#' @aliases SchemaList-class
#'
#' @param \dots element or list of the element.
#'
#' @export SchemaList
#' @exportClass SchemaList
#' @rdname Schema
#' @aliases ScehmaList
SchemaList <- setListClass("Schema")

#' Schema Class
#'
#' A schema defines a parameter type.
#'
#' @field type (ANY) The data type of this parameter.
#' 
#' @field fields [SchemaList] When type is record, defines the fields of the
#' record.
#' 
#' @field symbols [character] When type is enum, defines the set of valid symbols.
#' 
#' @field items [ANY] When type is array, defines the type of the array
#' elements.
#' 
#' @field values [ANY] When type is map, defines the value type for the
#' key/value pairs.
#'
#' @export Schema
#' @exportClass Schema
#' @rdname Schema
#' @aliases Schema
#' @examples
#' Schema(fields = SchemaList(SchemaDef(name = "schema")))
Schema <- setRefClass("Schema",  contains = "CWL", 
                      fields = list(
                          type = "DSCList",
                          fields = "SchemaList",
                          symbols = "character",
                          items = "ANY",
                          values = "ANY"
                      ),
                      methods = list(
                          initialize = function(type = "", ...){
                              if(is(type, "DSCList")){
                                  type <<- type
                              }else{
                                  if(is.character(type)){
                                      .type <- deType(type)
                                  }else{
                                      .type <- type
                                  }
                                  type <<- DSCList(.type)
                              }
                              callSuper(...)
                          }
))

#' SchemaDef Class
#'
#' @export SchemaDef
#' @exportClass SchemaDef
#' @rdname Schema
#' @aliases SchemaDef
SchemaDef <- setRefClass("SchemaDef", contains = "Schema",
                         fields = list(
                             name = "character"
                         ))



