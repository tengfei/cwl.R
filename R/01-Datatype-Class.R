########################################################################
## Datatype
########################################################################
.CWL.Primitive <- c("null",  # no value
                "boolean", # a binary value
                "int", # 32-bit signed integer
                "long", # 64-bit signed integer
                "float", # single precision (32-bit) 
                "double", # double precision (64-bit)
                "bytes", # sequence of uninterpreted 8-bit unsigned bytes
                "string") # unicode character sequence

.CWL.Complex <- c("record", # An object with one or more fields defined by name and type
              "enum", # A value from a finite set of symbolic values
              "array", # An ordered sequence of values
              "map") # An unordered collection of key/value pairs



#' PrimitiveEnum
#'
#' Please check \code{cwl:::.CWL.Pritimive}
#'
#' @importFrom objectProperties setSingleEnum
#' @importClassesFrom S4Vectors SimpleList List
#' 
#' @export PrimitiveEnum
PrimitiveEnum <- setSingleEnum("Primitive" , levels = .CWL.Primitive)

#' ComplexEnum
#'
#' Please check \code{cwl:::.CWL.Complex}
#'
#' @export ComplexEnum
ComplexEnum <- setSingleEnum("Complex" , levels = .CWL.Primitive)

#' DatatypeEnum
#'
#' Primitive + Complex + File
#'
#' @export DatatypeEnum
DatatypeEnum <- setSingleEnum("Datatype",
                              levels = c(.CWL.Primitive, .CWL.Complex, "file"))




########################################################################
## Data Type
########################################################################

#' FileList Class
#'
#' @aliases FileList-class
#' @param \dots element or list of the element.
#' 
#' @export FileList
#' @exportClass FileList
FileList <- setListClass("File")

#' File Class 
#'
#' @field path (character) The path to the file.
#' @field checksum [character] Optional hash code for validating file
#' integrity. Currently must be in the form "sha1$ + hexidecimal
#' string" using the SHA-1 algorithm.
#' @field size [numeric] Optional file size.
#' @field secondaryFile [FileList] A list of additional files that are
#' associated with the primary file and must be transferred alongside
#' the primary file. Examples include indexes of the primary file, or
#' external references which must be included when loading primary
#' document. A file object listed in secondaryFiles may itself include
#' secondaryFiles for which the same rules apply.
#'
#' @return File class generator
#'
#' @export File
#' @exportClass File
#'
#' @examples
#' f1 <- new("File")
#' f2 <- File()
#' FileList(f1, f2)
File <- setRefClass("File",
            fields = list(
                path = "character",
                checksum = "character",
                size = "integer",
                secondaryFile = "FileList" 
            ))

## Class ANY: exists
