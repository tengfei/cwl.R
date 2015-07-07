########################################################################
## Process
########################################################################


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
SchemaList <- setListClass("Schema")

#' Schema Class
#'
#' A schema defines a parameter type.
#'
#' @field type (ANY) The data type of this parameter.
#' 
#' @field fileds [SchemaList] When type is record, defines the fields of the
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
Schema <- setRefClass("Schema",
                      fields = list(
                          type = "ANY", # fixme: Datatype | Schema | string
                          fileds = "SchemaList",
                          symbols = "character",
                          items = "ANY",
                          values = "ANY"
                      ))

#' SchemaDef Class
#'
#' @export SchemaDef
#' @exportClass SchemaDef
SchemaDef <- setRefClass("SchemaDef", contains = "Schema",
                         fields = list(
                             name = "character"
                         ))

#' SchemaDefList
#' 
#' @aliases SchemaDefList-class
#'
#' @param \dots element or list of the element.
#'
#'
#' @export SchemaDefList
#' @exportClass SchemaDefList 
SchemaDefList <- setListClass("SchemaDef")

setRefClass("SchemaDefRequirement", contains = "ProcessRequirement",
            fields = list(
                types = "SchemaDefList"
            ))


##----------------------------------------------------------------------
## Binding
##----------------------------------------------------------------------

## 
## setListClass("characterORExpression")

#' Binding
#'
#' @field loadContents [logical] Only applies when type is File. Read
#' up to the first 64 KiB of text from the file and place it in the
#' "contents" field of the file object for manipulation by
#' expressions.
#'
#' @field secondaryFiles [] Only applies when type is File. Describes
#' files that must be included alongside the primary file. If the
#' value is Expression, the context of the expression is the input or
#' output File parameter to which this binding applies. Where the
#' value is a string, it specifies that the following pattern should
#' be applied to the primary file: If string begins with one or more
#' caret characters, for each caret, remove the last file extension
#' from the path (the last period . and all following characters). If
#' there are no file extensions, the path is unchanged.  Append the
#' remainder of the string to the end of the file path.
#'
#' @export Binding
#' @exportClass Binding
Binding <- setRefClass("Binding",
                       fields = list(
                           loadContents = "logical",
                           secondaryFiles = "characterORExpression" ## fixme: should be a list
                       ))


##======================================================================
## Parameter
##======================================================================


#' Paramter class (reference class)
#'
#' Define an input or output parameter to a process.
#'
#' @field type [ANY] Specify valid types of data that may be assigned
#' to this parameter.
#' 
#' @field label [character] A short, human-readable label of this
#' parameter object.
#' 
#' @field description [character] A long, human-readable description
#' of this parameter object.
#' 
#' @field streamable [logical] Currently only applies if type is
#' File. A value of true indicates that the file is read or written
#' sequentially without seeking. An implementation may use this flag
#' to indicate whether it is valid to stream file contents using a
#' named pipe. Default: false.
#' 
#' @field default [ANY] The default value for this parameter if not
#' provided in the input object.
#'
#' @export Parameter
#' @exportClass Parameter
#'
#' @examples
#' Parameter(type = "integer", label = "thread",
#'          description = "Specify the thread #",
#'          default = 0)
Parameter <- setRefClass("Parameter",
                         fields = list(
                             type = "ANY", # fixme
                             label = "character",
                             description = "character",
                             streamable = "logical",
                             default = "ANY"
                         ),
                         methods = list(
                             initialize <- function(..., stream = FALSE){
                                 stream <<- stream
                                 callSuper(...)
                             }
                         ))

#' InputParameterList
#' 
#' @aliases InputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#'
#' @export InputParameterList
#' @exportClass InputParameterList
InputParameterList <- setListClass("InputParameter")

#' OutputParameterList
#' 
#' @aliases OutputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#'
#' @export OutputParameterList
#' @exportClass OutputParameterList
OutputParameterList <- setListClass("OutputParameter")

#' ProcessRequirementList
#' 
#' @aliases ProcessRequirementList-class
#'
#' @param \dots element or list of the element.
#'
#' @export ProcessRequirementList
#' @exportClass ProcessRequirementList
ProcessRequirementList <- setListClass("ProcessRequirement")

#' Process Class
#'
#' The base executable type in CWL is the Process object defined by
#' the document. Note that the Process object is abstract and cannot
#' be directly executed.
#'
#' @field id [character] The unique identifier for this process
#' object.
#'
#' @field inputs (InputParameterList) Defines the input parameters of
#' the process. The process is ready to run when all required input
#' parameters are associated with concrete values. Input parameters
#' include a schema for each parameter and is used to validate the
#' input object, it may also be used build a user interface for
#' constructing the input object.
#'
#' @field outputs (OutputParameterList) Defines the parameters
#' representing the output of the process. May be used to generate
#' and/or validate the output object.
#'
#' @field requirements [ProcessRequirementList] Declares requirements
#' that apply to either the runtime environment or the workflow engine
#' that must be met in order to execute this process. If an
#' implementation cannot satisfy all requirements, or a requirement is
#' listed which is not recognized by the implementation, it is a fatal
#' error and the implementation must not attempt to run the process,
#' unless overridden at user option.
#'
#' @field hints [ANY] Declares hints applying to either the runtime
#' environment or the workflow engine that may be helpful in executing
#' this process. It is not an error if an implementation cannot
#' satisfy all hints, however the implementation may report a warning.
#'
#' @field label [character] A short, human-readable label of this
#' process object.
#'
#' @field description [character] A long, human-readable description
#' of this process object.
#'
#' @export Process
#' @exportClass Process
Process <- setRefClass("Process",
                       fields = list(
                           id = "character",
                           inputs = "InputParameterList",
                           outputs = "OutputParameterList",
                           requirements = "ProcessRequirementList", 
                           hints = "ANY", 
                           label = "character",
                           description = "character"
                       ))


#' InputSchema Class
#'
#' @field inputBinding [Binding] Describes how to handle a value in
#' the input object convert it into a concrete form for execution,
#' such as command line parameters.
#'
#' @export InputSchema
#' @exportClass InputSchema
InputSchema <- setRefClass("InputSchema", contains = "Schema", 
                           fields = list(
                               inputBinding = "Binding"
                           ))

#' OutputSchema Class
#'
#' @export OutputSchema
#' @exportClass OutputSchema
OutputSchema <- setRefClass("OutputSchema", contains = "Schema")


#' InputParameter Class
#'
#' @field id (character) The unique identifier for this parameter object.
#' 
#' @field inputBinding [Binding] Describes how to handle the inputs of
#' a process and convert them into a concrete form for execution, such
#' as command line parameters.
#'
#' @export InputParameter
#' @exportClass InputParameter 
InputParameter <- setRefClass("InputParameter", contains = "Parameter",
                              fields = list(
                                  id = "character",
                                  inputBinding = "Binding"
                              ))

#' OutputParameter Class
#'
#' @field id (character) The unique identifier for this parameter object.
#'
#' @export OutputParameter
#' @exportClass OutputParameter
OutputParameter <- setRefClass("OutputParameter", contains = "Parameter",
                               fields = list(
                                   id = "character"
                               ))


