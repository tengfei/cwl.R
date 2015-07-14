########################################################################
## Process
########################################################################
#' SchemaDefList
#' 
#' @aliases SchemaDefList-class
#'
#' @export SchemaDefList
#' @exportClass SchemaDefList
#' @rdname Schema
#' @aliases SchemaDefList
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
#' @rdname Binding
#' @examples
#' Binding(loadContents = TRUE, secondaryFiles = "./test.txt")
Binding <- setRefClass("Binding", contains = "CWL", 
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
#' @rdname Parameter
#'
#' @return Parameter object
#'
#' @examples
#' Parameter(type = "integer", label = "thread",
#'          description = "Specify the thread #",
#'          default = 0)
#' 
#' ipl <- InputParameterList(
#'     InputParameter(id = "BAM", type = "File",
#'                    label = "input bam",
#'                    description = "input bam",
#'                    inputBinding = CommandLineBinding(
#'                        position = 1L
#'                    )),
#'     InputParameter(id = "level", type = "Integer",
#'                    label = "Compression level",
#'                    description = "Compression level",
#'                    inputBinding = CommandLineBinding(
#'                        position = 2L,
#'                        prefix = "-l"
#'                    ))    
#' )
#' ipl
Parameter <- setRefClass("Parameter", contains = "CWL",
                         fields = list(
                             type = "DSCList",
                             label = "character",
                             description = "character",
                             streamable = "logical",
                             default = "ANY"
                         ),
                         methods = list(
                             initialize = function(..., type = "",
                                 streamable = FALSE){
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
                                 streamable <<- streamable
                                 callSuper(...)
                             }
                         ))


#' InputParameterList
#' 
#' @aliases InputParameterList InputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#'
#' @export InputParameterList
#' @exportClass InputParameterList
#' @rdname Parameter
InputParameterList <- setListClass("InputParameter")

#' OutputParameterList
#' 
#' @aliases OutputParameterList OutputParameterList-class
#'
#' @export OutputParameterList
#' @exportClass OutputParameterList
#' @rdname Parameter
OutputParameterList <- setListClass("OutputParameter")

#' ProcessRequirementList
#' 
#' @aliases ProcessRequirementList-class
#'
#' @export ProcessRequirementList
#' @exportClass ProcessRequirementList
#' @rdname ProcessRequirement
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
#'
#' @rdname Process
#' @examples
#' ipl <- InputParameterList(
#'     InputParameter(id = "BAM", type = "File",
#'                    label = "input bam",
#'                    description = "input bam",
#'                    inputBinding = CommandLineBinding(
#'                        position = 1L
#'                    )),
#'     InputParameter(id = "level", type = "Integer",
#'                    label = "Compression level",
#'                    description = "Compression level",
#'                    inputBinding = CommandLineBinding(
#'                        position = 2L,
#'                        prefix = "-l"
#'                    ))    
#' )
#' ipl
#' p <- Process(id = "process", inputs = ipl)
#' p
Process <- setRefClass("Process", contains = "CWL",
                       fields = list(
                           id = "character",
                           inputs = "InputParameterList",
                           outputs = "OutputParameterList",
                           requirements = "ProcessRequirementList", 
                           hints = "ANY", 
                           label = "character",
                           description = "character"
                       ),
                       methods = list(
                           initialize = function(id = "", ...){
                               id <<- addIdNum(id)
                               callSuper(...)
                           }
                       ))


#' InputSchema Class
#'
#' @field inputBinding [Binding] Describes how to handle a value in
#' the input object convert it into a concrete form for execution,
#' such as command line parameters.
#'
#' @export InputSchema
#' @exportClass InputSchema
#' @rdname Schema
#' @aliases InputSchema
#'
#' @return a Schema object or sbuclass object.
InputSchema <- setRefClass("InputSchema", contains = "Schema", 
                           fields = list(
                               inputBinding = "Binding"
                           ))

#' OutputSchema Class
#'
#' @export OutputSchema
#' @exportClass OutputSchema
#' @rdname Schema
#' @aliases OutputSchema
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
#' @rdname Parameter
#' @aliases InputParameter InputParameter-class
InputParameter <- setRefClass("InputParameter", contains = "Parameter",
                              fields = list(
                                  id = "character",
                                  inputBinding = "Binding"
                              ),
                              methods = list(
                                  initialize = function(id = "", ...){
                                      id <<- addIdNum(id)
                                      callSuper(...)
                                  }

                              ))

#' OutputParameter Class
#'
#' @field id (character) The unique identifier for this parameter object.
#'
#' @export OutputParameter
#' @exportClass OutputParameter
#' @rdname Parameter
#' @aliases OutputParameter OutputParameter-class
OutputParameter <- setRefClass("OutputParameter", contains = "Parameter",
                               fields = list(
                                   id = "character"
                               ),
                               methods = list(
                                   initialize = function(id = "", ...){
                                       id <<- addIdNum(id)
                                       callSuper(...)
                                   }

                               ))


