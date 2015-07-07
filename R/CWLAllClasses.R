## Referecence/Credit:
## http://common-workflow-language.github.io/draft-2

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


## A <- setClass("A")
## B <- setClass("B", contains = "A")
## AList <- setListClass("A")
## BList <- setListClass("B")
## AList(A(), B())



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


########################################################################
## Expression
########################################################################
setClass("JsonPointer", contains = "VIRTUAL")
setClassUnion("JsonPointerORcharacter", c("JsonPointer", "character"))

#' Expression Class
#'
#' Define an expression that will be evaluated and used to modify the
#' behavior of a tool or workflow. See Expressions for more
#' information about expressions and ExpressionEngineRequirement for
#' information on how to define a expression engine.
#'
#' @field engine (JsonPointerORcharacter) Either cwl:JsonPointer or a
#' reference to an ExpressionEngineRequirement defining which engine
#' to use.
#' @field script (character) The code to be executed by the expression
#' engine.
#'
#' @export Expression
#' @exportClass Expression
Expression <- setRefClass("Expression", fields = list(
                              engine = "JsonPointerORcharacter",
                              script = "character"
))


########################################################################
## ProcessRequirement
########################################################################

#' ProcessRequiremen Class
#'
#' A process requirement declares a prerequisite that may or must be
#' fufilled before executing a process. See Process.hints and
#' Process.requirements. Process requirements are the primary
#' mechanism for specifying extensions to the CWL core specification.
#'
#' @export ProcessRequirement
#' @exportClass ProcessRequirement
ProcessRequirement <- setRefClass("ProcessRequirement", contains = "VIRTUAL")

#' DockerRequirement Class
#'
#' Indicates that a workflow component should be run in a Docker
#' container, and specifies how to fetch or build the image.
#'
#' If a CommandLineTool lists DockerRequirement under hints or
#' requirements, it may (or must) be run in the specified Docker
#' container. The platform must first acquire or install the correct
#' Docker image, as described by DockerRequirement. The platform must
#' execute the tool in the container using docker run with the
#' appropriate Docker image and the tool command line. The workflow
#' platform may provide input files and the designated output
#' directory through the use of volume bind mounts. The platform may
#' rewrite file paths in the input object to correspond to the Docker
#' bind mounted locations. When running a tool contained in Docker,
#' the workflow platform must not assume anything about the contents
#' of the Docker container,such as the presence or absence of specific
#' software, except to assume that the generated command line
#' represents a valid command within the runtime environment of the
#' container.
#'
#' @field dockerPull [character] Get a Docker image using docker pull.
#' 
#' @field dockerLoad [character] Specify a HTTP URL from which to
#' download a Docker image using docker load.
#' 
#' @field dockerFile [character] Supply the contents of a Dockerfile
#' which will be build using docker build.
#' 
#' @field dockerImageId [character] The image id that will be used for
#' docker run. May be a human-readable image name or the image
#' identifier hash. May be skipped if dockerPull is specified, in
#' which case the dockerPull image id will be used.
#' 
#' @field dockerOutputDirectory [character] Set the designated output
#' directory to a specific location inside the Docker container.
#'
#' @export DockerRequirement
#' @exportClass DockerRequirement
DockerRequirement <- setRefClass("DockerRequirement",
                                 contains = "ProcessRequirement",
                                 fields = list(
                                     dockerPull = "character",
                                     dockerLoad = "character",
                                     dockerFile = "character",
                                     dockerImageId = "character",
                                     dockerOutputDirectory = "character"
                                 ))

#' SubworkflowFeatureRequirement Class
#'
#' Indicates that the workflow platform must support nested workflows
#' in the run field of (WorkflowStep)(#workflowstep).
#'
#' @export SubworkflowFeatureRequirement 
#' @exportClass SubworkflowFeatureRequirement
SubworkflowFeatureRequirement <-
    setRefClass("SubworkflowFeatureRequirement", contains = "ProcessRequirement")


setClassUnion("characterORExpression", c("character", "Expression"))


#' FileDef Class
#'
#' Define a file that must be placed by in the designated output
#' directory prior to executing the command line tool. May be the
#' result of executing an expression, such as building a configuration
#' file from a template.
#'
#' @field filename (characterORExpression) The name of the file to
#' create in the output directory.
#' 
#' @field fileContent (characterORExpression) If the value is a string
#' literal or an expression which evalutes to a string, a new file
#' must be created with the string as the file contents. If the value
#' is an expression that evaluates to a File object, this indicates
#' the referenced file should be added to the designated output
#' directory prior to executing the tool. Files added in this way may
#' be read-only, and may be implemented through bind mounts or file
#' system links in such a way as to avoid unecessary copying of the
#' input file.
#'
#' @export FileDef
#' @exportClass FileDef
FileDef <- setRefClass("FileDef", fields = list(
                                      filename = "characterORExpression",
                                      fileContent = "characterORExpression"
))

#' FileDefList
#'
#' @aliases FileDefList-class
#'
#' @param \dots element or list of the element.
#'
#' @export FileDefList
#' @exportClass FileDefList
FileDefList <- setListClass("FileDef")

#' CreateFileRequirement Class
#'
#' Define a list of files that must be created and placed by the
#' workflow platform in the designated output directory prior to
#' executing the command line tool. See FileDef for details.
#'
#' @field fileDef (FileDefList) The list of files.
#'
#' @export CreateFileRequirement
#' @exportClass CreateFileRequirement
CreateFileRequirement <-
    setRefClass("CreateFileRequirement", contains = "ProcessRequirement",
                fields = list(
                    fileDef = "FileDefList"
                ))

#' EnvironmentDef Class
#'
#' Define an environment variable that will be set in the runtime
#' environment by the workflow platform when executing the command
#' line tool. May be the result of executing an expression, such as
#' getting a parameter from input.
#' 
#' @field envName (character) The environment variable name. 
#' 
#' @field envValue (characterORExpression) The environment variable value.
#' 
#' @export EnvironmentDef
#' @exportClass EnvironmentDef
EnvironmentDef <- setRefClass("EnvironmentDef",
                              fields = list(
                                  envName = "character",
                                  envValue = "characterORExpression"
                              ))


#' EnvironmentDefList
#' 
#' @aliases EnvironmentDefList-class
#'
#' @param \dots element or list of the element.
#'
#' @export EnvironmentDefList
#' @exportClass EnvironmentDefList
EnvironmentDefList <- setListClass("EnvironmentDef")

#' EnvVarRequirement Class
#'
#' Define a list of environment variables which will be set in the
#' execution environment of the tool. See EnvironmentDef for details.
#'
#' @field envDef (EnvironmentDefList) The list of environment
#' variables.
#'
#' @export EnvVarRequirement
#' @exportClass EnvVarRequirement
EnvVarRequirement <-
    setRefClass("EnvVarRequirement", contains = "ProcessRequirement",
                fields = list(
                    envDef = "EnvironmentDefList"
                ))

#' ScatterFeatureRequirement Class
#'
#' Indicates that the workflow platform must support the scatter and
#' scatterMethod fields of (WorkflowStep)(#workflowstep).
#' 
#' @export ScatterFeatureRequirement
#' @exportClass ScatterFeatureRequirement
ScatterFeatureRequirement <-
    setRefClass("ScatterFeatureRequirement", contains = "ProcessRequirement")


#' ExpressionEngineRequirement Class
#'
#' Define an expression engine, as described in Expressions.
#'
#' @field id (character) Used to identify the expression engine in the
#' engine field of Expressions.
#' 
#' @field requirements [ProcessRequirement]Requirements to run this
#' expression engine, such as DockerRequirement for specifying a
#' container with the engine.
#' 
#' @field engineCommand [character] The command line to invoke the
#' expression engine.
#' 
#' @field engineConfig [character] Additional configuration or code
#' fragments that will also be passed to the expression engine. The
#' semantics of this field are defined by the underlying expression
#' engine. Intended for uses such as providing function definitions
#' that will be called from CWL expressions.
#'
#' @export  ExpressionEngineRequirement
#' @exportClass ExpressionEngineRequirement
ExpressionEngineRequirement <-
    setRefClass("ExpressionEngineRequirement", contains = "ProcessRequirement",
                fields = list(
                    id = "character",
                    requirements = "ProcessRequirement",
                    engineCommand = "character",
                    engineConfig = "character"
                ))



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


########################################################################
## ExpressionTool (fixme)
########################################################################

#' ExpressionTool Class
#'
#' Execute an expression as a process step.
#'
#' @field expression (Expression) The expression to execute. The
#' expression must return a JSON object which matches the output
#' parameters of the ExpressionTool.
#'
#' @export ExpressionTool
#' @exportClass ExpressionTool
ExpressionTool <- setRefClass("ExpressionTool", contains = "Process",
                              fields = list(
                                  expression = "Expression"
                              ))


########################################################################
## CommandLineTool
########################################################################

#' CommandLineBinding Class
#'
#' When listed under inputBinding in the input schema, the term
#' "value" refers to the the corresponding value in the input
#' object. For binding objects listed in CommandLineTool.arguments,
#' the term "value" refers to the effective value after evaluating
#' valueFrom.
#'
#' @details The binding behavior when building the command line
#' depends on the data type of the value. If there is a mismatch
#' between the type described by the input schema and the effective
#' value, such as resulting from an expression evaluation, an
#' implementation must use the data type of the effective value.
#' \itemize{
#'   \item{character}{Add prefix and the string to the command line.}
#'   \item{numeric}{Add prefix and decimal representation to command line.}
#'   \item{logical}{If true, add prefix to the command line. If false,
#' add nothing.}
#'   \item{File}{Add prefix and the value of File.path to the command line.}
#'   \item{*Array}{If itemSeparator is specified, add prefix and the join the array into a single string with itemSeparator separating the items. Otherwise add prefix and recursively add individual elements.}
#' \item{*object}{Add prefix only, and recursively add object fields for which inputBinding is specified.}
#'  \item{null}{Add nothing.}
#' }
#'
#' @field position [integer] The sorting key. Default position is 0.
#'
#' @field prefix [character] Command line prefix to add before the value.
#'
#' @field separate [logical] If true (default) then the prefix and
#' value must be added as separate command line arguments; if false,
#' prefix and value must be concatenated into a single command line
#' argument.
#'
#' @field itemSeparator [character] Join the array elements into a
#' single string with the elements separated by by itemSeparator.
#'
#' @field valueFrom [characterOrExpression] If valueFrom is a constant
#' string value, use this as the value and apply the binding rules
#' above. If valueFrom is an expression, evaluate the expression to
#' yield the actual value to use to build the command line and apply
#' the binding rules above. If the inputBinding is associated with an
#' input parameter, the "context" of the expression will be the value
#' of the input parameter. When a binding is part of the
#' CommandLineTool.arguments field, the valueFrom field is required.
#'
#' @export CommandLineBinding
#' @exportClass CommandLineBinding
CommandLineBinding <- setRefClass("CommandLineBinding", contains = "Binding",
                                  fields = list(
                                      position = "integer",
                                      prefix = "character",
                                      separate = "logical",
                                      itemSeparator = "character",
                                      valueFrom = "characterORExpression"
                                  ))


setClassUnion("characterORCommandLineBinding",
              c("character", "CommandLineBinding"))

## fixme:
setListClass("characterORCommandLineBinding")

#' CommandLineTool Class
#'
#' A CommandLineTool process is a process implementation for executing
#' a non-interactive application in a POSIX environment. To help
#' accomodate of the enormous variety in syntax and semantics for
#' input, runtime environment, invocation, and output of arbitrary
#' programs, CommandLineTool provides the concept of "input binding"
#' to describe how to translate input parameters to an actual program
#' invocation, and "output binding" to describe how generate output
#' parameters from program output.
#'
#' @section Input binding:
#'
#' The tool command line is built by applying command line bindings to
#' the input object. Bindings are listed either as part of an input
#' parameter using the inputBinding field, or separately using the
#' arguments field of the CommandLineTool.
#'
#' The algorithm to build the command line is as follows. In this
#' algorithm, the sort key is a list consisting of one or more numeric
#' and string elements. Strings are sorted lexicographically based on
#' UTF-8 encoding.
#'
#' \itemize{
#'
#' \item{}{Collect CommandLineBinding objects from arguments. Assign a
#' sorting key [position, i] where position is
#' CommandLineBinding.position and the i is the index in the arguments
#' list.}
#'
#' \item{}{Collect CommandLineBinding objects from the inputs schema
#' and associate them with values from the input object. Where the
#' input type is a record, array, or map, recursively walk the schema
#' and input object, collecting nested CommandLineBinding objects and
#' associating them with values from the input object.  }
#'
#' \item{}{Assign a sorting key for each leaf binding object by appending
#' nested position fields together with the array index, or map key of
#' the data at each nesting level. If two bindings have the same
#' position, the tie must be broken using the lexographic ordering of
#' the field or parameter name immediately containing the binding.}
#'
#' \item{}{Sort elements using the assigned sorting keys. Numeric
#' entries sort before strings.}
#'
#' \item{}{In the sorted order, apply the rules defined in
#' CommandLineBinding to convert bindings to actual command line
#' elements.}
#'
#' \item{}{Insert elements from baseCommand at the beginning of the
#' command line.}
#'
#' }
#' 
#' @section Runtime environment:
#'
#' All files listed in the input object must be made available in the
#' runtime environment. The implementation may use a shared or
#' distributed file system or transfer files via explicit
#' download. Implementations may choose not to provide access to files
#' not explicitly specified by the input object or process
#' requirements.
#'
#' Output files produced by tool execution must be written to the
#' designated output directory.
#'
#' The initial current working directory when executing the tool must
#' be the designated output directory.
#'
#' The TMPDIR environment variable must be set in the runtime
#' environment to the designated temporary directory. Any files
#' written to the designated temporary directory may be deleted by the
#' workflow platform when the tool invocation is complete.
#'
#' An implementation may forbid the tool from writing to any location
#' in the runtime environment file system other than the designated
#' temporary directory and designated output directory. An
#' implementation may provide read-only input files, and disallow
#' in-place update of input files.
#'
#' The standard input stream and standard output stream may be
#' redirected as described in the stdin and stdout fields. 
#' 
#' @section Extensions:
#'
#' DockerRequirement, CreateFileRequirement, and EnvVarRequirement,
#' are available as standard extensions to core command line tool
#' semantics for defining the runtime environment.  
#'
#' @section Execution:
#'
#' Once the command line is built and the runtime environment is
#' created, the actual tool is executed.
#'
#' The standard error stream and standard output stream (unless
#' redirected by setting stdout) may be captured by platform logging
#' facilities for storage and reporting.
#'
#' Tools may be multithreaded or spawn child processes; however, when
#' the parent process exits, the tool is considered finished
#' regardless of whether any detached child processes are still
#' running. Tools must not require any kind of console, GUI, or web
#' based user interaction in order to start and run to completion.
#'
#' The exit code of the process indicates if the process completed
#' successfully. By convention, an exit code of zero is treated as
#' success and non-zero exit codes are treated as failure. This may be
#' customized by providing the fields successCodes,
#' temporaryFailCodes, and permanentFailCodes. An implementation may
#' choose to default unspecified non-zero exit codes to either
#' temporaryFailure or permanentFailure. 
#'
#' @section Output binding:
#' 
#' If the output directory contains a file called "cwl.output.json",
#' that file must be loaded and used as the output object. Otherwise,
#' the output object must be generated by walking the parameters
#' listed in outputs and applying output bindings to the tool
#' output. Output bindings are associated with output parameters using
#' the outputBinding field. See CommandOutputBinding for details.
#' 
#'
#' @field baseCommand (character) Specifies the program to execute. If
#' the value is an array, the first element is the program to execute,
#' and subsequent elements are placed at the beginning of the command
#' line in prior to any command line bindings. If the program includes
#' a path separator character it must be an absolute path, otherwise
#' it is an error. If the program does not include a path separator,
#' search the $PATH variable in the runtime environment find the
#' absolute path of the executable.
#'
#' @field arguments [characterORCommandLineBinding] Command line
#' bindings which are not directly associated with input parameters.
#'
#' @field stdin [characterORExpression] A path to a file whose
#' contents must be piped into the command's standard input stream.
#'
#' @field stdout [characterORExpression] Capture the command's
#' standard output stream to a file written to the designated output
#' directory. If stdout is a string, it specifies the file name to
#' use.If stdout is an expression, the expression is evaluated and
#' must return a string with the file name to use to capture
#' stdout. If the return value is not a string, or the resulting path
#' contains illegal characters (such as the path separator /) it is an
#' error.
#'
#' @field successCodes [integer] Exit codes that indicate the process
#' completed successfully.
#'
#' @field temporaryFailCodes [integer] Exit codes that indicate the
#' process failed due to a possibly temporary condition, where
#' excuting the process with the same runtime environment and inputs
#' may produce different results.
#'
#' @field permanentFailCodes [integer] Exit codes that indicate the
#' process failed due to a permanent logic error, where excuting the
#' process with the same runtime environment and same inputs is
#' expected to always fail.
#'
#' @export CommandLineTool
#' @exportClass CommandLineTool 
CommandLineTool <- setRefClass("CommandLineTool", contains = "Process",
                               fields = list(
                                   baseCommand = "character",
                                   arguments = "characterORCommandLineBindingList",
                                   stdin = "characterORExpression",
                                   stdout = "characterORExpression",
                                   successCodes = "integer",
                                   temporaryFailCodes = "integer",
                                   permanentFailCodes = "integer"
                               ))

#' CommandInputParameter Class
#'
#' An input parameter for a CommandLineTool.
#'
#' @export CommandInputParameter
#' @exportClass CommandInputParameter
CommandInputParameter <-
    setRefClass("CommandInputParameter", contains = "InputParameter")


#' CommandInputSchema Class
#'
#' @export CommandInputSchema
#' @exportClass CommandInputSchema
CommandInputSchema <-
    setRefClass("CommandInputSchema", contains = "InputSchema")



#' CommandOutputBinding Class
#'
#' Describes how to generate an output parameter based on the files produced by a CommandLineTool. The output parameter is generated by applying these operations in the following order: glob, loadContents, outputEval
#'
#' @field glob [characterORExpression] Find files relative to the
#' output directory, using POSIX glob(3) pathname matching. If
#' provided an array, match all patterns in the array. If provided an
#' expression, the expression must return a string or an array of
#' strings, which will then be evaluated as a glob pattern. Only files
#' which actually exist will be matched and returned.
#'
#' @field outputEval [Expression] Evaluate an expression to generate
#' the output value. If glob was specified, the script context will be
#' an array containing any files that were matched. Additionally, if
#' loadContents is true, the file objects will include up to the first
#' 64 KiB of file contents in the contents field.
#'
#' @export CommandOutputBinding
#' @exportClass CommandOutputBinding
CommandOutputBinding <-
    setRefClass("CommandOutputBinding", contains = "Binding",
                fields = list(
                    glob = "characterORExpression",
                    outputEval = "Expression"
                ))

#' CommandOutputSchema
#'
#' @field outputBinding [CommandOutputBinding] Describes how to handle
#' the concrete outputs of a process step (such as files created by a
#' program) and describe them in the process output parameter.
#'
#' @export CommandOutputSchema
#' @exportClass CommandOutputSchema
CommandOutputSchema <-
    setRefClass("CommandOutputSchema", contains = "Schema", 
                fields = list(
                    outputBinding = "CommandOutputBinding"
                ))


#' CommandOutputParameter Class
#'
#' @field outputBinding [CommandOutputBinding] Describes how to handle
#' the concrete outputs of a process step (such as files created by a
#' program) and describe them in the process output parameter.
#'
#' @export CommandOutputParameter
#' @exportClass CommandOutputParameter
CommandOutputParameter <-
    setRefClass("CommandOutputParameter", contains = "OutputParameter",
                fields = list(
                    outputBinding = "CommandOutputBinding"
                ))


########################################################################
## Workflow
########################################################################



setClass("LinkMergeMethod", contains = "VIRTUAL")

#' WorkflowStepInput Class
#'
#'
#' The input of a workflow step connects an upstream parameter (from
#' the workflow inputs, or the outputs of other workflows steps) with
#' the input parameters of the underlying process.
#'
#' @section details:
#'
#' If the sink parameter is an array, or named in a workflow scatter
#' operation, there may be multiple inbound data links listed in the
#' connect field. The values from the input links are merged depending
#' on the method specified in the linkMerge field. If not specified,
#' the default method is merge_nested:
#'
#' \itemize{
#'
#' \item{merge_nested}{ The input shall be an array consisting of
#' exactly one entry for each input link. If merge_nested is specified
#' with a single link, the value from the link is wrapped in a
#' single-item list.  }
#'
#' \item{merge_flattened}{ 1) The source and sink parameters must be
#' compatible types, or the source type must be compatible with single
#' element from the "items" type of the destination array
#' parameter. 2) Source parameters which are arrays are concatenated;
#' source parameters which are single element types are appended as
#' single elements.  } }
#'
#'
#' @field id (character) A unique identifier for this workflow input
#' parameter.
#'
#' @field source [character] Specifies one or more workflow parameters
#' that will provide input to the underlying process parameter.
#'
#' @field linkMerge [LineMergeMethod] The method to use to merge
#' multiple inbound links into a single array. If not specified, the
#' default method is merge_nested:
#'
#' @field default [ANY] The default value for this parameter if there
#' is no source field.
#' 
#' @export WorkflowStepInput
#' @exportClass WorkflowStepInput
WorkflowStepInput <- setRefClass("WorkflowStepInput",
                                 fields = list(
                                     id = "character",
                                     source = "character",
                                     linkMerge = "LinkMergeMethod",
                                     default = "ANY"
                                 ))

#' WorkflowStepOutput Class
#'
#' Associate an output parameter of the underlying process with a
#' workflow parameter. The workflow parameter (given in the id field)
#' be may be used as a source to connect with input parameters of
#' other workflow steps, or with an output parameter of the process.
#'
#' @field id (character) A unique identifier for this workflow output
#' parameter. This is the identifier to use in the source field of
#' WorkflowStepInput to connect the output value to downstream
#' parameters.
#'
#' @export WorkflowStepOutput
#' @exportClass WorkflowStepOutput
WorkflowStepOutput <- setRefClass("WorkflowStepOutput",
                                  fields = list(
                                      id = "character"
                                  ))

#' WorkflowStepInputList
#' 
#' @aliases WorkflowStepInputList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowStepInputList
#' @exportClass WorkflowStepInputList
WorkflowStepInputList <- setListClass("WorkflowStepInput")

#' WorkflowStepOutputList
#' 
#' @aliases WorkflowStepOutputList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowStepOutputList
#' @exportClass WorkflowStepOutputList
WorkflowStepOutputList <- setListClass("WorkflowStepOutput")

#' WorkflowStepList
#' 
#' @aliases WorkflowStepList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowStepList
#' @exportClass WorkflowStepList
WorkflowStepList <- setListClass("WorkflowStep")


#' WorkflowOutputParameter Class
#'
#' Describe an output parameter of a workflow. The parameter must be
#' connected to one or more parameters defined in the workflow that
#' will provide the value of the output parameter.
#'
#' @field source [character] Specifies one or more workflow parameters
#' that will provide this output value.
#'
#' @field linkMerge [LinkMergeMethod] The method to use to merge
#' multiple inbound links into a single array. If not specified, the
#' default method is merge_nested:
#'
#' @export WorkflowOutputParameter
#' @exportClass WorkflowOutputParameter
WorkflowOutputParameter <-
    setRefClass("WorkflowOutputParameter", contains = "OutputParameter",
                fields = list(
                    source = "character",
                    linkMerge = "LinkMergeMethod"
                ))

#' WorkflowOutputParameterList
#'
#' @aliases WorkflowOutputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowOutputParameterList
#' @exportClass WorkflowOutputParameterList
WorkflowOutputParameterList <- setListClass("WorkflowOutputParameter", contains = "OutputParameterList")



#' Workflow
#'
#' A workflow is a process consisting of one or more steps. Each step
#' has input and output parameters defined by the inputs and outputs
#' fields. A workflow executes as described in execution model.
#'
#' @section Dependencies:
#'
#' Dependencies between parameters are expressed using the source
#' field on workflow step input parameters and workflow output
#' parameters.
#'
#' The source field expresses the dependency of one parameter on
#' another such that when a value is associated with the parameter
#' specified by source, that value is propagated to the destination
#' parameter. When all data links inbound to a given step are
#' fufilled, the step is ready to execute.
#'
#' @section Extensions:
#'
#' ScatterFeatureRequirement and SubworkflowFeatureRequirement are
#' available as standard extensions to core workflow semantics.
#'
#' @field outputs (WorkflowOutputParameterList) Defines the parameters
#' representing the output of the process. May be used to generate
#' and/or validate the output object. Inherited from Process
#' 
#' @field steps (WorkflowStepList) The individual steps that make up the
#' workflow. Steps are executed when all input data links are
#' fufilled. An implementation may choose to execute the steps in a
#' different order than listed and/or execute steps concurrently,
#' provided that dependencies between steps are met.
#'
#' @export Workflow
#' @exportClass Workflow
Workflow <-
    setRefClass("Workflow", contains = "Process",
                fields = list(
                    outputs = "WorkflowOutputParameterList", 
                    steps = "WorkflowStepList"
                ))




setClass("ScatterMethod")

setClassUnion("CommandLineToolORExpressionToolORWorkflow",
              c("CommandLineTool", "ExpressionTool", "Workflow"))


#' WorkflowStep Class
#'
#' A workflow step is an executable element of a workflow. It
#' specifies the underlying process implementation (such as
#' CommandLineTool) in the run field and connects the input and output
#' parameters of the underlying process to workflow parameters.
#'
#' @section Scatter/gather:
#'
#' To use scatter/gather, ScatterFeatureRequirement must be specified
#' in the workflow or workflow step requirements.
#'
#' A "scatter" operation specifies that the associated workflow step
#' or subworkflow should execute separately over a list of input
#' elements. Each job making up a scatter operaution is independent
#' and may be executed concurrently.
#'
#' The scatter field specifies one or more input parameters which will
#' be scattered. An input parameter may be listed more than once. The
#' declared type of each input parameter is implicitly wrapped in an
#' array for each time it appears in the scatter field. As a result,
#' upstream parameters which are connected to scattered parameters may
#' be arrays.
#'
#' All output parameters types are also implicitly wrapped in arrays;
#' each job in the scatter results in an entry in the output array.
#'
#' If scatter declares more than one input parameter, scatterMethod
#' describes how to decompose the input into a discrete set of jobs.
#'
#' \itemize{
#' 
#' \item{dotproduct}{ specifies that each the input arrays are aligned
#' and one element taken from each array to construct each job. It is
#' an error if all input arrays are not the same length.}
#'
#' \item{nested_crossproduct}{specifies the cartesian product of the
#' inputs, producing a job for every combination of the scattered
#' inputs. The output must be nested arrays for each level of
#' scattering, in the order that the input arrays are listed in the
#' scatter field.}
#'
#' \item{flat_crossproduct}{specifies the cartesian product of the
#' inputs, producing a job for every combination of the scattered
#' inputs. The output arrays must be flattened to a single level, but
#' otherwise listed in the order that the input arrays are listed in
#' the scatter field.}
#'
#' }
#'
#' @section Subworkflows:
#'
#' To specify a nested workflow as part of a workflow step,
#' SubworkflowFeatureRequirement must be specified in the workflow or
#' workflow step requirements.
#'
#' @field id [character] The unique identifier for this workflow step.
#'
#' @field inputs (WorkflowStepInputList) Defines the input parameters
#' of the workflow step. The process is ready to run when all required
#' input parameters are associated with concrete values. Input
#' parameters include a schema for each parameter and is used to
#' validate the input object, it may also be used build a user
#' interface for constructing the input object.
#'
#' @field outputs (WorkflowStepOutputList) Defines the parameters
#' representing the output of the process. May be used to generate
#' and/or validate the output object.
#'
#' @field requirements [ProcessRequirement] Declares requirements that
#' apply to either the runtime environment or the workflow engine that
#' must be met in order to execute this workflow step. If an
#' implementation cannot satisfy all requirements, or a requirement is
#' listed which is not recognized by the implementation, it is a fatal
#' error and the implementation must not attempt to run the process,
#' unless overridden at user option.
#'
#' @field hints [ANY] Declares hints applying to either the runtime
#' environment or the workflow engine that may be helpful in executing
#' this workflow step. It is not an error if an implementation cannot
#' satisfy all hints, however the implementation may report a warning.
#'
#' @field label [character] A short, human-readable label of this
#' process object.
#'
#' @field description [character] A long, human-readable description
#' of this process object.
#'
#' @field run (CommandLineToolORExpressionToolORWorkflow) Specifies
#' the process to run.
#'
#' @field scatter [character]
#'
#' @field scatterMethod [ScatterMethod] Required if scatter is an array of more
#' than one element.
#'
#' @export WorkflowStep
#' @exportClass WorkflowStep
WorkflowStep <-
    setRefClass("WorkflowStep",
                fields = list(
                    id = "character",
                    inputs = "WorkflowStepInputList",
                    outputs = "WorkflowStepOutputList",
                    requirements = "ProcessRequirement",
                    hints = "ANY", 
                    label = "character",
                    description = "character",
                    run = "CommandLineToolORExpressionToolORWorkflow",
                    scatter = "character",
                    scatterMethod = "ScatterMethod"
                ))
