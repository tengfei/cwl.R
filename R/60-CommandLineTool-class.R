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
#'
#' @examples
#' CommandLineBinding(position = 1L, prefix = "-l")
CommandLineBinding <- setRefClass("CommandLineBinding",
                                  contains = c("Binding", "CWL"),
                                  fields = list(
                                      position = "integer",
                                      prefix = "character",
                                      separate = "logical",
                                      itemSeparator = "character",
                                      valueFrom = "characterORExpression"
                                  ),
                                  methods = list(
                                      initialize = function(
                                          position = 0L,
                                          separate = TRUE,
                                          ...){
                                          
                                          position <<- position
                                          separate <<- separate
                                          callSuper(...)
                                      }
                                  ))


setClassUnion("characterORCommandLineBinding",
              c("character", "CommandLineBinding"))

#' characterORCommandLineBindingList Class
#'
#' @param \dots element or list of the element.
#' @export CCBList
#' @exportClass characterORCommandLineBindingList
#'
#' @aliases CCBList characterORCommandLineBindingList-class
#' @return CCBList
#' 
#' @examples
#' CCBList("-o output.bam")
CCBList <- setListClass("characterORCommandLineBinding")

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
#' @rdname CommandLineTool
#'
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
#' 
#' clt <- CommandLineTool(inputs = ipl, baseCommand = "samtools sort")
CommandLineTool <- setRefClass("CommandLineTool",
                               contains = "Process",
                               fields = list(
                                   class = "character",
                                   baseCommand = "character",
                                   arguments = "characterORCommandLineBindingList",
                                   stdin = "characterORExpression",
                                   stdout = "characterORExpression",
                                   successCodes = "integer",
                                   temporaryFailCodes = "integer",
                                   permanentFailCodes = "integer"
                               ),
                               method = list(
                                   initialize = function(class = "CommandLineTool",
                                       arguments = "",
                                       ...){
                                       if(is.character(arguments)){
                                           arguments <<- CCBList(
                                               CommandLineBinding(
                                                   valueFrom = arguments
                                               ))
                                       }
                                       if(is(arguments, "CommandLineBiding")){
                                           arguments <<- CCBList(arguments)
                                       }
                                       if(is(arguments,
                                             "characterORCommandLineBindingList")){
                                           arguments <<- arguments
                                       }
                                       class <<- class
                                       callSuper(...)
                                   }
                               ))


#' CommandInputParameter Class
#'
#' An input parameter for a CommandLineTool.
#'
#' @export CommandInputParameter
#' @exportClass CommandInputParameter
#'
#' @examples
#' ipl <- InputParameterList(
#'     CommandInputParameter(id = "BAM", type = "File",
#'                           label = "input bam",
#'                           description = "input bam",
#'                           inputBinding = CommandLineBinding(
#'                               position = 1L
#'                           )),
#'     CommandInputParameter(id = "level", type = "Integer",
#'                           label = "Compression level",
#'                           description = "Compression level",
#'                           inputBinding = CommandLineBinding(
#'                               position = 2L,
#'                               prefix = "-l"
#'                           ))    
#' )
CommandInputParameter <-
    setRefClass("CommandInputParameter", contains = "InputParameter")

 



#' CommandInputSchema Class
#'
#' @export CommandInputSchema
#' @exportClass CommandInputSchema
#' @examples
#' CommandInputSchema()
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
#'
#' @examples
#' CommandOutputBinding(glob = "*.bam")
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
#'
#' @examples
#' CommandOutputSchema()
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
#'
#' @examples
#' CommandOutputParameter(outputBinding = CommandOutputBinding(glob = "*.bam"))
CommandOutputParameter <-
    setRefClass("CommandOutputParameter", contains = "OutputParameter",
                fields = list(
                    outputBinding = "CommandOutputBinding"
                ))


