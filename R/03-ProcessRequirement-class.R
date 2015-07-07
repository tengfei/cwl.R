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


