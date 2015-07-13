########################################################################
## ProcessRequirement
########################################################################

#' ProcessRequirement Class
#'
#' @section ProcessRequirement:
#' \describe{
#' A process requirement declares a prerequisite that may or must be
#' fufilled before executing a process. See Process.hints and
#' Process.requirements. Process requirements are the primary
#' mechanism for specifying extensions to the CWL core specification. 
#'
#' 
#' \item{\code{class}}{(character) The specific requirement type.}
#' }
#'
#' @rdname ProcessRequirement
#' @export ProcessRequirement
#' @exportClass ProcessRequirement
#'
#' @return a ProcessRequirement object or subclass object.
#'
#' @examples
#' dkr <- DockerRequirement(dockerImageId = "testid")
#' cfr <- CreateFileRequirement(fileDef =
#'                                  FileDefList(FileDef(filename = "hello.txt")))
#' sfr <- SubworkflowFeatureRequirement()
#' evr <- EnvVarRequirement(envDef = EnvironmentDefList(
#'     EnvironmentDef(envName = "path",
#'                    envValue = "testpath")
#' ))
#' safr <- ScatterFeatureRequirement()
#' eer <- ExpressionEngineRequirement(id = "hello")
#' ProcessRequirementList(dkr, cfr, sfr, evr, safr, eer)                    
ProcessRequirement <- setRefClass("ProcessRequirement",
                                  contains = c("VIRTUAL", "CWL"),
                                  field = list(class = "character"))





#' @section DockerRequirement Class:
#' \describe{
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
#' 
#' 
#' \item{\code{dockerPull}}{[character] Get a Docker image using
#' docker pull}
#' 
#' \item{\code{dockerLoad}}{[character] Specify a HTTP URL from which
#' to download a Docker image using docker load.}
#' 
#' \item{\code{dockerFile}}{[character] Supply the contents of a
#' Dockerfile which will be build using docker build.}
#' 
#' \item{\code{dockerImageId}}{[character] The image id that will be
#' used for docker run. May be a human-readable image name or the
#' image identifier hash. May be skipped if dockerPull is specified,
#' in which case the dockerPull image id will be used.}
#' 
#' \item{\code{dockerOutputDirectory}}{ [character] Set the designated
#' output directory to a specific location inside the Docker
#' container.}
#' 
#' }
#' 
#'
#'  
#' @export DockerRequirement
#' @exportClass DockerRequirement
#'
#' @rdname ProcessRequirement
#' @aliases DockerRequirement
DockerRequirement <- setRefClass("DockerRequirement",
                                 contains = "ProcessRequirement",
                                 fields = list(
                                     dockerPull = "character",
                                     dockerLoad = "character",
                                     dockerFile = "character",
                                     dockerImageId = "character",
                                     dockerOutputDirectory = "character"
                                 ),
                                 method = list(
                                     initialize = function(
                                         class = "DockerRequirement",
                                     ...){
                                         class <<- class
                                         callSuper(...)
                                     }
                                 ))

#' @section SubworkflowFeatureRequirement Class:
#'
#' Indicates that the workflow platform must support nested workflows
#' in the run field of (WorkflowStep)(#workflowstep).
#'
#' @export SubworkflowFeatureRequirement 
#' @exportClass SubworkflowFeatureRequirement
#' 
#' @rdname ProcessRequirement
#' @aliases SubworkflowFeatureRequirement
SubworkflowFeatureRequirement <-
    setRefClass("SubworkflowFeatureRequirement",
                contains = "ProcessRequirement",
                method = list(
                    initialize = function(
                        class = "SubworkflowFeatureRequirement",
                        ...){
                        class <<- class
                        callSuper(...)
                    }                    
                ))


setClassUnion("characterORExpression", c("character", "Expression"))


#' @section FileDef Class:
#'
#' \describe{
#' 
#' Define a file that must be placed by in the designated output
#' directory prior to executing the command line tool. May be the
#' result of executing an expression, such as building a configuration
#' file from a template.
#'
#' 
#'
#' \item{\code{filename}}{(characterORExpression) The name of the file
#' to create in the output directory.}
#'
#' \item{\code{fileContent}}{(characterORExpression) If the value is a
#' string literal or an expression which evalutes to a string, a new
#' file must be created with the string as the file contents. If the
#' value is an expression that evaluates to a File object, this
#' indicates the referenced file should be added to the designated
#' output directory prior to executing the tool. Files added in this
#' way may be read-only, and may be implemented through bind mounts or
#' file system links in such a way as to avoid unecessary copying of
#' the input file.}
#'
#' 
#'
#' }
#'
#' @export FileDef
#' @exportClass FileDef
#' @rdname ProcessRequirement
FileDef <- setRefClass("FileDef", fields = list(
                                      filename = "characterORExpression",
                                      fileContent = "characterORExpression"
))

#' @export FileDefList
#' @exportClass FileDefList
#'
#' @rdname ProcessRequirement
#' @aliases FileDefList FileDefList-class
FileDefList <- setListClass("FileDef")

#' @section CreateFileRequirement Class:
#' \describe{
#' 
#' Define a list of files that must be created and placed by the
#' workflow platform in the designated output directory prior to
#' executing the command line tool. See FileDef for details.
#'
#' 
#' 
#' \item{\code{fileDef}}{(FileDefList) The list of files.}
#' 
#' }
#' 
#'
#' @export CreateFileRequirement
#' @exportClass CreateFileRequirement
#' @rdname ProcessRequirement
#' @aliases CreateFileRequirement 
CreateFileRequirement <-
    setRefClass("CreateFileRequirement", contains = "ProcessRequirement",
                fields = list(
                    fileDef = "FileDefList"
                ),
                method = list(
                    initialize = function(
                        class = "CreateFileRequirement",
                        ...){
                        class <<- class
                        callSuper(...)
                    }                    
                ))

#' @section EnvironmentDef Class:
#' \describe{
#' 
#' Define an environment variable that will be set in the runtime
#' environment by the workflow platform when executing the command
#' line tool. May be the result of executing an expression, such as
#' getting a parameter from input.
#'
#' 
#'
#' \item{\code{envName}}{(character) The environment variable name. }
#' 
#' \item{\code{envValue}}{(characterORExpression) The environment
#' variable value.}
#'
#' 
#' 
#'
#' }
#' @export EnvironmentDef
#' @exportClass EnvironmentDef
#' @rdname ProcessRequirement
#' @aliases EnvironmentDef
EnvironmentDef <- setRefClass("EnvironmentDef",
                              fields = list(
                                  envName = "character",
                                  envValue = "characterORExpression"
                              ))


#' @param \dots element or list of the element.
#' 
#' @export EnvironmentDefList
#' @exportClass EnvironmentDefList
#' @rdname ProcessRequirement
#' @aliases EnvironmentDefList EnvironmentDefList-class
EnvironmentDefList <- setListClass("EnvironmentDef")

#' @section EnvVarRequirement Class:
#' \describe{
#' 
#' Define a list of environment variables which will be set in the
#' execution environment of the tool. See EnvironmentDef for details.
#'
#' 
#' 
#' \item{\code{envDef}}{(EnvironmentDefList) The list of environment
#' variables.}
#' 
#' }
#'
#' 
#'
#' @export EnvVarRequirement
#' @exportClass EnvVarRequirement
#' @rdname ProcessRequirement
#' @aliases EnvVarRequirement
EnvVarRequirement <-
    setRefClass("EnvVarRequirement", contains = "ProcessRequirement",
                fields = list(
                    envDef = "EnvironmentDefList"
                ),
                method = list(
                    initialize = function(
                        class = "EnvVarRequirement",
                        ...){
                        class <<- class
                        callSuper(...)
                    }                    
                ))

#' @section ScatterFeatureRequirement Class:
#'
#' Indicates that the workflow platform must support the scatter and
#' scatterMethod fields of (WorkflowStep)(#workflowstep).
#' 
#' @export ScatterFeatureRequirement
#' @exportClass ScatterFeatureRequirement
#' @rdname ProcessRequirement
#' @aliases ScatterFeatureRequirement
ScatterFeatureRequirement <-
    setRefClass("ScatterFeatureRequirement", contains = "ProcessRequirement",
                method = list(
                    initialize = function(
                        class = "ScatterFeatureRequirement",
                        ...){
                        class <<- class
                        callSuper(...)
                    }                    
                ))


#' @section ExpressionEngineRequirement Class:
#' \describe{
#' 
#' Define an expression engine, as described in Expressions.
#'
#' 
#' 
#' \item{\code{id}}{(character) Used to identify the expression engine in the
#' engine field of Expressions.}
#' 
#' \item{\code{requirements}}{[ProcessRequirement]Requirements to run this
#' expression engine, such as DockerRequirement for specifying a
#' container with the engine.}
#' 
#' \item{\code{engineCommand}}{ [character] The command line to invoke the
#' expression engine.}
#' 
#' \item{\code{engineConfig}}{ [character] Additional configuration or code
#' fragments that will also be passed to the expression engine. The
#' semantics of this field are defined by the underlying expression
#' engine. Intended for uses such as providing function definitions
#' that will be called from CWL expressions.}
#'
#' }
#'
#' 
#' @export  ExpressionEngineRequirement
#' @exportClass ExpressionEngineRequirement
#' @rdname ProcessRequirement
#' @aliases ExpressionEngineRequirement
ExpressionEngineRequirement <-
    setRefClass("ExpressionEngineRequirement", contains = "ProcessRequirement",
                fields = list(
                    id = "character",
                    requirements = "ProcessRequirement",
                    engineCommand = "character",
                    engineConfig = "character"
                ),
                method = list(
                    initialize = function(
                        class = "ExpressionEngineRequirement",
                        ...){
                        class <<- class
                        callSuper(...)
                    }                    
                ))


