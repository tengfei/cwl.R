########################################################################
## Workflow
########################################################################



setClass("LinkMergeMethod", contains = "VIRTUAL")

#' @section WorkflowStepInput Class:
#' \describe{
#'
#' The input of a workflow step connects an upstream parameter (from
#' the workflow inputs, or the outputs of other workflows steps) with
#' the input parameters of the underlying process.
#'
#' #' If the sink parameter is an array, or named in a workflow scatter
#' operation, there may be multiple inbound data links listed in the
#' connect field. The values from the input links are merged depending
#' on the method specified in the linkMerge field. If not specified,
#' the default method is merge_nested:
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
#' single elements.  } 
#'
#' Fields:
#' 
#' \item{\code{id}}{ (character) A unique identifier for this workflow input
#' parameter.}
#'
#' \item{\code{source}}{[character] Specifies one or more workflow parameters
#' that will provide input to the underlying process parameter.}
#'
#' \item{\code{linkMerge}}{[LineMergeMethod] The method to use to merge
#' multiple inbound links into a single array. If not specified, the
#' default method is merge_nested:}
#'
#' \item{\code{default}}{ [ANY] The default value for this parameter if there
#' is no source field.}
#' }
#' 
#' @export WorkflowStepInput
#' @exportClass WorkflowStepInput
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepInput WorkflowStepInput-class
#'
#' @return a WorkflowStep object or subclass object.
WorkflowStepInput <- setRefClass("WorkflowStepInput",
                                 fields = list(
                                     id = "character",
                                     source = "character",
                                     linkMerge = "LinkMergeMethod",
                                     default = "ANY"
                                 ),
                                 methods = list(
                                     initialize = function(id = "", ...){
                                         id <<- addIdNum(id)
                                         callSuper(...)
                                     }

                                 ))

#' @section WorkflowStepOutput Class:
#' \describe{
#' 
#' Associate an output parameter of the underlying process with a
#' workflow parameter. The workflow parameter (given in the id field)
#' be may be used as a source to connect with input parameters of
#' other workflow steps, or with an output parameter of the process.
#'
#' \item{\code{id}}{ (character) A unique identifier for this workflow output
#' parameter. This is the identifier to use in the source field of
#' WorkflowStepInput to connect the output value to downstream
#' parameters.}
#'
#' }
#'
#' @export WorkflowStepOutput
#' @exportClass WorkflowStepOutput
#' @rdname WorkflowStep
#' @aliases WorkflowStepOutput WorkflowStepOutput-class
WorkflowStepOutput <- setRefClass("WorkflowStepOutput",
                                  fields = list(
                                      id = "character"
                                  ),
                                  methods = list(
                                      initialize = function(id = "", ...){
                                          id <<- addIdNum(id)
                                          callSuper(...)
                                      }

                                  ))

#' WorkflowStepInputList
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepInputList WorkflowStepInputList-class
#'
#' @export WorkflowStepInputList
#' @exportClass WorkflowStepInputList
WorkflowStepInputList <- setListClass("WorkflowStepInput")

#' WorkflowStepOutputList
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepOutputList WorkflowStepOutputList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowStepOutputList
#' @exportClass WorkflowStepOutputList
WorkflowStepOutputList <- setListClass("WorkflowStepOutput")


#' WorkflowStepList
#'
#' @rdname WorkflowStep
#' @aliases WorkflowStepList-class WorkflowStepList
#' @export WorkflowStepList
#' @exportClass WorkflowStepList
WorkflowStepList <- setListClass("WorkflowStep")


#' @section WorkflowOutputParameter Class:
#' \describe{
#' Describe an output parameter of a workflow. The parameter must be
#' connected to one or more parameters defined in the workflow that
#' will provide the value of the output parameter.
#'
#' \item{\code{source}}{ [character] Specifies one or more workflow parameters
#' that will provide this output value.}
#'
#' \item{\code{linkMerge}}{ [LinkMergeMethod] The method to use to merge
#' multiple inbound links into a single array. If not specified, the
#' default method is merge_nested:}
#'}
#' @export WorkflowOutputParameter
#' @exportClass WorkflowOutputParameter
#'
#' @return a Workflow object.
#' 
#' @rdname Workflow
#' @aliases WorkflowOutputParameter WorkflowOutputParameter-class
WorkflowOutputParameter <-
    setRefClass("WorkflowOutputParameter", contains = "OutputParameter",
                fields = list(
                    source = "character",
                    linkMerge = "LinkMergeMethod"
                ))

#' @aliases WorkflowOutputParameterList-class
#'
#' @param \dots element or list of the element.
#'
#' @export WorkflowOutputParameterList
#' @exportClass WorkflowOutputParameterList
#'
#' @rdname Workflow
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
#'
#' @rdname Workflow
#' @examples
#' ## need better examples here
#' ws <- WorkflowStepList(WorkflowStep(id = "step1", label = "align-and-sort",
#'              description = "align and sort", 
#'              inputs = WorkflowStepInputList(
#'                  WorkflowStepInput(id = "id1"),
#'                  WorkflowStepInput(id = "id2")
#'              )))
#' Workflow(steps = ws)
Workflow <-
    setRefClass("Workflow", contains = "Process",
                fields = list(
                    class = "character",
                    outputs = "WorkflowOutputParameterList", 
                    steps = "WorkflowStepList"
                ),
                method = list(
                    initialize = function(class = "Workflow", ...){
                        class <<- class
                        callSuper(...)
                    }
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
#' @rdname WorkflowStep
#' @aliases WorkflowStep WorkflowStep-class
#'
#' @examples
#' ws <- WorkflowStepList(WorkflowStep(id = "step1", label = "align-and-sort",
#'              description = "align and sort", 
#'              inputs = WorkflowStepInputList(
#'                  WorkflowStepInput(id = "id1"),
#'                  WorkflowStepInput(id = "id2")
#'              )))
WorkflowStep <-
    setRefClass("WorkflowStep", contains = "CWL", 
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
                ),
                methods = list(
                    initialize = function(id = "", ...){
                        id <<- addIdNum(id)
                        callSuper(...)
                    }
                ))

