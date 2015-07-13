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
#'
#' @examples
#' ExpressionTool(expression =
#'                   Expression(engine = "cwl:JsonPointer",
#'                              script = "$job.inputs['threads']"))
ExpressionTool <- setRefClass("ExpressionTool", contains = "Process",
                              fields = list(
                                  class = "character", 
                                  expression = "Expression"
                              ),
                              method = list(
                                  initialize = function(
                                      class = "ExpressionTool",
                                      ...){
                                      class <<- class
                                      callSuper(...)
                                  }                    
                              ))
