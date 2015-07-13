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
#'
#' @examples
#' Expression(engine = "cwl:JsonPointer", script = "$job.inputs['threads']")
Expression <- setRefClass("Expression",
                          contains = "CWL",
                          fields = list(
                              engine = "JsonPointerORcharacter",
                              script = "character"
                          ))

