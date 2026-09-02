#' @keywords internal
"_PACKAGE"

# `.data` is rlang's pronoun for a column inside a tidy-eval expression. Without
# importing it, R CMD check reports it as an undeclared global in every chart
# function that references a column by name.
#' @importFrom rlang .data
NULL
