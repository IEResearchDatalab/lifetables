################################################################################
#
# Utility Functions
#
# Common helper functions used across the pipeline.
# Source this file once at the top of any script that needs these.
#
################################################################################

#' String concatenation operator
#' @examples "hello" %+% " world"
`%+%` <- function(a, b) paste0(a, b)

#' Repeat a string n times and collapse
#' @param x Character string to repeat
#' @param n Number of repetitions
#' @return Single collapsed string
Rep <- function(x, n) paste(rep(x, n), collapse = "")

#' Compute ax (fraction of interval lived by those who die)
#' @param age Numeric age
#' @return ax value (0.1 for age 0, 0.5 otherwise)
get_ax <- function(age) {
	ifelse(age == 0, 0.1, 0.5)
}

#' Convert central death rate (mx) to probability of death (qx)
#' @param mx Central death rate
#' @param ax Fraction of interval lived by those who die (default 0.5)
#' @return Probability of dying in the interval
mx_to_qx <- function(mx, ax = 0.5) {
	mx / (1 + (1 - ax) * mx)
}

#' Print a section header to console
#' @param title Section title
#' @param width Width of the header line (default 72)
cat_header <- function(title, width = 72) {
	cat("\n" %+% Rep("=", width) %+% "\n")
	cat(title %+% "\n")
	cat(Rep("=", width) %+% "\n\n")
}

#' Print a step label to console
#' @param step_num Step number
#' @param description Step description
cat_step <- function(step_num, description) {
	cat(sprintf("\nStep %d: %s...\n", step_num, description))
}
