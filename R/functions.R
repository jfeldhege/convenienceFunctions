
#' Count missing values in a vector
#'
#' @param vector  The vector which values are to be counted.
#' @param missing Boolean. Whether to count missing or non-missing values.
#'
#' @return An integer with the count of values
#' @export
#'
#' @examples
#' iris$Sepal.Length[4] <- NA
#' length_NA(iris$Sepal.Length)
#' length_NA(iris$Sepal.Length, missing = FALSE)

length_NA <- function(vector,
                      missing = TRUE){

  if(missing){
    inp <- is.na(vector)
  }else {
    inp <- !is.na(vector)
  }

  len <- length(which(inp))

  return(len)
}


#' Count values that satisfy a condition
#'
#' @param condition The condition for which values are to be counted. Must be
#' type logical
#'
#' @return An integer with the count of values that satisfy the condition.
#' @export

length_which <- function(condition) {

  assertthat::assert_that(is.logical(condition), msg = "Value is not a logical")

  return(length(which(condition)))
}


#' Sets new chapter in R script in RStudio
#'
#' @return Creates a vertical line in R script
#' @export
#' @references https://www.statworx.com/de/blog/defining-your-own-shortcut-in-rstudio/


set_new_chapter <- function(){
  # set limit to which position dashes should be included
  nchars <- 81

  # grab current document information
  context <- rstudioapi::getActiveDocumentContext()
  # extract horizontal courser position in document
  context_col <- context$selection[[1]]$range$end["column"]

  # if a line has less than 81 characters, insert hyphens at the current line
  # up to 80 characters
  if (nchars > context_col) {
    rstudioapi::insertText(strrep("-", nchars - context_col))
  }
}
