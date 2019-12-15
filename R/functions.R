
#' Count missing values in a vector
#'
#' @param vector  The vector which values are to be counted.
#' @param missing Boolean. Whether to count missing or non-missing values.
#'
#' @return An integer with the count of values
#' @export
#'
#' @examples
#'
#' iris$Sepal.Length[4] <- NA
#' length_na(iris$Sepal.Length)
#' length_na(iris$Sepal.Length, missing = FALSE)
#'
length_na <- function(vector,
                      missing = TRUE){

  if(missing){
    inp <- is.na(vector)
  }else {
    inp <- !is.na(vector)
  }

  len <- length(which(inp))

  return(len)
}
