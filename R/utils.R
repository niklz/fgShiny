#' Provides a list containing the data.frame columns that are of a given type.
#' This can be useful for functions that only support a given type.
#'
#' Thanks to https://github.com/adam-m-mcelhinney/helpRFunctions/blob/master/R/list.df.var.types.R
#'
#' @param df The data.frame you wish to have a list of the types for.
#' @return A list with names corresponding to the data type, and entries
#' corresponding to the variable names that are of that type.
#' @examples
#' df <- data.frame(matrix(rnorm(100), nrow = 10))
#' df <- cbind(df, rep("A", nrow(df)))
#' #cor(df) # This will return an error because the last column is not numeric.
#' varList <- list.df.var.types(df)
#' cor(df[,varList$numeric]) # No longer returns an error. Ignores the
#' #non-numeric columns.
list.df.var.types <- function(df) {
  if(!is.data.frame(df)) stop('Must provide data frame')
  var.types <- lapply(df, class) # Get all the var types for each variable
  types <- names(table(unlist(var.types))) # Determine the distinct var types that exist in that data set
  varList <- list() # Create empty list to store the names of the variables of each type


  for (j in types) {
    l <- list(names(var.types[var.types == j]))
    names(l) <- j
    varList <- c(varList, l)
  }
  return (varList)
}

#' List NAs of a table
list.var.na <- function(tbl) {

  logVal <- sapply(tbl, function(x) any(is.na(x)))
  data.frame(ifHasNa = logVal)
}

#' Summarise NAs of a table
sum.var.na <- function(tbl) {
  sapply(tbl, function(x) any(is.na(x))) %>% as.factor %>% summary
}

#' List blanks of a table
list.var.blank <- function(tbl) {
  logVal <- sapply(tbl, function(x) any(x==""))
  data.frame(ifHasBlank = logVal)
}

#' Summarise blanks of a table
list.var.blank <- function(tbl) {
  sapply(tbl, function(x) any(x=="")) %>% as.factor %>% summary
}

#' Replace NA with empty strings or vice versa
#' @examples fill.var.missing(tbl, NA, "")
fill.var.missing <- function(x, missingFrom, missingTo) {
  if (is.na(missingFrom)) {
    x[is.na(x)] <- missingTo
  } else {
    x[x == missingFrom] <- missingTo
  }
  return(x)
}

#' Convert character to factor
cha2fac <- function(tbl){
  tbl <- as.data.frame(unclass(tbl))
  return(tbl)
}

#' Checks its input vector for numerical special values if the type is numeric,
#' otherwise it only checks for NA.
#' Thanks to https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf
is.special <- function(x) {
  if (is.numeric(x)) !is.finite(x)
    else is.na(x)
}
