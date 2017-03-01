# Private function : stop execution if object is not a matrix
#   Optionally, requires object to have certain number of rows and columns

stop.ismatrix <- function(x, rows = NA, cols = NA){

  name <- as.character(match.call()[2])
  if (!is.matrix(x)) stop(sprintf("%s must be in matrix form", name))
  if (!is.na(rows)) if (nrow(x) != rows)
    stop(sprintf("%s must have %s rows", name, rows))
  if (!is.na(cols)) if (ncol(x) != cols)
    stop(sprintf("%s must have %s columns", name, cols))
  TRUE
}


# Private function : stop execution if object is not a list
#   Optionally, requires a certain length
stop.islist <- function(x, len = NA){

  name <- as.character(match.call()[2])
  if (!is.list(x)) stop(sprintf("%s must be in list form", name))
  if (!is.na(len)) if (length(x) != len)
    stop(sprintf("%s must have length %s", name, len))
  TRUE
}


# Private function : stop execution if object is not a vector
#   Optionally, requires a certain length
stop.isvector <- function(x, len = NA){
  name <- as.character(match.call()[2])
  if (!is.vector(x)) stop(sprintf("%s must be in vector form", name))
  if (!is.na(len)) if (length(x) != len)
    stop(sprintf("%s must have length %s", name, len))
  TRUE
}


# Private function : stop execution if object class is not 'mfa'
stop.ismfa <- function(x){
  if (class(x) != "mfa")
  stop("This function should only be used with objects of class 'mfa'.")
  TRUE
  }


