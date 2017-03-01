# Private function : make.data - a function to process data prior to MFA
#   Checks to make sure data is either matrix or data.frame
#   If data is data.frame, makes it matrix
#   Makes rownames and column names lower case
make.data <- function(dat){
  if (!is.matrix(dat) & !is.data.frame(dat)) stop("data must be either matrix or data frame")
  if (is.data.frame(dat)) dat <- as.matrix(dat)
  dat <- make_lowercase(dat, c("colnames", "rownames"))
  dat
}


# Private function : get.sets - This function processes the set list, which
#   contains information about how the main data is split into sub-tables
#   Any character element in the set list becomes numeric
#   Also does a variety of checks on the set list itself:
#     it should be the right length, and each element should be either
#     the name or the index of a data column
get.sets <- function(data, sets, K){
  # Check that each argument is the appropriate type of object
  stop.ismatrix(data)
  stop.isvector(K, 1)
  # length of sets should be equal to the number of sub-tables
  stop.islist(sets, K)

  # Get the variable names
  varnames <- colnames(data)

  # Each variable name should be unique
  if ( mean(tolower(varnames) == unique(tolower(varnames))) != 1)
    stop("variable names must be unique (case-insensitive)")

  # Get the indices associated with each variable name
  sets.numeric <- lapply(sets, function(x)
    if (is.character(x)){
      # Check that each character element of sets is a column name of data
      if (mean(tolower(x) %in% tolower(varnames)) < 1)
        stop("each character element of sets must be a column name of data")
      return(which(tolower(varnames) %in% tolower(x)))
    }else if (is.numeric(x)) return(x)
    else stop("each element of the sets list must be either character or numeric"))

  # Check that each numeric element of sets is a column index
  if (mean(unlist(sets.numeric) %in% seq.int(ncol(data))) < 1)
    stop("each numeric element of sets must be the index of a column of data")

  # new list of sets should also have length K
  stop.islist(sets.numeric, K)

  # length of each vector in the original and processed lists should be the same
  for (k in seq.int(K)) if(length(sets[[k]]) != length(sets.numeric[[k]]))
    stop("Something weird happened. Each element of the lists should have the
         same length")

  # Return the numeric set list
  sets.numeric
}

