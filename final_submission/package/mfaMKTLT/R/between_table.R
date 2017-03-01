#' @title Rv Coefficient
#'
#' @description Calculates the Rv Coefficient between two data tables. This is
#'   a measure of similarity between data tables.
#'
#' @param X,Y  Two data tables (matrices or data frames)
#'
#' @return  The function returns the Rv Coefficient, a numeric vector, length 1.
#'   This is a multivariate generalization of the squared Pearson Product-Moment
#'   Correlation Coefficient. It takes real values from 0 to 1.
#'
#' @export
#'
#' @details Calculated according to Equation (29), page 8 of 'Abdi, Herve,
#'   Lynne J. Williams, and Domininique Valentin. "Multiple Factor Analysis:
#'   PrincipalComponent Analysis for Multitable and Multiblock Data Sets." Wiley
#'   Interdisciplinary Reviews: Computational Statistics 5.2 (2013): 149-79'
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#'
#' # wine data: sub-tables 1 and 2, normalized
#' rv_coeff(mfa1$subtable[[1]], mfa1$subtable[[2]])
#'
#' # wine data: sub-tables 1 and 2, non-normalized
#' rv_coeff(winedata[,1:6], winedata[,7:12])
#'
rv_coeff <- function(X, Y){
  # if a data table is a df, make it a matrix
  X <- make.data(X) # X corresponds to table1 or k
  Y <- make.data(Y) # Y corresponds to table2 or k'
  XXt <- X %*% t(X)
  YYt <- Y %*% t(Y)

  my_trace(XXt %*% YYt) /
    (sqrt( my_trace(XXt %*% XXt) *
             my_trace(YYt %*% YYt)))
}


# -----------------------------------------------------------------------------


#' @title Lg Coefficient
#'
#' @description Calculates the Lg Coefficient between two data tables. This is
#'   another measure of similarity between data tables.
#'
#' @param X,Y  Two data tables (matrices or data frames)
#'
#' @return  The function returns the Rv Coefficient, a numeric vector, length 1.
#'   This is similar to the Rv Coefficient, but unlike Rv, ir reflects the MFA
#'   normalization. It takes positive real values.
#'
#' @export
#'
#' @details Calculated according to Equation (30), page 8 of 'Abdi, Herve,
#'   Lynne J. Williams, and Domininique Valentin. "Multiple Factor Analysis:
#'   PrincipalComponent Analysis for Multitable and Multiblock Data Sets." Wiley
#'   Interdisciplinary Reviews: Computational Statistics 5.2 (2013): 149-79'
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#'
#' # wine data: sub-tables 1 and 2, normalized
#' lg_coeff(mfa1$subtable[[1]], mfa1$subtable[[2]])
#'
#' # wine data: sub-tables 1 and 2, non-normalized
#' lg_coeff(winedata[,1:6], winedata[,7:12])
#'
lg_coeff <- function(X, Y){
  # if a data table is a df, make it a matrix
  X <- make.data(X) # X corresponds to table1 or k
  Y <- make.data(Y) # Y corresponds to table2 or k'
  XXt <- X %*% t(X)
  YYt <- Y %*% t(Y)

  my_trace(XXt %*% YYt) * svd(X)$d[1]**-2 * svd(Y)$d[1]**-2

}


# -----------------------------------------------------------------------------


#' @title General Table Function
#'
#' @description Creates a table of Rv Coefficients or Lg Coefficients, based on
#'   the user's choice.
#'
#' @param data  The data set whose between-table structure the user wishes
#'   to study.
#'
#' @param sets  A list of vectors indicating how the \code{data} is organized
#'   into sub-tables.
#'
#' @param func  A function that calculates a between-table coefficient. Must be
#'   either \code{rv_coeff} or \code{lg_coeff}.
#'
#' @param dataname  An optional parameter. A character vector with the names
#'   of the sub-tables. If the user supplies a value, \code{dataname} will be
#'   used as both the rownames and colnames of the result matrix.
#'
#' @return  The function returns a K by K matrix of between-table coefficients,
#'   where K is the number of sub-tables indicated by the \code{sets}
#'   parameter. The [i, j] element of the result matrix is the between-table
#'   coefficient between sub-table i and sub-table j.
#'
#' @export
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#'
#' # scaled data, Rv
#' gentable(mfa1$data_scale, as.list(c(sets.num[1], sets.char[2:3])),
#'           paste0("sub-table_", 1:3), func = rv_coeff)
#'
#' # raw data, Rv
#' gentable(mfa1$origdata, as.list(c(sets.num[1], sets.char[2:3])),
#'           paste0("sub-table_", 1:3), func = rv_coeff)
#'
#' # scaled data, Lg
#' gentable(mfa1$data_scale, as.list(c(sets.num[1], sets.char[2:3])),
#'           paste0("sub-table_", 1:3), func = lg_coeff)
#'
#' # raw data, Lg
#' gentable(mfa1$origdata, as.list(c(sets.num[1], sets.char[2:3])),
#'           paste0("sub-table_", 1:3), func = lg_coeff)
#'
gentable <- function(data, sets, func = NA, dataname = NULL){

  clist <- lapply(match.call()[-1], function(x){
    if (!is.null(x)) return(as.character(x))
    if (is.null(x)) return(NULL)
  })

  # The user must choose a function: either rv_coeff or lg_coeff
  if (!is.function(func) | !(clist$func %in% c("rv_coeff", "lg_coeff")))
    stop("The func argument must be a between-table coefficient function:
         either rv_coeff or lg_coeff ")

  # if the data is a df, make it a matrix
  data <- make.data(data)

  # if some of the vectors in the set list are character, make them numeic
  # This will also ensure that each element in sets is a column of data
  sets <- get.sets(data, sets, length(sets))

  # Check: Data length = data name's length
  if(!is.null(dataname) && length(dataname) != length(sets)){
    stop("Length of dataname should be equal to the length of set list.")
  }

  # identify all of the different combinations of data tables
  table_combos <- expand.grid(i = seq.int(length(sets)),
                              j = seq.int(length(sets)))

  # a list to store results. Elements of this list will populate the Rv table
  result_list <- vector("list", length(sets)**2)

  # All diagonal elements will be 1, since the Rv coefficient of a table
  #   with itself is always 1
  # Because the Rv table will be symmetric, we only need to calculate the
  #   upper off-diagonal elements
  upper <- which(table_combos$i >= table_combos$j)
  result_list[upper] <- sapply(upper, function(x)
    func(data[,sets[[table_combos[x,"i"]]]],
         data[,sets[[table_combos[x,"j"]]]]))

  # Build and return the rv_table
  result_table <- matrix(result_list, ncol = length(sets), byrow = TRUE)
  # diag(result_table) <- 1
  result_table[lower.tri(result_table)] <- result_table[upper.tri(result_table)]

  # Add names if not NULL
  if (!is.null(dataname)){
    rownames(result_table) <- dataname
    colnames(result_table) <- dataname
  }

  result_table

}
