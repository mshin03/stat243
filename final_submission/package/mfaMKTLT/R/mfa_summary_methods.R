#' @title Eigenvalue Summary Method
#'
#' @description Creates a Summary Table with Singular Values, Eigenvalues, and
#'   inertias. Used with the "mfa" object class.
#'
#' @param x  An object of class "mfa".
#' @param verbose  A flag that controls whether the summary table is printed
#'   to the console. \cr
#'   If TRUE, then the summary table is printed when the function is called. \cr
#'   If FALSE, no output is printed. \cr
#'   In either event, the summary table may be assigned to another object and
#'     accessed later.
#'
#' @return  The function returns a summary table with the singular values,
#'   eigenvalues, and inertias for each component in the MFA. NB: whatever
#'   value the user chose for \code{ncomps}, this table will show information
#'   for all possible components. That is, the table has \code{L} columns,
#'   where \code{L} is the rank of the MFA \code{data}.
#'
#' @export
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#' ## Two ways to use this function
#' # 1. Print the output immediately
#' eigsum(mfa1, verbose = TRUE)
#' # 2. silently assign the object to another object without displaying
#' summary <- eigsum(mfa1)
#' round(summary, 3)[1:5,1:5]
#'
eigsum <- function(x, verbose = FALSE){

  stop.ismfa(x) # Check to make sure the object has class "mfa"

  # mass of each observation. NB: We are assuming that the observation mass
  #   is always I^-1, where I is the number of observations
  mass <-  x$dimdata["num_obj"]**-1

  # Common Factor Scores, with each element squared
  f2 <- x$Fcommon**2

  # Calculating eigenvalues using Paper 1, Eq (24)
  eigs <- x$eigvals

  sv <- sqrt(eigs)
  cum.eig <- cumsum(eigs)
  pcnt.inertia <- eigs/sum(eigs) * 100
  cum.pcnt <- cumsum(pcnt.inertia)
  mytable <- rbind(sv, eigs, cum.eig, pcnt.inertia, cum.pcnt)
  rownames(mytable) <- c("Singular Values",
                         "Eigenvalues",
                         "Cumulative Eigenvalues",
                         "% Inertia",
                         "Cumulative % Inertia")
  colnames(mytable) <- paste0("comp ", seq.int(length(eigs)))
  if (verbose) print(round(mytable, 3))
  invisible(mytable)
}


# -----------------------------------------------------------------------------

