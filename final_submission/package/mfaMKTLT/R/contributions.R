#' @title Observation Contribution Method
#'
#' @description Creates a table that shows the contribution of each observation
#'   to each component.
#'
#' @param x  An object of class "mfa".
#' @param verbose  A flag that controls whether the summary table is printed
#'   to the console. \cr
#'   If TRUE, then the summary table is printed when the function is called. \cr
#'   If FALSE, no output is printed. \cr
#'   In either event, the summary table may be assigned to another object and
#'     accessed later.
#'
#' @return  The function returns a matrix of observation contributions. The
#'   [i, j] element of this matrix (ith row, jth column) is the contribution
#'   of the ith observation to the jth component.
#'
#' @export
#'
#' @details Calculated according to Equation (25), page 7 of 'Abdi, Herve,
#'   Lynne J. Williams, and Domininique Valentin. "Multiple Factor Analysis:
#'   PrincipalComponent Analysis for Multitable and Multiblock Data Sets." Wiley
#'   Interdisciplinary Reviews: Computational Statistics 5.2 (2013): 149-79'
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#' ## Two ways to use this function
#' # 1. Print the output immediately
#' obscont(mfa1, verbose = TRUE)
#' # 2. silently assign the object to another object without displaying
#' summary <- obscont(mfa1)
#' round(summary, 3)[1:5,1:5]
#'
obscont <- function(x, verbose = FALSE){

  stop.ismfa(x) # Check to make sure the object has class "mfa"

  # observation mass
  mass <-  x$dimdata["num_obj"]**-1

  # squared commmon factor score matrix
  f2 <- x$Fcommon**2

  # numerator
  num <- mass * f2

  # eigenvalues
  eigs <- eigsum(x, verbose = FALSE)["Eigenvalues", ]

  # Calculate the observation contribution matrix
  # divide each column of the numerator matrix by the corresponding eigenvalue
  obscont <- sapply(seq.int(x$comps_gen), function(i) num[,i]/eigs[i])

  # Every column should sum to 1, since together, all the observations explain
  #   100% of a given component
  if (mean(round(apply(obscont, 2, sum), 5) == 1) != 1)
    stop("Each column in the observation contribution matrix should sum to 1")

  if (verbose) print(round(obscont, 3))
  invisible(obscont)
}


# -----------------------------------------------------------------------------


#' @title Variable Contribution Method
#'
#' @description Creates a table that shows the contribution of each variable
#'   to each component.
#'
#' @param x  An object of class "mfa".
#' @param verbose  A flag that controls whether the summary table is printed
#'   to the console. \cr
#'   If TRUE, then the summary table is printed when the function is called. \cr
#'   If FALSE, no output is printed. \cr
#'   In either event, the summary table may be assigned to another object and
#'     accessed later.
#'
#' @return  The function returns a matrix of variable contributions. The
#'   [i, j] element of this matrix (ith row, jth column) is the contribution
#'   of the ith variable to the jth component.
#'
#' @export
#'
#' @details Calculated according to Equation (27), page 7 of 'Abdi, Herve,
#'   Lynne J. Williams, and Domininique Valentin. "Multiple Factor Analysis:
#'   PrincipalComponent Analysis for Multitable and Multiblock Data Sets." Wiley
#'   Interdisciplinary Reviews: Computational Statistics 5.2 (2013): 149-79'
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#' ## Two ways to use this function
#' # 1. Print the output immediately
#' varcont(mfa1, verbose = TRUE)
#' # 2. silently assign the object to another object without displaying
#' summary <- varcont(mfa1)
#' round(summary, 3)[1:5,1:5]
#'
varcont <- function(x, verbose = FALSE){

  stop.ismfa(x) # Check to make sure the object has class "mfa"

  # squared loadings
  q2 <- x$Q**2


  varcont <- t(sapply(seq.int(length(unlist(x$sets))),
                      function(j) x$a[j] * q2[j,]))

  # Check that columns sum to 1
  if (mean(round(apply(varcont, 2, sum), 5) == 1) != 1)
    stop("Each column in the observation contribution matrix should sum to 1")

  if (verbose) print(round(varcont, 3))
  invisible(varcont)

}


# -----------------------------------------------------------------------------


#' @title Table Contribution Method
#'
#' @description Creates a table that shows the contribution of each sub-table
#'   to each component.
#'
#' @param x  An object of class "mfa".
#' @param verbose  A flag that controls whether the summary table is printed
#'   to the console. \cr
#'   If TRUE, then the summary table is printed when the function is called. \cr
#'   If FALSE, no output is printed. \cr
#'   In either event, the summary table may be assigned to another object and
#'     accessed later.
#'
#' @return  The function returns a matrix of variable contributions. The
#'   [i, j] element of this matrix (ith row, jth column) is the contribution
#'   of the ith sub-table to the jth component.
#'
#' @export
#'
#' @details Calculated according to Equation (28), page 7 of 'Abdi, Herve,
#'   Lynne J. Williams, and Domininique Valentin. "Multiple Factor Analysis:
#'   PrincipalComponent Analysis for Multitable and Multiblock Data Sets." Wiley
#'   Interdisciplinary Reviews: Computational Statistics 5.2 (2013): 149-79' \cr
#' NB: The table contribution of a given sub-table can be found by summing up
#'   the contributions of its constituent variables.
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#' ## Two ways to use this function
#' # 1. Print the output immediately
#' tabcont(mfa1, verbose = TRUE)
#' # 2. silently assign the object to another object without displaying
#' summary <- tabcont(mfa1)
#' round(summary, 3)[1:5,1:5]
#'
tabcont <- function(x, verbose = FALSE){
  stop.ismfa(x) # Check to make sure the object has class "mfa"
  varcont <- varcont(x)

  # i <- 1
  # x$sets[[i]]
  # apply(varcont[x$sets[[i]], ], 2, sum)

  tabcont <- t(sapply(seq.int(x$K), function(i)
    apply(varcont[x$sets[[i]], ], 2, sum) ))

  # Check that columns sum to 1
  if (mean(round(apply(tabcont, 2, sum), 5) == 1) != 1)
    stop("Each column in the observation contribution matrix should sum to 1")

  if (verbose) print(round(tabcont, 3))
  invisible(tabcont)
}

