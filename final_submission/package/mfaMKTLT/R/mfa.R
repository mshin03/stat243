#' @title mfa
#'
#' @description Creates an object of class \code{"mfa"}
#'
#' @param data  A dataframe or matrix to be analyzed with MFA.
#' @param sets  A list of numeric or character vectors indicating the blocks,
#'   or sub-tables, into which the \code{data} is divided.
#' @param ncomps  Integer indicating how many components (i.e. factors) are
#'   to be extracted by the GSVD performed during MFA. \cr
#'   By default, NULL means that the maximum number of components will be
#'     extracted (based on the rank of \code{data}).
#' @param center  A logical value, or a numeric vector of length equal to the
#'   number of active variables in the analysis. \cr
#'   If center is a numeric vector, each variable (column) has the corresponding
#'     element subtracted from it. \cr
#'   If TRUE, each column is mean-centered. \cr
#'   If FALSE, no centering is done.
#' @param scale  A logical value, a numeric vector of length equal to the
#'   number of active variables in the analysis, or the character string
#'   'vector.norm'. Scaling is performed after centering. \cr
#'   If TRUE, scaling is done by dividing each column by its
#'     standard deviation. \cr
#'  If FALSE, no scaling is done. \cr
#'  If scale is a numeric vector, each column is divided by the corresponding
#'    element of scale. \cr
#'    If scale is 'vector.norm' then each variable is divided by its own vector
#'      norm (Euclidean length).
#'
#' @return The function returns an S3 object of class 'mfa'. The object is a
#'   list with the following elements: \cr
#'  \describe{
#'    \item{a}{A vector of repeated table weights for the sub-tables.
#'      Each element of \code{alpha} is repeated J.k times, where J.k is
#'      the number of variables (columns) in the kth sub-table.}
#
#'    \item{alpha}{A vector of table weights. The kth element of \code{alpha}
#'       is equal to the first singular value of the kth subtable, raised to
#'       the power of -2. This is based on a standard PCA conducted on each
#'       sub-table, after centering and scaling.}
#'
#'    \item{arglist}{A list of all the arguments the user passed to the
#'       \code{mfa} function.}
#'
#'    \item{data_scale}{The original \code{data}, limited to the active
#'       columns (as determined by \code{sets}), after centering and scaling.}
#'
#'    \item{dimdata}{The dimensions of \code{data_scale}.}
#'
#'    \item{eigvals}{Eigenvalues from the GaSVD performed on the Grand Table.}
#'
#'    \item{Fcommon}{A matrix of common/compromise factor scores.}
#'
#'    \item{Fpartial}{A list of partial factor score matrices.}
#'
#'    \item{gen_comps}{The number of components actually generated. \cr
#'       If the user chose an integer for \code{ncomps},
#'         then \code{gencomps} = \code{ncomps}. \cr
#'       Else if \code{ncomps} = NULL,
#'         then \code{gencomps} = \code{L}.}
#'
#'    \item{K}{The number of sub-tables (blocks) into which the main data is
#'       divided, as determined by \code{sets}.}
#'
#'    \item{L}{The rank of \code{data}.}
#'
#'    \item{origdata}{A copy of the data frame or matrix the user passed to
#'      the \code{data} argument of the \code{mfa} function.}
#'
#'    \item{Q}{A matrix of loadings/right singular vectors from the GSVD.}
#'
#'    \item{sets}{A copy of the list passed to the \code{sets} argument.}
#'
#'    \item{subtable}{A list of \code{K} matrices, where the kth element is
#'       the kth sub-table (block) of data, centered and scaled.}
#'  }
#'
#' @export
#' @examples
#' # Different ways to identify the sub-tables (blocks)
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' sets.char <- list(c('V1','V2','V3','V4','V5','V6'),
#'   c('V1.1','V2.1','V3.1','V4.1','V7','V8'),
#'   c('V1.2','V2.2','V3.2','V4.2','V9','V10'),
#'   c('V1.3','V2.3','V3.3','V4.3','V8.1'),
#'   c('V1.4','V2.4','V3.4','V4.4','V11','V12'),
#'   c('V1.5','V2.5','V3.5','V4.5','V13'),
#'   c('V1.6','V2.6','V3.6','V4.6'),
#'   c('V1.7','V2.7','V3.7','V4.7','V14','V5.1'),
#'   c('V1.8','V2.8','V3.8','V4.8','V15'),
#'   c('V1.9','V2.9','V3.9','V4.9'))
#' sets.mixed <- c(sets.num[1:4], sets.char[5:10])
#'
#' # default values for ncomps, center, and scale
#' mfa1 <- mfa(winedata, sets.mixed)
#'
#' # Only generate the first 2 components
#' mfa2 <- mfa(winedata, sets.mixed, ncomps = 2)
#'
#' # Cannot ask for more components than the rank of data
#' \dontrun{
#' mfa(winedata, sets.mixed, ncomps = 22)
#' }
#'
#' Cannot have elements in sets that are not column names/indices
#' \dontrun{
#' mfa(winedata, c(sets.mixed, "AlfredRenyi"))
#' mfa(winedata, c(sets.mixed, 18180505))
#' }
#'
#' If center or scale are vectors, they must be equal in
#'   length to the number of active columns (variables).
#' mfa3 <- mfa(winedata, sets.mixed, center = 1:53, scale = 1:53)
#' \dontrun{
#' mfa3 <- mfa(winedata, sets.mixed, scale = 1:52)
#' mfa3 <- mfa(winedata, sets.mixed, center = 1:54)
#' }
#'
#' @details
#' This function computes the MFA using the 'MFA as Simple PCA' approach
#'    outlinedby Abdi, Williams, and Valentin on page 9 of "Multiple factor
#'    analysis: principal component analysis for multitable and multiblock data
#'    sets" (2013).
#'
#'  STEP 1: Normalize each column of the original data. Typically this involves
#'    subtracting the mean and dividing by the vector norm. Split the data
#'    into sub-tables based on the different subjects (e.g. wine critics).
#'
#'  Step 2: Perform standard PCA on each sub-table, and record each first
#'    singular value. Create the matrices of Row and Column constraints.
#'    Calculate GSVD using these constraints. This approach is laid out on
#'    Page 24 of "Principal Component Analysis" by Hevre Abdi and Lynne J
#'    Williams, published in Wiley Comp Stats, Volume 2, 2010.
#'
#'  Step 3: Calculate quantities of interest relating to the GSVD. Put these
#'    in a nice list and return the list as an object of class 'mfa.'
mfa <- function(data, sets, ncomps = NULL, center = TRUE,
                scale = "vector.norm"){

  ###########
  ## Preliminaries: Data processing and error checking
  ##########

  # save a copy of the original data
  origdata <- data

  # Record the values of each function argument. Store them in a character list
  # clist<- as.list(match.call()[-1])
  clist <- lapply(match.call()[-1], function(x){
    if (!is.null(x)) return(as.character(x))
    if (is.null(x)) return(NULL)
  })

  # Check that data is either a data.frame or a matrix.
  # If data.frame, make it a matrix.
  # Change row and column names to lower case.
  data <- make.data(data)

  L <- qr(data)$rank # Identify the column rank of our data, denoted L

  # Calculate how many components we are going to return - comps_gen
  # if the user chose ncomps = NULL, then we will return L components,
  #   where L is column rank or the data
  # otherwise if ncomps is an integer, we will return that many components
  if (is.null(ncomps)) comps_gen <- L else comps_gen <- ncomps

  # Check that comps_gen is an integer between 1 and L
  stop.isvector(comps_gen, 1)
  if (comps_gen != as.integer(comps_gen)) stop("ncomps must be an integer")
  if (comps_gen < 1 | comps_gen > L) stop("ncomps must be between 1 and L,
                                          where L is the rank of the data")

  # how many different rows/objects do we have? denoted I in Abdi (Paper 1)
  # how many active variables do we have? denoted J
  num_obj <- nrow(data)
  num_var <- sum(sapply(sets, length))

  # how many sub-tables do we have? denoted K
  K <- length(sets)

  # If 'sets' is a list of character vectors, make it a list of numerics.
  sets <- get.sets(data, sets, K)

  # Limit the data to active columns
  data <- data[,unlist(sets)]



  ###########
  ## Step 1: Center, Scale, Create Sub-Tables
  ###########

  # Do the centering first, since scaling may depend on centering
  data_center <- scale(data, center = center, scale = FALSE)

  # If the user chose a character string for the 'scale' argument:
  # 1. make sure it's 'vector.norm' - this is the only valid string argument.
  # 2. calculate the vector of scale values so it can be passed to scale().
  if (is.character(scale)){
    if (scale != "vector.norm")
       stop("scale must be a logical, a vector (length = # of active variables),
             or the char string 'vector.norm'")
    else if (scale == 'vector.norm') scale <- apply(data_center, 2, vec_norm)
  }

  # Else if the user chose a logical, or a vector with the right length,
  #   that can be passed directly to the scale fn.
  #   NB (right length means same length as the number of active vars.
  if (!is.logical(scale)) stop.isvector(scale, num_var)
  data_scale <- scale(data_center, center = FALSE, scale = scale)

  # Create a list of the K sub-tables, Xsub
  Xsub <- lapply(sets, function(x) data_scale[,x])



  ###########
  ## Step 2: Create Matrices of Row and Col Constraints. Calculate GSVD
  ###########

  # Do Standard PCA on the sub-tables and get the first Singular Values
  # firstsv stores the first Singular Value of each of the K sub-tables
  firstsv <- sapply(Xsub, function(x) svd(x)$d[1])


  # Calculate A - the matrix of column constraints
  #   alpha is a vector of table weights. alpha.k is the table weight
  #       (singular value ^-2) of the kth subtable
  #   "a" is a vector where each alpha.k gets repeated J.k times,
  #       where J.k is the number of vars (columns) in the kth subtable
  #   "A" is a diagonal matrix constructed from the vector "a"
  alpha <- firstsv**-2
  a <- unlist(sapply(seq.int(K), function(k)
    rep(alpha[k], length(sets[[k]]))))
  A_diag <- diag(a)


  # 'M_diag' is another matrix of constraints. It is just
  #   equal to the observation weights
  #   We are using I^-1 as the weight for each observation, where
  #     I is the number of observations
  M_diag <- diag(rep(num_obj**-1), num_obj)


  ## CALCULATE GSVD:
  # First weight the grand table using the M and A constraint matrices
  # then do standardized SVD on this constrainted table. This is
  # equivalent to GSVD on the un-constrainted table.

  # NB: we are calculating the 'full' SVD at this stage.
  #   If the user chose ncomps < L, then we will just 'trim' the
  #   left/right singular vectors, and the singular values, before
  #   returning them to the user
  # X_tilde = M^1/2 * X * A^1/2,
  #   where X is the scaled data
  X_tilde <- sqrtm(M_diag) %*% data_scale %*% sqrtm(A_diag)
  gsvd <- svd(X_tilde)

  # CHECK - link 1, eq (B.4)
  # we can re-create the original (scaled) data as follows.
  #  X = M^-1/2 * X_tilde * A^-1/2
  check <- sqrtm(solve(M_diag)) %*% X_tilde %*% sqrtm(solve(A_diag))
  if (mean(round(check - data_scale, 5) == 0) != 1)
    stop("Problem calculating X_tilde, the constrained data matrix for GSVD")
  rm(check)



  ###########
  ## Step 3: Calculate quantities of interest
  ###########

  P <- gsvd$u      # Left singular vectors
  Delta <- gsvd$d  # Singular Values
  Q <- gsvd$v      # Right singular vectors


  # Checks: Link 1, EQ B.5
  #         t(P) %*% P = I
  #         t(Q) %*% Q = I
  #         X.tilde = P * Delta * Q
  if (mean( round(t(P) %*% P,5) == diag(num_obj) ) != 1 |
      mean( round(t(Q) %*% Q,5) == diag(num_obj) ) != 1 |
      mean(round(P %*% diag(Delta) %*% t(Q) - X_tilde, 5) == 0) != 1)
    stop("Problem with GSVD. The following equalities should hold:
         t(P) %*% P = I
         t(Q) %*% Q = I
         X_tilde = P * Delta * Q")


  ### CALCULATE QUANTITIES OF INTEREST
  #     These will be the elements of our 'mfa' S3 object

  ## vector of eigenvalues - eigvals
  eigvals <- Delta**2 # eigenvalues are the squared singular values

  ## P_tilde - left singular vectors
  ## Q_tilde - right singular vectors
  P_tilde <- sqrtm(solve(M_diag)) %*% P
  Q_tilde <- sqrtm(solve(A_diag)) %*% Q

  # Checks: Link 1, EQ B.3
  #         t(P_tilde) %*% M %*% P_tilde = I
  #         t(Q_tilde) %*% A %*% Q_tilde = I
  #         X = P_tilde * Delta * Q_tilde
  if (mean(round(t(P_tilde) %*% M_diag %*% P_tilde, 5)
           == diag(num_obj)) != 1 |
      mean(round(t(Q_tilde) %*% A_diag %*% Q_tilde, 5)
           == diag(num_obj)) != 1 |
      mean(round(P_tilde %*% diag(Delta) %*% t(Q_tilde)
                 - data_scale, 5) == 0) != 1)
    stop("Problem with GSVD. The following equalities should hold:
         t(P_tilde) %*% M %*% P_tilde = I
         t(Q_tilde) %*% A %*% Q_tilde = I
         X = P_tilde * Delta * Q_tilde")


  ## matrix of common factor scores—a.k.a. compromise factor scores - F
  # F = P * Delta = X * A * Q -- Paper1 Eq 65
  Fcommon <- P_tilde %*% diag(Delta)
  rownames(Fcommon) = rownames(data)

  # Check that F = X * A * Q_tilde
  check <- data_scale %*% A_diag %*% Q_tilde
  if (mean(round(check - Fcommon, 5) == 0) != 1)
    stop("Problem calculating Fcommon, the common/compromise factor scores")
  rm(check)


  ## matrix of partial factors scores -- Paper1 eq. 22
  # F.k = K * alpha.k * X.k * Q.k
  # We need the Q sub-matrices of singular vectors for the the sub-tables
  #   The overall Q matrix can be expressed as a column block matrix made of
  #   the K different Q sub-matrices - Paper1, Page 5, figure (15)
  Qsub <- lapply(sets, function(x) Q_tilde[x,])

  mytext1 <- paste0("Qsub[[", 1:K, "]]", collapse = ", ")
  mytext2 <- paste0("rbind(", mytext1, ")")
  check <- eval(parse(text = mytext2)) # weighted grand table
  if (mean(c(check == Q_tilde, Qsub[[1]] == Q_tilde[1:6,])) != 1)
    stop("Problem creating Q sub-matrices")
  rm(check)

  # This is 'trimmed' : the partial factor score matrix for each sub-table
  #   should only have 'gen_comp' columns
  Fpartial <- lapply(seq.int(K),
                     function(k)  K * alpha[k] * Xsub[[k]] %*% Qsub[[k]])
  Fpartial <- lapply(Fpartial, function(x) x[,seq.int(comps_gen)])


  ## Create the S3 object of class "mfa" with the desired elements
  # NB: if gen_comp < L, then the singular/eigen values and left/right singular
  #   vectors will be 'trimmed'
  object <- list(eigvals = eigvals[seq.int(comps_gen)],
                 Fcommon = Fcommon[,seq.int(comps_gen)],
                 Fpartial = Fpartial,
                 Q = Q_tilde[,seq.int(comps_gen)],
                 arglist = clist,
                 dimdata = c(num_obj = num_obj,
                             num_var = num_var),
                 K = K,
                 L = L,
                 origdata = origdata,
                 data_scale = data_scale,
                 subtable = Xsub,
                 comps_gen = comps_gen,
                 sets = sets,
                 a = a,
                 alpha = alpha
  )

  class(object) <- "mfa"
  object

}


# -----------------------------------------------------------------------------


#' @export
#  Print method for the S3 object class "mfa"
print.mfa <- function(x, ...){
  cat('object "mfa"\n')
  cat(sprintf('Data analyzed: %s,
              with %s objects (rows)
              and %s active variables (columns)\n',
              x$arglist$data, x$dimdata[1], x$dimdata[2]))
  cat(sprintf('Number of sub-tables analyzed:  %s\n', x$K))
  cat(sprintf('Number of components generated: %s\n', x$comps_gen))

  if (!("center" %in% x$arglist)) center <- "mean-centering" else
    if (length(x$arglist$center) > 1) center <- "subtract specific values" else
      if (x$arglist$center == "TRUE") center <- "mean-centering" else
        if (x$arglist$center == "FALSE") center <- "None" else
          center <- "subtract specific values"
  cat(sprintf('Column centering:               %s\n', center))

  if (!("scale" %in% x$arglist)) scale <- "divide by standard deviation" else
    if (length(x$arglist$scale) > 1) scale <- "divide by specific values" else
      if (x$arglist$scale == "TRUE") scale <- "divide by standard deviation" else
        if (x$arglist$scale == "FALSE") scale <- "None" else
          if (x$arglist$scale == "vector.norm") scale <- "divide by vector norm" else
            scale <- "divide by specific values"
  cat(sprintf('Column scaling:                 %s\n', scale))

  invisible(x)
}
