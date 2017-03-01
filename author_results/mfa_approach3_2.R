# 243 final project

# Changes: fixed some confusion between:
#   X - the normalized but unweighted grand table 
#   X.wt - the normalized, weighted grand table

# 1.  a. read in the raw data
#     b. normalize each column. subtract mean and divide by euclidean length
#     c. create 10 separate tables, corresponding to the 10 wine critics
# 2.  a. perform standard PCA on each (normalized) sub-table. 
#     b. record the 1st singular value for each sub-table
#     c. weight each sub-table by dividing it by its 1st singular value
# 3.  a. re-concatenate the weighted subtables together into a Grand Table
#     b. do standard PCA on the (weighted) grand table. This is equivalent to a 
#         Generalized PCA on the unweighted grand table.

# rm(list = ls())
# library(stringr)
# library(hash)

##########
### PRELIMINARY FUNCTIONS
#########

# Need a function to get vector norm (euclidean length) for a vector
my.vec.norm <- function(x) sqrt(sum(x**2))

# Checks for square matrices
is.square <- function(x) is.matrix(x) & nrow(x) == ncol(x)

# calc trace 
my.trace <- function(x){
  if (!is.square(x)) stop("ma'am or sir or person of any gender identification, please use a square matrix")
  sum(diag(x))
}

## Stop messages
# Note to Lev - try to write these functions programmatically
# stop.ismatrix <- function(x) if (!is.matrix(x)) stop("this object must be in matrix form")

stop.ismatrix <- function(x, rows = NA, cols = NA){
  if (!is.matrix(x)) stop("this object must be in matrix form")
  if (!is.na(rows)) if (nrow(x) != rows) stop(paste0("this object must have ",
                                                     rows, "rows"))
  if (!is.na(cols)) if (ncol(x) != cols) stop(paste0("this object must have ",
                                                     cols, "columns"))
}


# stop.islist <- function(x) if (!is.list(x)) stop("this object must be in list form")
stop.islist <- function(x, len = NA){
  if (!is.list(x)) stop("this object must be in list form")
  if (!is.na(len)) if (length(x) != len) stop(paste0("this object must have length ", len))
}

stop.isvector <- function(x, len = NA){
  if (!is.vector(x)) stop("this object must be in vector form")
  if (!is.na(len)) if (length(x) != len) stop(paste0("this object must have length ", len))
}



##########
### STEP 1. read in the data and break it down into separate tables
##########


### 1.A - read in data
dat.orig <- read.csv("../data/wines.csv", header = T,row.names = 1, stringsAsFactors = F)
# rownames(wine) <- tolower(rownames(wine))
# names(wine) <- tolower(names(wine))
# for now I am assuming we don't need the vars acidity, pH, alcohol, and res_sugar
# wine <- as.matrix(wine[,-c(54:57)])


## SUBFUNCTION: my.lower - changes the attribute(s) of an object to lower case
my.lower <- function(object, attr.list){
  for (attr in attr.list){
    eval(parse(text = paste(attr, "(object) <- tolower(", attr, "(object))", sep = "")))
  }
  object
}
# for (attr in c("colnames", "rownames")) {
#   # print(paste(attr, "(dat) <- tolower(", attr, "(dat))", sep = ""))
#   eval(parse(text = paste(attr, "(dat) <- tolower(", attr, "(dat))", sep = "")))
# }


## SUBFUNCTION: make.data - processes the data. 
#   coerces data to a matrix if it's in a data.frame
#   changes row and column names to lower case just bc I prefer it that way
make.data <- function(dat){
  if (!is.matrix(dat) & !is.data.frame(dat)) stop("data must be either matrix or data frame")
  # if (is.matrix(dat)) lower.list <- c("colnames", "rownames")
  if (is.data.frame(dat)) dat <- as.matrix(dat)
  dat <- my.lower(dat, c("colnames", "rownames"))
  dat
}
  
# Make the wine data a matrix
my.dat <- make.data(dat.orig)
num.obj <- nrow(my.dat) # how many different rows/objects do we have data for? denoted I


### 1.C - Identify the number of sub-tables
# each table corresponds to the assessment of one critic
# exploit the fact that each critic used the first variable
my.names <- colnames(my.dat)[-(54:57)]
varnums <- !is.na(str_match(my.names, 'v1$')) | !is.na(str_match(my.names, 'v1\\.'))
cumvars <- cumsum(varnums) # this tells us which columns go in which tables
K <- max(cumvars) # number of sub-tables (should be 10)
if (K != 10) stop("There should be 10 sub-tables. Something is horribly wrong")


## Identify the 'sets,' a list of vectors. the kth vector stores the column indices for
#   the kth sub-table
sets.num <- vector("list", K)
sets.char <- vector("list", K)
for (k in seq.int(K)){
  firstcol <- min(which(cumvars == k))
  lastcol <- max(which(cumvars == k))
  sets.num[[k]] <- firstcol:lastcol
  sets.char[[k]] <- my.names[firstcol:lastcol]
}  


## SUBFUNCTION - if the user passed a list of character vectors for sets,
#   turn this into a list of numerics
# dat <- as.matrix(wine.orig)
# sets <- sets.char

# This function takes a character set list and returns a numeric one
get.sets <- function(dat, sets){
  # Check that each argument is the appropriate type of object
  stop.ismatrix(dat)
  stop.islist(sets)
  K <- length(sets) # the length of this list is the number of sub-tables
  stop.isvector(K, 1)
  for (k in seq.int(K)) if (!is.character(sets[[k]])) 
    stop("only use this function on lists of characters")
  
  # Get the variable names
  varnames <- colnames(dat)
  
  # Each variable name should be unique
  if ( mean(tolower(varnames) == unique(tolower(varnames))) != 1) 
    stop("variable names must be unique (case-insensitive)")
  
  # Get the indices associated with each variable name
  sets.numeric <- lapply(sets, function(x) which(tolower(varnames) %in% tolower(x)))
  
  # new list of sets should also have length K
  stop.islist(sets.numeric, K)
  
  # length of each vector in the character and numeric lists should be the same
  for (k in seq.int(K)) if(length(sets[[k]]) != length(sets.numeric[[k]]))
    stop("Something weird happened. Each element of the lists should have the 
         same length")
  
  # Return the numeric set list
  sets.numeric 
}

my.sets <- get.sets(as.matrix(dat.orig), sets.char)
all.equal(my.sets, sets.num) # Check the results

# the number of ACTIVE vars is equal to the total number of elements in 'sets'
num.var <- sum(sapply(my.sets, length))



### 1.B - Center and normalize each column
# Center by subtracting mean
# scale by dividing by the euclidean length

# Previous approach, for the sake of comparison
check <-  scale(my.dat, center = apply(my.dat, 2, mean), scale = FALSE)
check <- sweep(check, MARGIN = 2, 
                   STATS = apply(check, 2, my.vec.norm), FUN = "/")
# check that each column mean is 0
if (mean(abs(apply(check, 2, mean)) < 1e-10) != 1 | 
    mean(abs(apply(check, 2, my.vec.norm) -1) < 1e-10) != 1) 
  stop("Something went wrong while normalizing")




# Apply the centering first, since the scaling may depend on the centering
center = TRUE
my.dat.cntr <- scale(my.dat, center = center, scale = FALSE)

# if the user gave either a logical or a vector for 'scale' then simply pass
#   that as the scale argument in the scale function
# if the user gave the string 'vector.norm' then calculate the vector norm 
#   (euclidean length) of each active column, and pass that to the scale function
scale = "vector.norm"
if (scale == "vector.norm"){
  if (scale != "vector.norm") stop("scale must be a logical, a vector (length = 
                                   # of active variables), or the char string 
                                   'vector.norm'")
  my.scale <- apply(my.dat.cntr, 2, my.vec.norm)
  scale.text <- "apply(my.dat.cntr, 2, my.vec.norm)"
} else{
  my.scale <- scale 
  scale.text <- "scale"
}
# scale = "vector.norm"
# if (scale == "vector.norm"){
#   my.scale <- apply(my.dat.cntr, 2, my.vec.norm)
#   scale.text <- "apply(my.dat.cntr, 2, my.vec.norm)"
# } else{
#   my.scale <- scale 
#   scale.text <- "scale"
# }

check2 <- scale(my.dat.cntr, center = FALSE, scale = my.scale)
mean(check2 - check == 0) == 1


# another approach, perhaps more elegant
scale.cmd <- paste0("scale(my.dat, center = center, scale = ", scale.text,")")
my.dat.norm <- eval(parse(text = scale.cmd))
mean(my.dat.norm - check == 0) == 1
mean(my.dat.norm - data.scaled == 0) == 1

# rm(my.scale, scale.cmd, scale.text)


### Back to 1.C - make the subtables

# The old way, to check
subtab.check <- vector("list", K)
for (k in seq.int(K)){
  firstcol <- min(which(cumvars == k))
  lastcol <- max(which(cumvars == k))
  subtab.check[[k]] <- my.dat.norm[,firstcol:lastcol]
}

# The new way
# Xsub is a list of the K different sub-tables
Xsub <- lapply(my.sets, function(x) my.dat.norm[,x])
all.equal(Xsub, subtab.check)

# View some example
# print(Xsub[[1]]); print(Xsub[[5]])

# Clean up
# rm(varnums, cumvars, my.names, k, lastcol, firstcol, check, check2, my.dat, my.dat.cntr, sets.char, sets.num)


##########
### STEP 2. weight each sub-table by its first singular value
##########

### 2.A - perform standard PCA on each (normalized) sub-table. 
### 2.B - record the 1st singular value for each sub-table
### 2.C - weight each sub-table by dividing it by its 1st singular value

# Old way, to check
# Loop through each of the 10 subtables
check.tab.wt <- vector("list", K)
check.sv <- rep(NA, K)
for (k in seq.int(K)){
  check.sv[k] <- svd(subtab.check[[k]])$d[1]
  check.tab.wt[[k]] <- (check.sv[k]**-1) * subtab.check[[k]]
}

# New way 
# firstsv stores the first Singular Value of each of the K sub-tables
firstsv <- sapply(Xsub, function(x) svd(x)$d[1])
all.equal(firstsv, check.sv)

# Xsub.wt is a list of the sub-tables, weighted by their own first SVs
Xsub.wt <- sapply(seq.int(K), function(k) (firstsv[k]**-1) * Xsub[[k]])
all.equal(Xsub.wt, check.tab.wt)
# rm(check.sv, check.tab.wt, subtab.check)


# The table table weights are calculated by taking the inverse of each table's
#   first eigenvector
# Multiplying by the sqrt of alpha is eqivalent to dividing by the first SV
alpha <- firstsv**-2 # alpha is a vector of table weights



##########
### STEP 3. Generalized PCA on the grand table
##########


### 3.A - re-concatenate the weighted subtables into a (weighted) Grand Table
# X is the weighted grand table. This corresponds to X~ ("X.tilde") in Abdi p 9
mytext1 <- paste0("Xsub.wt[[", 1:K, "]]", collapse = ", ")
mytext2 <- paste0("cbind(", mytext1, ")")
mytext2
X <- eval(parse(text = mytext2)) # weighted grand table
if (!all.equal(dim(X), c(num.obj, num.var))) stop("Something's amiss with 
                                             the (weighted) grand table")


### 3.B - do standard PCA on the (weighted) grand table 
gsvd <- svd(X)
P <- gsvd$u      # Left singular vectors
Delta <- gsvd$d  # Singular Values
Q <- gsvd$v      # Right singular vectors

# Checks: t(P) %*% P = I, t(Q) %*% Q = I, X = P * Delta * Q
# NOTE TO FLEVvvvvvv - wrap this in an if-stop statement 
if (mean( round(t(P) %*% P,5) == diag(num.obj) ) != 1 |
    mean( round(t(Q) %*% Q,5) == diag(num.obj) ) != 1 |
    mean(round(P %*% diag(Delta) %*% t(Q) - X, 5) == 0) != 1)
  stop("Problem with GSVD. The following equalities should hold:
       t(P) %*% P = I
       t(Q) %*% Q = I
       X = P * Delta * Q")


# Check against the values listed in Abdi p 14 table (64)
# Seems like my results are too big by a factor of about 3.46 - sqrt(12)
# singular values 
Delta[1:2] / c(.878,.351)
abs( Delta[1:2] / c(.878,.351) - sqrt(12) ) < 0.01
# FLEVT singular vectors
round(P[,1:2],3) # my result
checkP <- read.csv("../author_results/abdi-P-matrix.csv", header = F, skip = 6)
checkP # authors' result
as.matrix(checkP) / P[,1:2]
abs( as.matrix(checkP) / P[,1:2] - sqrt(12)) < 0.01

# The RELATIVE INERTIAS are EQUAL to the author's results
eigvals <- Delta**2
cumeigvals <- cumsum(eigvals)
round(eigvals, 3)
round(cumeigvals, 3)
pcnt.inertia <- eigvals / sum(eigvals)
round(pcnt.inertia, 3)
check.pcnt.inertia <- c(61, 10, 7, 6, 5, 3, 2, 2, 1, 1, 1)/100 # Abdi Table 2
all.equal(round(pcnt.inertia, 2)[1:11], check.pcnt.inertia)


########
### THE MFA FN SHOULD RETURN AN OBJECT OF CLASS MFA WITH THE FOLLOWING OBJECTS 
########

### vector of eigenvalues - eigvals
eigvals <- Delta**2 # eigenvalues are the squared singular values 

### matrix of common factor scores—a.k.a. compromise factor scores - F
# F = P * Delta = X * A * Q -- Paper1 Eq 65

# Calculate A - the matrix of column constraints
#   alpha is a vector of table weights. alpha.k is the table weight (singular
#     value ^-2) of the kth subtable
#   "a" is a vector where each alpha.k gets repeated J.k times, where J.k is the
#     number of variables (columns) in the kth subtable
#   "A" is a diagonal matrix constructed from the vector "a"

# old way, to check
a.check <- NULL
for (k in seq.int(Xsub)) a.check <- c(a.check, rep(alpha[k], ncol(Xsub[[k]]) ))

# new way
a <- unlist(sapply(seq.int(K), function(k) rep(alpha[k], ncol(Xsub[[k]])))) 
# check that length is equal to the number of active variables
if (length(a) != num.var) stop("Problem creating A Matrix (col constraints)")
all.equal(a, a.check)
# rm(a.check)

A <- diag(a) # "A" is the diagonal matrix of "a"

F.common <- P %*% diag(Delta)

## Check that F = X * A * Q - THIS CHECK IS NOT WORKING
# where X is the weighted grand table 
round(F.common / (X %*% A %*% Q), 2)
round(F.common / (data.scaled[,-c(54:57)] %*% A %*% Q), 2)
# where X is the un-weighted grand table 
round(F.common - (my.dat.norm[,-(54:57)]%*% A %*% Q), 2)

## Check against the authors' F values - these are nearly identical
checkF <- read.csv("../author_results/abdi-F-65.csv", header = F, skip = 6, stringsAsFactors = FALSE)[,1:2]
mean(abs(as.matrix(checkF) - F.common[,1:2]) < 0.01) == 1




### matrix of partial factors scores -- Paper1 eq. 22
# F.k = K * alpha.k * X.k * Q.k

# We need the Q sub-matrices of singular vectors for the the sub-tables
#   The overall Q matrix can be expressed as a column block matrix made of
#   the K different Q sub-matrices - Paper1, Page 5, figure (15)
Qsub <- lapply(my.sets, function(x) Q[x,])
all.equal(Qsub[[1]], Q[1:6,])

mytext1 <- paste0("Qsub[[", 1:K, "]]", collapse = ", ")
mytext2 <- paste0("rbind(", mytext1, ")")
mytext2
Q.check <- eval(parse(text = mytext2)) # weighted grand table
all.equal(Q.check, Q)
# rm(mytext1, mytext2, Q.check)

check1 <- K * alpha[1] * Xsub[[1]] %*% Qsub[[1]]
# F.partial <- sapply(seq.int(K), function(k)  Xsub[[k]] %*% Qsub[[k]], simplify = "array")
# F.partial.array <- sapply(seq.int(K), function(k)  K * alpha[1] * Xsub[[k]] %*% Qsub[[k]], simplify = "array")
F.partial <- lapply(seq.int(K), function(k)  K * alpha[1] * Xsub[[k]] %*% Qsub[[k]])
all.equal(F.partial[[1]], check1)

# authors' result - OFF BY A FACTOR of about 0.203
checkF.partial <- read.csv("../author_results/abdi-partialF-66.csv", header = F, skip = 6, stringsAsFactors = FALSE)[,1:2]
check1 <-  Xsub[[1]] %*% Qsub[[1]]
check1 / checkF.partial
mean(abs(check1 / checkF.partial - .203) < 1e-02) == 1

# Loadings - Q
# authors' result - OFF BY ABOUT A FACTOR OF 0.5
checkQ.raw <-  read.csv("../author_results/abdi-Q-table3.csv", header = F, skip = 6, 
                    stringsAsFactors = FALSE, sep = " ")
c1 <- unlist(sapply(c(1,3,5), function(x) checkQ.raw[x,]))
c1 <- c1[!is.na(c1)]
c2 <- unlist(sapply(c(2,4,6), function(x) checkQ.raw[x,]))
c2 <- c2[!is.na(c2)]
# checkQ <- cbind(unlist(checkQ.raw[c(1,3,5),]), unlist(checkQ.raw[c(2,4,6),]))
checkQ <- cbind(c1, c2)
# rm(checkQ.raw, c1, c2)
head(Q[,1:2])
head(checkQ)
mean(Q[,1:2] / checkQ - .5 < 0.1)


