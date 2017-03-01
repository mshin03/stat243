# Private function : calculate vector norm (euclidean length)
vec_norm <- function(x){
  stop.isvector(x)
  if (!is.numeric(x))
    stop("vector norm can only be calculated for numeric vectors")
  sqrt(sum(x**2))
}


# Private function : check whether object is a square matrix
is.square <- function(x) if (is.matrix(x)) nrow(x) == ncol(x) else FALSE


# Private function : calculate trace of a matrix
my_trace <- function(x){
  if (!is.square(x)) stop("ma'am or sir, please use a square matrix")
  sum(diag(x))
}


# Private function : makes the specified attributes of an object lower case
#   For example, the row- and column-names of a matrix
make_lowercase <- function(object, attr.list){
for (attr in attr.list){
  eval(parse(text = paste(attr, "(object) <- tolower(", attr, "(object))", sep = "")))
}
object
}
