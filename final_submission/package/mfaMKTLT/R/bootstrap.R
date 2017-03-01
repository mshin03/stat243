#' @title Bootstrap
#'
#' @description To estimate the stability of the compromise factor scores,
#'   we use bootstrap approach
#'
#' @param datalist Partial factor scores tables that we are sampling from
#' @param n Bootstrap sample number
#'
#' @return
#'   \describe{
#'     \item{t_value}{Bootstrap ratios, which, like t statistics,
#'     can be used to find the observations that reliably contribute
#'     to a given component.}
#'     \item{bt_mean}{Mean of the bootstrap samples}
#'     \item{bt_sd}{Standard deviation of the bootstrap samples}
#'     }
#'
#' @export
#'
#' @examples
#' # Create an mfa object.
#' sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
#'   c(35:38), c(39:44), c(45:49), c(50:53))
#' mfa1 <- mfa(winedata, sets.num)
#' bootstrap(mfa1$Fpartial,1000)
#'


bootstrap <- function(datalist, n){
  ll <- list()
  l <- length(datalist)
  #Bootstrap sampling
  for (j in 1:n){
    t <- sample(c(1:l), l, TRUE)
    s <- datalist[[t[1]]]
    for (i in 2:l){
      s <- s + datalist[[t[i]]]
    }
    fs <- paste("F",j,sep="")
    assign(fs,s/l)
    ll[[j]] <- get(paste("F",j,sep=""))
  }
  #Bootstrap mean
  bt_mean <- Reduce('+',ll)/n
  #Bootstrap var
  bt_var1 <- lapply(ll,function(x){(x-bt_mean)^2})
  bt_var <- Reduce('+',bt_var1)/n
  #Return result as a list
  result<- list(
    t_value = bt_mean/sqrt(bt_var),
    bt_mean = bt_mean,
    bt_sd= sqrt(bt_var)
  )
  return(result)
}
