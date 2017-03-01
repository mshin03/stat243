## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(mfaMKTLT)

## ----initial-------------------------------------------------------------
sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
  c(35:38), c(39:44), c(45:49), c(50:53))
mfa1 <- mfa(data = winedata, sets = sets.num)
mfa1

## ----common1-------------------------------------------------------------
plot(mfa1, type = "compromise", mytitle = "Common Factor Scores")

## ----common2-------------------------------------------------------------
plot(mfa1, type = "compromise", xdim = 3, ydim = 7, size = 5, 
  mytitle = "Common Factor Scores")

## ----common3-------------------------------------------------------------
plot(mfa1, type = "compromise", legend=substr(rownames(mfa1$Fcommon),1,2),
label=substr(rownames(mfa1$Fcommon),3,3))

## ----pf1-----------------------------------------------------------------
plot(mfa1, type = "partial.factor", size = 2)
plot(mfa1, type = "partial.factor", size = 2.5, subtabs = c(1,2,4), 
     xdim = 4, ydim = 2, facetrows = 3)

## ----pf2-----------------------------------------------------------------
plot(mfa1, type = "partial.factor", size = 4, subtabs = NULL, xdim = 2, ydim = 3, 
     legend=substr(rownames(mfa1$Fpartial[[1]]),1,2), 
     label=substr(rownames(mfa1$Fcommon),3,3))

## ----loadings1-----------------------------------------------------------
plot(mfa1, type = "loadings", size = 2.5, subtabs = c(9,10), 
     legend = c("cat pee", "passion fruit", "green pepper", "mineral",
                "optional 1", "optional 2"))

## ----loadings2-----------------------------------------------------------
plot(mfa1, type = "eigenvalues")

## ----com.pf1-------------------------------------------------------------
plot(mfa1, type = "compromise.partial", xdim = 1, ydim = 10)

## ----com.pf2-------------------------------------------------------------
plot(mfa1, type = "compromise.partial", xdim = 1, ydim = 2, size = 5,
     legend=substr(rownames(mfa1$Fcommon),1,2), label=substr(rownames(mfa1$Fcommon),3,3))

## ----loadings------------------------------------------------------------
plot(mfa1, type= "bootstrap", bootstrap_size = 2000, bootstrap_comps=c(1,4), facetrows=2)

