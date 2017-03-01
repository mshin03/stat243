## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(mfaMKTLT)

## ----prelim1-------------------------------------------------------------
head(winedata[1:12])
is.data.frame(winedata)

## ----sets1---------------------------------------------------------------
sets.num <- list(c(1:6), c(7:12), c(13:18), c(19:23), c(24:29), c(30:34),
  c(35:38), c(39:44), c(45:49), c(50:53))
sets.char <- list(c('V1','V2','V3','V4','V5','V6'),
  c('V1.1','V2.1','V3.1','V4.1','V7','V8'),
  c('V1.2','V2.2','V3.2','V4.2','V9','V10'),
  c('V1.3','V2.3','V3.3','V4.3','V8.1'),
  c('V1.4','V2.4','V3.4','V4.4','V11','V12'),
  c('V1.5','V2.5','V3.5','V4.5','V13'),
  c('V1.6','V2.6','V3.6','V4.6'),
  c('V1.7','V2.7','V3.7','V4.7','V14','V5.1'),
  c('V1.8','V2.8','V3.8','V4.8','V15'),
  c('V1.9','V2.9','V3.9','V4.9'))
sets.mixed <- c(sets.num[1:4], sets.char[5:10])

## ----sets2---------------------------------------------------------------
all.equal(unlist(sets.num), unique(unlist(sets.num)))
all.equal(unlist(sets.char), unique(unlist(sets.char)))
head(winedata[c("acidity", "ph", "alcohol", "res_sugar")])

## ----mfa1----------------------------------------------------------------
mfa1 <- mfa(data = winedata, sets = sets.num)
mfa1

## ----mfa2----------------------------------------------------------------
names(mfa1)
help(mfa, package = "mfaMKTLT")

## ----mfa3----------------------------------------------------------------
head(mfa1$eigvals)
head(mfa1$Fcommon[,1:5])
head(mfa1$Fpartial[[1]][,1:5])
head(mfa1$Q[,1:5])

## ----sum1----------------------------------------------------------------
eigsum <- eigsum(mfa1)
round(eigsum[, 1:5], 3)

## ----sum2----------------------------------------------------------------
# The [i,j] element is the contribution of observation i to component j.
obscont <- obscont(mfa1) 
round(obscont[1:5, 1:5], 3) 

# The [i,j] element is the contribution of variable i to component j.
varcont <- varcont(mfa1) 
round(varcont[1:5, 1:5], 3) 

# The [i,j] element is the contribution of sub-table i to component j.
tabcont <- tabcont(mfa1) 
round(tabcont[1:5, 1:5], 3) 

## ----sum3----------------------------------------------------------------
rv_coeff(mfa1$subtable[[2]], mfa1$subtable[[5]])
lg_coeff(mfa1$subtable[[2]], mfa1$subtable[[5]])

## ----sum4----------------------------------------------------------------
# rv coefficients
gentable(mfa1$data_scale, as.list(c(sets.num[1], sets.char[2:3])),
          paste0("sub-table_", 1:3), func = rv_coeff)

# lg coefficients
gentable(mfa1$data_scale, as.list(c(sets.num[1], sets.char[2:3])),
          paste0("sub-table_", 1:3), func = lg_coeff)

