context("MFA arguments")

# testing helper functions for data processing

# check matrix
test_that("check matrices", {
  expect_true(stop.ismatrix(matrix(c(1,2,3,4), nrow = 2), rows = 2, cols = 2))
  expect_true(stop.ismatrix(matrix(c(1,2,3,4,5,6), nrow = 3), rows = 3))
  expect_error(stop.ismatrix(matrix(c(1,2,3,4,5,6), nrow = 3), rows = 2))
  expect_error(stop.ismatrix(c(1,2,3,4)))
  expect_error(stop.ismatrix(list(5)))
})


# check list
test_that("check lists", {
  expect_true(stop.islist(list(5)))
  expect_true(stop.islist(list(1,2,3,4), len = 4))
  expect_error(stop.islist(matrix(c(1,2,3,4), nrow = 2)))
  expect_error(stop.islist(c(1,2,3,4)))
  expect_error(stop.islist(list(1,2,3,4), len = 3))
})

# check vector
test_that("check vector", {
  expect_true(stop.isvector("stat243"))
  expect_true(stop.isvector(c(1,2,3,4)))
  expect_error(stop.isvector(matrix(c(1,2,3,4), nrow = 2)))
  expect_error(stop.isvector(c(1,2,3,4), len = 3))
})

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

# check mfa arguments
test_that("check arguments of mfa object", {
  expect_true(stop.ismfa(mfa(winedata, sets.mixed)))
  expect_error(mfa(winedata, sets.mixed, ncomps = 22))
  expect_error(mfa(winedata, c(sets.mixed, 181805050)))
  expect_error(mfa(winedata, sets.mixed, scale = 1:52))
  expect_error(mfa(winedata, sets.mixed, scale = 1:54))
})


context("Wine data results")

my.mfa <- mfa(winedata, sets.mixed)

my.partial.F <- round(my.mfa$Fpartial[[1]][,1:2],3)
row.names(my.partial.F) <- NULL
author.partial.F <- matrix(c(-1.037, -1.179, -.213, -.946, 1.546, 1.176, 0.698,
                             1.006, -.922, .189, -.643, .323, .155, .596, -.104,
                             .446, -.676, -.747, .166, -.063, .486, -.936, .640,
                             .036), ncol = 2)

my.common.F <- round(my.mfa$Fcommon[,1:2], 3)
row.names(my.partial.F) <- NULL
author.common.F <- matrix(c(-.980, -.809, -.761, -1.115, 1.373, 1.264, .808,
                            .925, -.669, .073, -.476, .367, .163, .033, -.454,
                            -.166, -.128, -.108, .205, .408, .369, -.757, .513,
                            -.076), ncol = 2)

my.Q <- round(my.mfa$Q[,1:2], 3)
author.Q <- matrix(c(-0.294, -0.267, -0.260, 0.241, 0.286, -0.233, -0.297,
                     -0.296, -0.267, 0.256, -0.238, -0.222, -0.305, -0.136,
                     -0.258, 0.203, -0.277,  0.267, -0.313, -0.261, -0.303,
                     0.230, -0.205, -0.296, -0.213, -0.268, 0.124, -0.259,
                     0.177, -0.302, -0.277, -0.265, 0.231, -0.205, -0.275,
                     -0.246, -0.277, 0.180, -0.276, -0.247, -0.235, 0.138,
                     -0.286, 0.239, -0.303, -0.235, -0.287, 0.251, -0.296,
                     -0.323, -0.274, -0.286,  0.282, 0.318, -0.248,  0.396,
                     -0.184,  0.161, 0.129, 0.183, -0.178, 0.200, -0.240,
                     -0.113, -0.333, 0.234, -0.228, 0.379, -0.365, -0.297,
                     0.283, 0.082, -0.353, -0.169, 0.066, -0.117, 0.201,
                     -0.249, 0.258, 0.132, -0.144, 0.019,  0.215, -0.274,
                     0.328,  0.031, -0.340, 0.380, -0.410, 0.290, 0.376, 0.309,
                     -0.376, 0.231, -0.219, -0.261, 0.293, 0.241, -0.221, 0.226,
                     -0.083, -0.188,  0.080, -0.262, 0.187, 0.272), ncol = 2)

author.eig <- c(0.77, 0.123, 0.091, 0.076, 0.06, 0.039, 0.031, 0.025, 0.019,
                0.013, 0.011)


test_that("compare results of mfa", {
  expect_equivalent(my.partial.F, author.partial.F)
  expect_equivalent(my.common.F, author.common.F)
  expect_equivalent(my.Q, author.Q)
  expect_equivalent(round(eigsum(mfa(winedata, sets.mixed),
                                 verbose = FALSE)[2,1:11], 3), author.eig)
})
