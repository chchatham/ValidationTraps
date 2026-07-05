simulate_trap1 <- function(
  n = 1e5, var_T = 1.0, r_T = 0.95,
  var_dT = 0.25, r_dT = 0.05,
  var_S = 0.015, var_e = 0.05,
  mean_ch = 1, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  Sigma_T <- matrix(c(var_T, r_T * var_T, r_T * var_T, var_T), 2, 2)
  Sigma_dT <- matrix(c(var_dT, r_dT * var_dT, r_dT * var_dT, var_dT), 2, 2)

  validate_pd <- function(mat, name) {
    eigs <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
    if (any(eigs < -1e-10)) {
      stop(paste(name, "is not positive-definite. Adjust parameters."))
    }
    mat
  }
  validate_pd(Sigma_T, "Trait covariance matrix")
  validate_pd(Sigma_dT, "Change covariance matrix")

  traits <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = Sigma_T)
  T_x <- traits[, 1]
  T_y <- traits[, 2]

  changes <- MASS::mvrnorm(n, mu = c(mean_ch, mean_ch), Sigma = Sigma_dT)
  dT_x <- changes[, 1]
  dT_y <- changes[, 2]

  S1 <- rnorm(n, 0, sqrt(var_S))
  S2 <- rnorm(n, 0, sqrt(var_S))

  e1_x <- rnorm(n, 0, sqrt(var_e))
  e1_y <- rnorm(n, 0, sqrt(var_e))
  e2_x <- rnorm(n, 0, sqrt(var_e))
  e2_y <- rnorm(n, 0, sqrt(var_e))

  X1 <- T_x + S1 + e1_x
  Y1 <- T_y + e1_y
  X2 <- (T_x + dT_x) + S2 + e2_x
  Y2 <- (T_y + dT_y) + e2_y

  list(
    X1 = X1, Y1 = Y1, X2 = X2, Y2 = Y2,
    stats = list(
      trt_X = cor(X1, X2),
      trt_Y = cor(Y1, Y2),
      conv_T1 = cor(X1, Y1),
      conv_T2 = cor(X2, Y2),
      change_corr = cor(X2 - X1, Y2 - Y1)
    )
  )
}
