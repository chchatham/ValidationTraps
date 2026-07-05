simulate_trap2 <- function(
  n_subjects = 200, n_timepoints = 5,
  var_between = 1.0,
  var_within = 0.3,
  r_cross = 0.0,
  r_longitudinal = 0.8,
  var_e = 0.1, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  Sigma_between <- matrix(
    c(var_between, r_cross * var_between,
      r_cross * var_between, var_between), 2, 2
  )
  Sigma_within <- matrix(
    c(var_within, r_longitudinal * var_within,
      r_longitudinal * var_within, var_within), 2, 2
  )

  validate_pd <- function(mat, name) {
    eigs <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
    if (any(eigs < -1e-10)) {
      stop(paste(name, "is not positive-definite. Adjust parameters."))
    }
    mat
  }
  validate_pd(Sigma_between, "Between-person covariance matrix")
  validate_pd(Sigma_within, "Within-person covariance matrix")

  intercepts <- MASS::mvrnorm(n_subjects, mu = c(0, 0), Sigma = Sigma_between)
  slopes <- MASS::mvrnorm(n_subjects, mu = c(0, 0), Sigma = Sigma_within)

  time_vec <- 1:n_timepoints
  time_centered <- time_vec - mean(time_vec)

  data <- data.frame(
    subject = rep(1:n_subjects, each = n_timepoints),
    time = rep(time_vec, times = n_subjects)
  )
  data$time_centered <- rep(time_centered, times = n_subjects)

  data$X <- intercepts[data$subject, 1] +
    slopes[data$subject, 1] * data$time_centered +
    rnorm(nrow(data), 0, sqrt(var_e))
  data$Y <- intercepts[data$subject, 2] +
    slopes[data$subject, 2] * data$time_centered +
    rnorm(nrow(data), 0, sqrt(var_e))

  mid_time <- ceiling(n_timepoints / 2)
  mid_data <- data[data$time == mid_time, ]
  r_cross_obs <- cor(mid_data$X, mid_data$Y)

  first_data <- data[data$time == 1, ]
  last_data <- data[data$time == n_timepoints, ]
  delta_X <- last_data$X - first_data$X
  delta_Y <- last_data$Y - first_data$Y
  r_within_obs <- cor(delta_X, delta_Y)

  icc_oneway <- function(values, groups) {
    group_f <- factor(groups)
    k <- nlevels(group_f)
    ni <- length(values) / k
    group_means <- tapply(values, group_f, mean)
    grand_mean <- mean(values)
    MS_b <- ni * sum((group_means - grand_mean)^2) / (k - 1)
    MS_w <- sum((values - group_means[group_f])^2) / (k * (ni - 1))
    max(0, (MS_b - MS_w) / (MS_b + (ni - 1) * MS_w))
  }

  list(
    data = data,
    delta_X = delta_X,
    delta_Y = delta_Y,
    stats = list(
      r_cross_obs = r_cross_obs,
      r_within_obs = r_within_obs,
      icc_x = icc_oneway(data$X, data$subject),
      icc_y = icc_oneway(data$Y, data$subject)
    )
  )
}
