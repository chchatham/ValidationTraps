simulate_trap3 <- function(
  n = 5000, prevalence = 0.10,
  k_markers = 4,
  sensitivities = c(0.90, 0.80, 0.70, 0.95),
  specificities = c(0.60, 0.85, 0.90, 0.50),
  marker_correlations = NULL,
  combination_rule = c("and", "or", "majority", "weighted", "logistic"),
  weights = NULL,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  combination_rule <- match.arg(combination_rule)
  k <- k_markers

  sensitivities <- sensitivities[1:k]
  specificities <- specificities[1:k]

  truth <- rbinom(n, 1, prevalence)
  n_pos <- sum(truth)
  n_neg <- n - n_pos

  if (n_pos < 5 || n_neg < 5) {
    stop("Too few cases in one group. Adjust n or prevalence.")
  }

  thresholds <- qnorm(specificities)
  d_values <- thresholds + qnorm(sensitivities)

  if (!is.null(marker_correlations)) {
    validate_pd <- function(mat, name) {
      eigs <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
      if (any(eigs < -1e-10)) {
        stop(paste(name, "is not positive-definite."))
      }
    }
    validate_pd(marker_correlations, "Marker correlation matrix")
    scores_neg <- MASS::mvrnorm(n_neg, mu = rep(0, k), Sigma = marker_correlations)
    scores_pos <- MASS::mvrnorm(n_pos, mu = d_values, Sigma = marker_correlations)
  } else {
    scores_neg <- matrix(rnorm(n_neg * k), n_neg, k)
    scores_pos <- matrix(rnorm(n_pos * k, mean = rep(d_values, each = n_pos)), n_pos, k)
  }

  latent_scores <- matrix(0, n, k)
  latent_scores[truth == 0, ] <- scores_neg
  latent_scores[truth == 1, ] <- scores_pos

  markers <- matrix(0L, n, k)
  for (j in 1:k) {
    markers[, j] <- as.integer(latent_scores[, j] >= thresholds[j])
  }

  if (combination_rule %in% c("logistic", "weighted")) {
    if (combination_rule == "weighted") {
      if (is.null(weights)) weights <- rep(1 / k, k)
      combined_score <- as.numeric(latent_scores %*% weights)
      opt_thresh <- optimize_threshold(combined_score, truth)
      combined <- as.integer(combined_score >= opt_thresh)
    } else {
      half <- n %/% 2
      train_idx <- 1:half
      test_idx <- (half + 1):n
      df <- data.frame(truth = truth, latent_scores)
      names(df)[2:(k + 1)] <- paste0("M", 1:k)
      fit <- tryCatch(
        glm(truth ~ ., data = df[train_idx, ], family = binomial),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        combined_score <- rowMeans(latent_scores)
        combined <- as.integer(combined_score > median(combined_score))
      } else {
        combined_score <- predict(fit, newdata = df, type = "response")
        combined <- as.integer(combined_score >= 0.5)
      }
    }
  } else {
    combined <- switch(combination_rule,
      "and" = as.integer(rowSums(markers) == k),
      "or" = as.integer(rowSums(markers) > 0),
      "majority" = as.integer(rowSums(markers) > k / 2)
    )
    combined_score <- rowSums(latent_scores)
  }

  individual_rocs <- lapply(1:k, function(j) {
    pROC::roc(truth, latent_scores[, j], quiet = TRUE, direction = "<")
  })
  combined_roc <- pROC::roc(truth, combined_score, quiet = TRUE, direction = "<")

  calc_metrics <- function(pred, actual) {
    tp <- sum(pred == 1 & actual == 1)
    fp <- sum(pred == 1 & actual == 0)
    tn <- sum(pred == 0 & actual == 0)
    fn <- sum(pred == 0 & actual == 1)
    list(
      sens = if (tp + fn > 0) tp / (tp + fn) else NA_real_,
      spec = if (tn + fp > 0) tn / (tn + fp) else NA_real_,
      ppv  = if (tp + fp > 0) tp / (tp + fp) else NA_real_,
      npv  = if (tn + fn > 0) tn / (tn + fn) else NA_real_
    )
  }

  ind <- lapply(1:k, function(j) calc_metrics(markers[, j], truth))
  comb <- calc_metrics(combined, truth)

  list(
    markers = markers,
    truth = truth,
    combined = combined,
    latent_scores = latent_scores,
    combined_score = combined_score,
    individual_rocs = individual_rocs,
    combined_roc = combined_roc,
    stats = list(
      individual_sens = sapply(ind, `[[`, "sens"),
      individual_spec = sapply(ind, `[[`, "spec"),
      individual_ppv  = sapply(ind, `[[`, "ppv"),
      individual_npv  = sapply(ind, `[[`, "npv"),
      combined_sens = comb$sens,
      combined_spec = comb$spec,
      combined_ppv  = comb$ppv,
      combined_npv  = comb$npv
    )
  )
}

optimize_threshold <- function(scores, truth) {
  roc_obj <- pROC::roc(truth, scores, quiet = TRUE, direction = "<")
  coords <- pROC::coords(roc_obj, "best", ret = "threshold",
                         best.method = "youden")
  as.numeric(coords[1])
}
