# Elastic Net extension utilities for the analysis report.
# This script assumes get_at_time(), preds, and index objects are available
# in the calling R Markdown environment.

build_log_frame <- function(t, preds, index) {
  df_t <- get_at_time(t, preds, index)

  out <- dplyr::transmute(
    df_t,
    Revenue = Revenue,
    y_log = log1p(Revenue),
    log_V = log1p(V),
    log_U = log1p(U),
    log_R = log1p(R),
    log_E = log1p(E),
    log_Theaters = log1p(Theaters)
  )

  out[stats::complete.cases(out), , drop = FALSE]
}

build_raw_frame <- function(t, preds, index) {
  df_t <- get_at_time(t, preds, index)

  out <- dplyr::transmute(
    df_t,
    Revenue = Revenue,
    y_log = log1p(Revenue),
    raw_V = V,
    raw_U = U,
    raw_R = R,
    raw_E = E,
    raw_Theaters = Theaters
  )

  out[stats::complete.cases(out), , drop = FALSE]
}

compute_cv_metrics <- function(y_log_true, y_log_pred) {
  ybar <- mean(y_log_true)
  ss_res <- sum((y_log_true - y_log_pred)^2)
  ss_tot <- sum((y_log_true - ybar)^2)
  r2_log <- ifelse(ss_tot > 0, 1 - ss_res / ss_tot, NA_real_)

  y_usd_true <- expm1(y_log_true)
  y_usd_pred <- pmax(expm1(y_log_pred), 0)

  mae_usd <- mean(abs(y_usd_true - y_usd_pred))
  rmse_usd <- sqrt(mean((y_usd_true - y_usd_pred)^2))

  tibble::tibble(
    r2_log = r2_log,
    mae_usd = mae_usd,
    rmse_usd = rmse_usd
  )
}

evaluate_ols_log_cv <- function(dat, fold_id) {
  pred_log <- rep(NA_real_, nrow(dat))

  for (fold in sort(unique(fold_id))) {
    train_idx <- which(fold_id != fold)
    test_idx <- which(fold_id == fold)

    train_dat <- dat[train_idx, , drop = FALSE]
    test_dat <- dat[test_idx, , drop = FALSE]

    if (nrow(train_dat) < 5 || nrow(test_dat) < 2) next

    fit <- stats::lm(
      y_log ~ log_V + log_U + log_R + log_E + log_Theaters,
      data = train_dat
    )

    pred_log[test_idx] <- as.numeric(stats::predict(fit, newdata = test_dat))
  }

  keep <- !is.na(pred_log)
  compute_cv_metrics(dat$y_log[keep], pred_log[keep])
}

evaluate_elnet_nested_cv <- function(
  dat,
  feature_cols,
  fold_id,
  alpha_grid = c(0, 0.25, 0.5, 0.75, 1),
  inner_nfolds = 10
) {
  pred_log <- rep(NA_real_, nrow(dat))
  coef_rows <- list()
  tune_rows <- list()

  for (fold in sort(unique(fold_id))) {
    train_idx <- which(fold_id != fold)
    test_idx <- which(fold_id == fold)

    train_dat <- dat[train_idx, , drop = FALSE]
    test_dat <- dat[test_idx, , drop = FALSE]

    if (nrow(train_dat) < 20 || nrow(test_dat) < 2) next

    x_train <- as.matrix(train_dat[, feature_cols, drop = FALSE])
    y_train <- train_dat$y_log
    x_test <- as.matrix(test_dat[, feature_cols, drop = FALSE])

    inner_fits <- lapply(alpha_grid, function(a) {
      cv_fit <- glmnet::cv.glmnet(
        x = x_train,
        y = y_train,
        alpha = a,
        nfolds = min(inner_nfolds, nrow(train_dat)),
        family = "gaussian",
        standardize = TRUE
      )

      list(
        alpha = a,
        lambda = cv_fit$lambda.min,
        mse = min(cv_fit$cvm),
        fit = cv_fit
      )
    })

    mse_vals <- vapply(inner_fits, function(x) x$mse, numeric(1))
    best <- inner_fits[[which.min(mse_vals)]]

    pred_log[test_idx] <- as.numeric(
      stats::predict(best$fit, newx = x_test, s = "lambda.min")
    )

    coef_mat <- as.matrix(stats::coef(best$fit, s = "lambda.min"))
    coef_rows[[length(coef_rows) + 1]] <- tibble::tibble(
      outer_fold = fold,
      feature = rownames(coef_mat)[-1],
      coef = as.numeric(coef_mat[-1, 1])
    )

    tune_rows[[length(tune_rows) + 1]] <- tibble::tibble(
      outer_fold = fold,
      alpha = best$alpha,
      lambda = best$lambda,
      inner_mse = best$mse
    )
  }

  keep <- !is.na(pred_log)
  metrics <- compute_cv_metrics(dat$y_log[keep], pred_log[keep])

  coef_summary <- dplyr::bind_rows(coef_rows)
  if (nrow(coef_summary) > 0) {
    coef_summary <- coef_summary |>
      dplyr::group_by(feature) |>
      dplyr::summarise(
        selection_rate = mean(abs(coef) > 1e-8),
        mean_coef = mean(coef),
        mean_abs_coef = mean(abs(coef)),
        .groups = "drop"
      )
  }

  tuning <- dplyr::bind_rows(tune_rows)
  tuning_summary <- tibble::tibble(
    mean_selected_alpha = mean(tuning$alpha),
    median_selected_lambda = stats::median(tuning$lambda),
    mean_inner_mse = mean(tuning$inner_mse)
  )

  list(
    metrics = metrics,
    coef_summary = coef_summary,
    tuning = tuning,
    tuning_summary = tuning_summary
  )
}

run_extension_grid <- function(
  preds,
  index,
  time_points = -30:10,
  key_time_points = c(-30, -14, -7, 0),
  seed = 2026
) {
  set.seed(seed)

  extension_rows <- list()
  enet_coef_store <- list()
  enet_tuning_store <- list()

  for (t in time_points) {
    dat_t <- build_log_frame(t, preds, index)
    fold_id <- sample(rep(1:10, length.out = nrow(dat_t)))

    ols_metrics <- evaluate_ols_log_cv(dat_t, fold_id) |>
      dplyr::mutate(Model = "Baseline OLS", t = t)

    enet_out <- evaluate_elnet_nested_cv(
      dat = dat_t,
      feature_cols = c("log_V", "log_U", "log_R", "log_E", "log_Theaters"),
      fold_id = fold_id
    )
    enet_metrics <- enet_out$metrics |>
      dplyr::mutate(Model = "Elastic Net", t = t)

    extension_rows[[length(extension_rows) + 1]] <- dplyr::bind_rows(
      ols_metrics,
      enet_metrics
    )

    enet_tuning_store[[length(enet_tuning_store) + 1]] <- enet_out$tuning |>
      dplyr::mutate(t = t)

    if (t %in% key_time_points) {
      enet_coef_store[[as.character(t)]] <- enet_out$coef_summary |>
        dplyr::mutate(t = t)
    }
  }

  list(
    extension_results = dplyr::bind_rows(extension_rows),
    enet_coef_key = dplyr::bind_rows(enet_coef_store),
    enet_tuning_records = dplyr::bind_rows(enet_tuning_store),
    key_time_points = key_time_points
  )
}

run_setup_comparison <- function(
  preds,
  index,
  key_time_points = c(-30, -14, -7, 0),
  seed = 2026
) {
  set.seed(seed + 1000)

  rows <- list()
  tuning_rows <- list()

  for (t in key_time_points) {
    dat_log <- build_log_frame(t, preds, index)
    dat_raw <- build_raw_frame(t, preds, index)

    fold_log <- sample(rep(1:10, length.out = nrow(dat_log)))
    fold_raw <- sample(rep(1:10, length.out = nrow(dat_raw)))

    out_log <- evaluate_elnet_nested_cv(
      dat = dat_log,
      feature_cols = c("log_V", "log_U", "log_R", "log_E", "log_Theaters"),
      fold_id = fold_log
    )
    out_raw <- evaluate_elnet_nested_cv(
      dat = dat_raw,
      feature_cols = c("raw_V", "raw_U", "raw_R", "raw_E", "raw_Theaters"),
      fold_id = fold_raw
    )

    rows[[length(rows) + 1]] <- tibble::tibble(
      t = t,
      setup = "Log-transformed",
      r2_log = out_log$metrics$r2_log,
      mae_usd = out_log$metrics$mae_usd,
      rmse_usd = out_log$metrics$rmse_usd
    )
    rows[[length(rows) + 1]] <- tibble::tibble(
      t = t,
      setup = "Raw-scale",
      r2_log = out_raw$metrics$r2_log,
      mae_usd = out_raw$metrics$mae_usd,
      rmse_usd = out_raw$metrics$rmse_usd
    )

    tuning_rows[[length(tuning_rows) + 1]] <- out_log$tuning |>
      dplyr::mutate(t = t, setup = "Log-transformed")
    tuning_rows[[length(tuning_rows) + 1]] <- out_raw$tuning |>
      dplyr::mutate(t = t, setup = "Raw-scale")
  }

  setup_results <- dplyr::bind_rows(rows)
  setup_long <- setup_results |>
    tidyr::pivot_longer(
      cols = c(r2_log, mae_usd, rmse_usd),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      metric = dplyr::recode(
        metric,
        r2_log = "CV R2 (log scale)",
        mae_usd = "MAE (USD)",
        rmse_usd = "RMSE (USD)"
      )
    )

  tuning_records <- dplyr::bind_rows(tuning_rows)
  setup_summary <- setup_results |>
    dplyr::group_by(setup) |>
    dplyr::summarise(
      mean_r2_log = mean(r2_log),
      mean_mae_usd = mean(mae_usd),
      mean_rmse_usd = mean(rmse_usd),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_r2_log))

  selected_setup <- setup_summary$setup[[1]]

  tuning_summary <- tuning_records |>
    dplyr::group_by(setup) |>
    dplyr::summarise(
      mean_selected_alpha = mean(alpha),
      median_selected_lambda = stats::median(lambda),
      .groups = "drop"
    )

  list(
    setup_results = setup_results,
    setup_long = setup_long,
    setup_summary = setup_summary,
    selected_setup = selected_setup,
    tuning_summary = tuning_summary,
    tuning_records = tuning_records
  )
}

make_setup_selection_table <- function(setup_summary) {
  setup_summary |>
    dplyr::mutate(
      mean_r2_log = round(mean_r2_log, 3),
      mean_mae_usd = round(mean_mae_usd, 0),
      mean_rmse_usd = round(mean_rmse_usd, 0)
    )
}

make_extension_key_table <- function(extension_results, key_time_points) {
  extension_results |>
    dplyr::filter(t %in% key_time_points) |>
    dplyr::select(t, Model, r2_log, mae_usd, rmse_usd) |>
    tidyr::pivot_longer(
      cols = c(r2_log, mae_usd, rmse_usd),
      names_to = "Metric",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Metric = dplyr::recode(
        Metric,
        r2_log = "CV R2 (log scale)",
        mae_usd = "MAE (USD)",
        rmse_usd = "RMSE (USD)"
      )
    ) |>
    tidyr::pivot_wider(names_from = Model, values_from = Value) |>
    dplyr::arrange(t, Metric) |>
    dplyr::mutate(
      `Baseline OLS` = ifelse(
        grepl("R2", Metric),
        round(`Baseline OLS`, 3),
        round(`Baseline OLS`, 0)
      ),
      `Elastic Net` = ifelse(
        grepl("R2", Metric),
        round(`Elastic Net`, 3),
        round(`Elastic Net`, 0)
      )
    )
}

summarize_enet_features <- function(enet_coef_key) {
  feature_labels <- c(
    log_V = "log1p(V)",
    log_U = "log1p(U)",
    log_R = "log1p(R)",
    log_E = "log1p(E)",
    log_Theaters = "log1p(Theaters)"
  )

  enet_coef_key |>
    dplyr::group_by(feature) |>
    dplyr::summarise(
      avg_selection_rate = mean(selection_rate),
      avg_abs_coef = mean(mean_abs_coef),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      feature = dplyr::recode(feature, !!!feature_labels),
      avg_selection_rate = round(avg_selection_rate, 2),
      avg_abs_coef = round(avg_abs_coef, 3)
    ) |>
    dplyr::arrange(dplyr::desc(avg_selection_rate), dplyr::desc(avg_abs_coef))
}
