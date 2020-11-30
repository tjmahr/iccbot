#' @importFrom stats na.omit pf qf
#' @importFrom rlang .data
NULL

#' @export
example_shrout_fleiss <- function() {
  tibble::tibble(
    Judge1 = c(9, 6, 8, 7, 10, 6),
    Judge2 = c(2, 1, 4, 1,  5, 2),
    Judge3 = c(5, 3, 6, 2,  6, 4),
    Judge4 = c(8, 2, 8, 6,  9, 7)
  )
}

#' @export
example_shrout_fleiss_nas_1 <- function() {
  tibble::tibble(
    Judge1 = c(NA, NA,  8,  7, 10,  6),
    Judge2 = c(NA, NA, NA, NA,  5,  2),
    Judge3 = c( 5,  3, NA, NA, NA, NA),
    Judge4 = c( 8,  2,  8,  6, NA, NA)
  )
}

#' @export
example_shrout_fleiss_nas_2 <- function() {
  tibble::tibble(
    Judge1 = c(9, 6, 8, 7, 10, 6),
    Judge2 = c(2, 1, 4, 1,  5, 2),
    Judge3 = c(5, 3, 6, 2,  6, 4),
    Judge4 = c(8, 2, 8, 6,  9, NA)
  )
}




#' @export
run_icc <- function(
  ratings,
  model = c("oneway", "twoway"),
  type = c("consistency", "agreement"),
  unit = c("single", "average"),
  r0 = 0,
  conf.level = 0.95,
  engine = "lme4"
) {

  model <- match.arg(model)
  type <- match.arg(type)
  unit <- match.arg(unit)

  if (type == "consistency" && model == "oneway") {
    stop("Consistency is not available for oneway models.")
  }

  if (engine == "lme4") {
    ms_list <- decompose_variance_with_lme4(ratings)
    data_stats <- analyze_data(ratings)
    icc <- compute_icc(
      ns = ms_list$n_participant,
      nr = ms_list$n_rater,
      MSr = ms_list$MSB,
      MSw = ms_list$MSW,
      MSc = ms_list$MSJ,
      MSe = ms_list$MSE,
      model = model,
      type = type,
      unit = unit,
      r0 = r0,
      conf.level = conf.level
    )
  } else  if (engine == "aov") {
    ms_list <- decompose_variance_with_aov(na.omit(ratings))
    data_stats <- analyze_data(na.omit(ratings))
    icc <- compute_icc(
      ns = ms_list$n_participant,
      nr = ms_list$n_rater,
      MSr = ms_list$MSB,
      MSw = ms_list$MSW,
      MSc = ms_list$MSJ,
      MSe = ms_list$MSE,
      model = model,
      type = type,
      unit = unit,
      r0 = r0,
      conf.level = conf.level
    )
  } else {
    engine <- "irr"
    data_stats <- analyze_data(na.omit(ratings))
    icc <- irr::icc(
      ratings,
      model = model,
      type = type,
      unit = unit,
      r0 = r0,
      conf.level = conf.level
    )
  }
  icc$engine <- engine
  icc[names(data_stats)] <- data_stats
  icc
}

analyze_data <- function(ratings) {
  l <- list()
  l$n_ratings <- sum(!is.na(ratings))
  l$n_ratings_missing <- sum(is.na(ratings))

  l$min_ratings_per_participant <- ratings %>%
    apply(1, function(x) sum(!is.na(x))) %>%
    min()
  l$max_ratings_per_participant <- ratings %>%
    apply(1, function(x) sum(!is.na(x))) %>%
    max()
  l$min_participants_per_rater <- ratings %>%
    apply(2, function(x) sum(!is.na(x))) %>%
    min()
  l$max_participants_per_rater <- ratings %>%
    apply(2, function(x) sum(!is.na(x))) %>%
    max()

  l
}

#' @export
decompose_variance_with_lme4 <- function(data) {
  data_long <- data %>%
    tibble::rowid_to_column("participant") %>%
    tidyr::gather("rater", "rating", -.data$participant) %>%
    dplyr::mutate(
      participant = paste0("S", .data$participant)
    )
  mixed_model <- lme4::lmer(
    rating ~ 1 + (1 | rater) + (1 | participant),
    data = data_long
  )

  vc <- lme4::VarCorr(mixed_model)

  MS_rater <- vc$rater[1, 1]
  MS_participant <- vc$participant[1, 1]
  MSE <- attr(vc, "sc") ^ 2

  n_rater <- unname(lme4::ngrps(mixed_model)["rater"])
  n_participant <- unname(lme4::ngrps(mixed_model)["participant"])

  MSW <- MS_rater + MSE
  MSB <- MS_participant * n_rater + MSE
  MSJ <- MS_rater * n_participant + MSE

  list(
    n_rater = n_rater,
    n_participant = n_participant,
    MSW = MSW,
    MSB = MSB,
    MSJ = MSJ,
    MSE = MSE
  )
}

#' @export
decompose_variance_with_aov <- function(data) {
  data_long <- data %>%
    tibble::rowid_to_column("participant") %>%
    dplyr::mutate(
      participant = paste0("S", .data$participant)
    ) %>%
    tidyr::gather("rater", "rating", -.data$participant)

  anova_df <- stats::aov(rating ~ participant + rater, data_long) %>%
    summary() %>%
    getElement(1)

  MSB <- anova_df["participant", "Mean Sq"]
  MSJ <- anova_df["rater", "Mean Sq"]
  MSE <- anova_df["Residuals", "Mean Sq"]

  DFJ <- anova_df["rater", "Df"]
  DFE <- anova_df["Residuals", "Df"]

  SSJ <- anova_df["rater", "Sum Sq"]
  SSE <- anova_df["Residuals", "Sum Sq"]

  MSW <- (SSJ + SSE) / (DFJ + DFE)

  list(
    n_rater = ncol(data),
    n_participant = nrow(data),
    MSW = MSW,
    MSB = MSB,
    MSJ = MSJ,
    MSE = MSE
  )
}

#' This is the core of the irr::icc() function
#' @export
compute_icc <- function(ns, nr, MSr, MSw, MSc, MSe, model, type, unit, r0, conf.level) {
  alpha <- 1 - conf.level

  if (type == "consistency" && model == "oneway") {
    stop("Consistency is not available for oneway models.")
  }

  if (unit == "single") {
    if (model == "oneway") {
      icc.name <- "ICC(1)"
      # type <- "agreement"
      coeff <- (MSr - MSw)/(MSr + (nr - 1) * MSw)
      Fvalue <- MSr/MSw * ((1 - r0)/(1 + (nr - 1) * r0))
      df1 <- ns - 1
      df2 <- ns * (nr - 1)
      p.value <- pf(Fvalue, df1, df2, lower.tail = FALSE)
      FL <- (MSr/MSw)/qf(1 - alpha/2, ns - 1, ns * (nr -
                                                      1))
      FU <- (MSr/MSw) * qf(1 - alpha/2, ns * (nr - 1),
                           ns - 1)
      lbound <- (FL - 1)/(FL + (nr - 1))
      ubound <- (FU - 1)/(FU + (nr - 1))
    }
    else if (model == "twoway") {
      if (type == "consistency") {
        icc.name <- "ICC(C,1)"
        coeff <- (MSr - MSe)/(MSr + (nr - 1) * MSe)
        Fvalue <- MSr/MSe * ((1 - r0)/(1 + (nr - 1) *
                                         r0))
        df1 <- ns - 1
        df2 <- (ns - 1) * (nr - 1)
        p.value <- pf(Fvalue, df1, df2, lower.tail = FALSE)
        FL <- (MSr/MSe)/qf(1 - alpha/2, ns - 1, (ns -
                                                   1) * (nr - 1))
        FU <- (MSr/MSe) * qf(1 - alpha/2, (ns - 1) *
                               (nr - 1), ns - 1)
        lbound <- (FL - 1)/(FL + (nr - 1))
        ubound <- (FU - 1)/(FU + (nr - 1))
      }
      else if (type == "agreement") {
        icc.name <- "ICC(A,1)"
        coeff <- (MSr - MSe)/(MSr + (nr - 1) * MSe +
                                (nr/ns) * (MSc - MSe))
        a <- (nr * r0)/(ns * (1 - r0))
        b <- 1 + (nr * r0 * (ns - 1))/(ns * (1 - r0))
        Fvalue <- MSr/(a * MSc + b * MSe)
        a <- (nr * coeff)/(ns * (1 - coeff))
        b <- 1 + (nr * coeff * (ns - 1))/(ns * (1 - coeff))
        v <- (a * MSc + b * MSe)^2/((a * MSc)^2/(nr -
                                                   1) + (b * MSe)^2/((ns - 1) * (nr - 1)))
        df1 <- ns - 1
        df2 <- v
        p.value <- pf(Fvalue, df1, df2, lower.tail = FALSE)
        FL <- qf(1 - alpha/2, ns - 1, v)
        FU <- qf(1 - alpha/2, v, ns - 1)
        lbound <- (ns * (MSr - FL * MSe))/(FL * (nr *
                                                   MSc + (nr * ns - nr - ns) * MSe) + ns * MSr)
        ubound <- (ns * (FU * MSr - MSe))/(nr * MSc +
                                             (nr * ns - nr - ns) * MSe + ns * FU * MSr)
      }
    }
  }
  else if (unit == "average") {
    if (model == "oneway") {
      # type <- "agreement"
      icc.name <- paste("ICC(", nr, ")", sep = "")
      coeff <- (MSr - MSw)/MSr
      Fvalue <- MSr/MSw * (1 - r0)
      df1 <- ns - 1
      df2 <- ns * (nr - 1)
      p.value <- pf(Fvalue, df1, df2, lower.tail = FALSE)
      FL <- (MSr/MSw)/qf(1 - alpha/2, ns - 1, ns * (nr -
                                                      1))
      FU <- (MSr/MSw) * qf(1 - alpha/2, ns * (nr - 1),
                           ns - 1)
      lbound <- 1 - 1/FL
      ubound <- 1 - 1/FU
    }
    else if (model == "twoway") {
      if (type == "consistency") {
        icc.name <- paste("ICC(C,", nr, ")",
                          sep = "")
        coeff <- (MSr - MSe)/MSr
        Fvalue <- MSr/MSe * (1 - r0)
        df1 <- ns - 1
        df2 <- (ns - 1) * (nr - 1)
        p.value <- pf(Fvalue, df1, df2, lower.tail = FALSE)
        FL <- (MSr/MSe)/qf(1 - alpha/2, ns - 1, (ns -
                                                   1) * (nr - 1))
        FU <- (MSr/MSe) * qf(1 - alpha/2, (ns - 1) *
                               (nr - 1), ns - 1)
        lbound <- 1 - 1/FL
        ubound <- 1 - 1/FU
      }
      else if (type == "agreement") {
        icc.name <- paste("ICC(A,", nr, ")",
                          sep = "")
        coeff <- (MSr - MSe)/(MSr + (MSc - MSe)/ns)
        a <- r0/(ns * (1 - r0))
        b <- 1 + (r0 * (ns - 1))/(ns * (1 - r0))
        Fvalue <- MSr/(a * MSc + b * MSe)
        a <- (nr * coeff)/(ns * (1 - coeff))
        b <- 1 + (nr * coeff * (ns - 1))/(ns * (1 - coeff))
        v <- (a * MSc + b * MSe)^2/((a * MSc)^2/(nr -
                                                   1) + (b * MSe)^2/((ns - 1) * (nr - 1)))
        df1 <- ns - 1
        df2 <- v
        p.value <- pf(Fvalue, df1, df2, lower.tail = FALSE)
        FL <- qf(1 - alpha/2, ns - 1, v)
        FU <- qf(1 - alpha/2, v, ns - 1)
        lbound <- (ns * (MSr - FL * MSe))/(FL * (MSc -
                                                   MSe) + ns * MSr)
        ubound <- (ns * (FU * MSr - MSe))/(MSc - MSe +
                                             ns * FU * MSr)
      }
    }
  }
  rval <- structure(list(subjects = ns, raters = nr, model = model,
                         type = type, unit = unit, icc.name = icc.name, value = coeff,
                         r0 = r0, Fvalue = Fvalue, df1 = df1, df2 = df2, p.value = p.value,
                         conf.level = conf.level, lbound = lbound, ubound = ubound),
                    class = "icclist")
  return(rval)
}

#' @export
add_formatted_results_to_icc <- function(icc, icc_digits = 3) {
  icc$lbound_p <- icc$lbound %>%
    printy::fmt_fix_digits(icc_digits) %>%
    printy::fmt_leading_zero()

  icc$ubound_p <- icc$ubound %>%
    printy::fmt_fix_digits(icc_digits) %>%
    printy::fmt_leading_zero()

  icc$value <- icc$value %>%
    printy::fmt_fix_digits(icc_digits) %>%
    printy::fmt_leading_zero()

  icc$model_p <- ifelse(
    icc$model == "twoway",
    "two-way",
    "one-way"
  )

  icc$unit_p <- ifelse(
    icc$unit == "single",
    "a single-score",
    "an average-score"
  )

  icc$unit_p2 <- ifelse(
    icc$unit == "single",
    "raters",
    "average ratings"
  )

  icc$type_p <- ifelse(
    icc$type == "consistency",
    "consistency-based",
    "absolute agreement"
  )


  if (icc$n_ratings_missing == 0) {
    p_sub <- if (icc$model == "oneway") {
      glue::glue("{icc$raters} different raters ({icc$n_ratings} unique raters)")
    } else {
      glue::glue("{icc$raters} raters")
    }
    p <- glue::glue(
      "
      Each of the {icc$subjects} participants
      were scored by {p_sub}.
      "
    )
  } else {
    if (icc$min_ratings_per_participant == icc$max_ratings_per_participant) {
      n_raters2 <- icc$max_ratings_per_participant
    } else {
      n_raters2 <- paste0(
        icc$min_ratings_per_participant,
        "&ndash;",
        icc$max_ratings_per_participant
      )
    }
    if (icc$min_participants_per_rater == icc$max_participants_per_rater) {
      n_participants_2 <- icc$max_participants_per_rater
    } else {
      n_participants_2 <- paste0(
        icc$min_participants_per_rater,
        "&ndash;",
        icc$max_participants_per_rater
      )
    }
    p_sub <- if (icc$model == "oneway") {
      glue::glue("{n_raters2} different raters ({icc$n_ratings} unique raters)")
    } else {
      glue::glue("{n_raters2} raters. Each of the {icc$raters} raters
      scored {n_participants_2} participants")
    }
    p <- glue::glue("Each of the {icc$subjects} participants were scored
      by {p_sub}."
    )
  }

  icc$rater_participant_counts_p <- p

  icc
}
