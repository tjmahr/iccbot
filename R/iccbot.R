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
run_icc <- function(
  ratings,
  model = c("oneway", "twoway"),
  type = c("consistency", "agreement"),
  unit = c("single", "average"),
  r0 = 0,
  conf.level = 0.95
) {

  model <- match.arg(model)
  type <- match.arg(type)
  unit <- match.arg(unit)
  alpha <- 1 - conf.level

  d_long <- ratings %>%
    tibble::rowid_to_column("subj") %>%
    tidyr::gather(judge, rating, -subj)

  mixed_model <- lme4::lmer(
    rating ~ 1 + (1 | judge) + (1 | subj), data = d_long
  )

  vc <- lme4::VarCorr(mixed_model)
  MS_judge <- vc$judge[1, 1]
  MS_subj <- vc$subj[1, 1]
  MSE <- attr(vc, "sc") ^ 2

  n_subj <- nrow(ratings)
  n_judge <- ncol(ratings)

  MSB <- n_judge * MS_subj  + MSE
  MSJ <- n_subj  * MS_judge + MSE
  MSW <- MS_judge + MSE

  icc <- compute_icc(
    ns = n_subj,
    nr = n_judge,
    MSr = MSB,
    MSw = MSW,
    MSc = MSJ,
    MSe = MSE,
    model = model,
    type = type,
    unit = unit,
    r0 = r0,
    conf.level = conf.level
  )

  icc$lme4 <- mixed_model
  icc
}

#' This is the core of the irr::icc() function
#' @export
compute_icc <- function(ns, nr, MSr, MSw, MSc, MSe, model, type, unit, r0, conf.level) {
  alpha <- 1 - conf.level

  if (unit == "single") {
    if (model == "oneway") {
      icc.name <- "ICC(1)"
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

  icc$raters_p <- ifelse(
    icc$model == "twoway",
    sprintf("%s raters", icc$raters),
    sprintf(
      "%s different raters (%s unique raters)",
      icc$raters,
      icc$raters *  icc$subjects
    )
  )

  icc
}
