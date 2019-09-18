test_that("lme4 output matches Shrout and Fleiss (1979)", {
  d <- example_shrout_fleiss()

  pull_icc <- . %>% getElement("value") %>% round(2)
  run_icc_lme4 <- function(...) run_icc(..., engine = "lme4")

  icc_1_c_s <- run_icc_lme4(d, "oneway", "consistency", "single", )
  icc_1_c_a <- run_icc_lme4(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc_lme4(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc_lme4(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc_lme4(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc_lme4(d, "twoway", "consistency", "average")

  icc_1_c_s %>% pull_icc() %>% expect_equal(.17)
  icc_1_c_a %>% pull_icc() %>% expect_equal(.44)

  icc_2_a_s %>% pull_icc() %>% expect_equal(.29)
  icc_2_a_a %>% pull_icc() %>% expect_equal(.62)

  icc_2_c_s %>% pull_icc() %>% expect_equal(.71)
  icc_2_c_a %>% pull_icc() %>% expect_equal(.91)
})

test_that("aov output matches Shrout and Fleiss (1979)", {
  d <- example_shrout_fleiss()

  pull_icc <- . %>% getElement("value") %>% round(2)
  run_icc_aov <- function(...) run_icc(..., engine = "aov")

  icc_1_c_s <- run_icc_aov(d, "oneway", "consistency", "single", )
  icc_1_c_a <- run_icc_aov(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc_aov(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc_aov(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc_aov(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc_aov(d, "twoway", "consistency", "average")

  icc_1_c_s %>% pull_icc() %>% expect_equal(.17)
  icc_1_c_a %>% pull_icc() %>% expect_equal(.44)

  icc_2_a_s %>% pull_icc() %>% expect_equal(.29)
  icc_2_a_a %>% pull_icc() %>% expect_equal(.62)

  icc_2_c_s %>% pull_icc() %>% expect_equal(.71)
  icc_2_c_a %>% pull_icc() %>% expect_equal(.91)
})

test_that("irr::icc output matches Shrout and Fleiss (1979)", {
  d <- example_shrout_fleiss()

  pull_icc <- . %>% getElement("value") %>% round(2)
  run_icc_irr <- function(...) run_icc(..., engine = "irr")

  icc_1_c_s <- run_icc_irr(d, "oneway", "consistency", "single", )
  icc_1_c_a <- run_icc_irr(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc_irr(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc_irr(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc_irr(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc_irr(d, "twoway", "consistency", "average")

  icc_1_c_s %>% pull_icc() %>% expect_equal(.17)
  icc_1_c_a %>% pull_icc() %>% expect_equal(.44)

  icc_2_a_s %>% pull_icc() %>% expect_equal(.29)
  icc_2_a_a %>% pull_icc() %>% expect_equal(.62)

  icc_2_c_s %>% pull_icc() %>% expect_equal(.71)
  icc_2_c_a %>% pull_icc() %>% expect_equal(.91)
})

test_that("Output matches example on Wikipedia", {
  d <- data.frame(
    x = c(1:5),
    y = c(1:5 * 2)
  )

  icc_1_c_s <- run_icc(d, "oneway", "consistency", "single")
  icc_1_c_a <- run_icc(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc(d, "twoway", "consistency", "average")

  icc_1_c_s %>% getElement("value") %>% round(2) %>% expect_equal(.34)
  icc_1_c_a %>% getElement("value") %>% round(2) %>% expect_equal(.51)

  icc_2_a_s %>% getElement("value") %>% round(2) %>% expect_equal(.48)
  icc_2_a_a %>% getElement("value") %>% round(2) %>% expect_equal(.65)

  icc_2_c_s %>% getElement("value") %>% round(2) %>% expect_equal(.8)
  icc_2_c_a %>% getElement("value") %>% round(2) %>% expect_equal(.89)
})

test_that("Counting missing data", {
  testthat::skip_if_not_installed("psych")

  d <- example_shrout_fleiss_nas_1()
  icc_2_a_s <- run_icc(d, "twoway", "agreement", "single")
  expect_equal(icc_2_a_s$engine, "lme4")
  expect_equal(icc_2_a_s$engine, "lme4")
  expect_equal(icc_2_a_s$n_ratings, 12)
  expect_equal(icc_2_a_s$n_ratings_missing, 12)

  icc_2_a_s %>%
    add_formatted_results_to_icc() %>%
    getElement("rater_participant_counts_p") %>%
    expect_match(regexp = "2&ndash;4 participants") %>%
    expect_match(regexp = "2 raters")

  d2 <- example_shrout_fleiss_nas_2()
  icc_2_a_s <- run_icc(d2, "twoway", "agreement", "single")
  expect_equal(icc_2_a_s$n_ratings, 23)
  expect_equal(icc_2_a_s$n_ratings_missing, 1)

  icc_2_a_s %>%
    add_formatted_results_to_icc() %>%
    getElement("rater_participant_counts_p") %>%
    expect_match(regexp = "5&ndash;6 participants") %>%
    expect_match(regexp = "3&ndash;4 raters")
})

test_that("Results with missing data match psych::ICC()", {
  testthat::skip_if_not_installed("psych")

  d <- example_shrout_fleiss_nas_2()
  pull_icc <- . %>% getElement("value") %>% round(5)
  run_icc_lme4 <- function(...) run_icc(..., engine = "lme4")

  icc_1_c_s <- run_icc_lme4(d, "oneway", "consistency", "single", )
  icc_1_c_a <- run_icc_lme4(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc_lme4(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc_lme4(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc_lme4(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc_lme4(d, "twoway", "consistency", "average")

  psych_one <- psych::ICC(d, lmer = TRUE, missing = FALSE)

  icc_1_c_s %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Single_raters_absolute", "ICC"], tol = .001
    )

  icc_1_c_a %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Average_raters_absolute", "ICC"], tol = .001
    )

  icc_2_a_s %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Single_random_raters", "ICC"], tol = .001
    )

  icc_2_a_a %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Average_random_raters", "ICC"], tol = .001
    )

  icc_2_c_s %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Single_fixed_raters", "ICC"], tol = .001
    )

  icc_2_c_a %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Average_fixed_raters", "ICC"], tol = .001
    )

  d <- example_shrout_fleiss_nas_1()

  icc_1_c_s <- run_icc_lme4(d, "oneway", "consistency", "single", )
  icc_1_c_a <- run_icc_lme4(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc_lme4(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc_lme4(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc_lme4(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc_lme4(d, "twoway", "consistency", "average")

  psych_one <- psych::ICC(d, lmer = TRUE, missing = FALSE)

  icc_1_c_s %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Single_raters_absolute", "ICC"], tol = .001
    )

  icc_1_c_a %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Average_raters_absolute", "ICC"], tol = .001
    )

  icc_2_a_s %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Single_random_raters", "ICC"], tol = .001
    )

  icc_2_a_a %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Average_random_raters", "ICC"], tol = .001
    )

  icc_2_c_s %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Single_fixed_raters", "ICC"], tol = .001
    )

  icc_2_c_a %>%
    pull_icc() %>%
    expect_equal(
      psych_one$results["Average_fixed_raters", "ICC"], tol = .001
    )
})
