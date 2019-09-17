test_that("Output matches Shrout and Fleiss (1979)", {
  d <- example_shrout_fleiss()

  icc_1_c_s <- run_icc(d, "oneway", "consistency", "single")
  icc_1_c_a <- run_icc(d, "oneway", "consistency", "average")

  icc_2_a_s <- run_icc(d, "twoway", "agreement", "single")
  icc_2_a_a <- run_icc(d, "twoway", "agreement", "average")

  icc_2_c_s <- run_icc(d, "twoway", "consistency", "single")
  icc_2_c_a <- run_icc(d, "twoway", "consistency", "average")

  icc_1_c_s %>% getElement("value") %>% round(2) %>% expect_equal(.17)
  icc_1_c_a %>% getElement("value") %>% round(2) %>% expect_equal(.44)

  icc_2_a_s %>% getElement("value") %>% round(2) %>% expect_equal(.29)
  icc_2_a_a %>% getElement("value") %>% round(2) %>% expect_equal(.62)

  icc_2_c_s %>% getElement("value") %>% round(2) %>% expect_equal(.71)
  icc_2_c_a %>% getElement("value") %>% round(2) %>% expect_equal(.91)


  y_icc_1_c_s <- irr::icc(d, "oneway", "consistency", "single")
  y_icc_1_c_a <- irr::icc(d, "oneway", "consistency", "average")

  y_icc_2_a_s <- irr::icc(d, "twoway", "agreement", "single")
  y_icc_2_a_a <- irr::icc(d, "twoway", "agreement", "average")

  y_icc_2_c_s <- irr::icc(d, "twoway", "consistency", "single")
  y_icc_2_c_a <- irr::icc(d, "twoway", "consistency", "average")

  expect_equal(icc_1_c_s$value, y_icc_1_c_s$value, tol = .001)
  expect_equal(icc_1_c_a$value, y_icc_1_c_a$value, tol = .001)
  expect_equal(icc_2_a_s$value, y_icc_2_a_s$value, tol = .001)
  expect_equal(icc_2_a_a$value, y_icc_2_a_a$value, tol = .001)
  expect_equal(icc_2_c_s$value, y_icc_2_c_s$value, tol = .001)
  expect_equal(icc_2_c_a$value, y_icc_2_c_a$value, tol = .001)
})

test_that("Output matches another test", {
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
