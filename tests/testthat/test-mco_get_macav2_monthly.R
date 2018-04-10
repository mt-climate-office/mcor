context("test-mco_get_macav2_monthly.R")

test_that("mco_get_macav2_monthly works",{

  datset_listing <- mco_get_macav2_monthly(x = mt_counties_simple %>%
                                             dplyr::filter(County == "Missoula"),
                                           models = c("BNU-ESM",
                                                      "CCSM4"),
                                           elements = "pr",
                                           scenarios = "rcp45",
                                           overwrite = FALSE,
                                           ncss_args = list(temporal = "all"))
  expect_s3_class(datset_listing, "tbl_df")
  expect_length(datset_listing, 21)
  expect_s4_class(datset_listing$data[[1]], "RasterBrick")

})
