context("test-mco_get_macav2_monthly_datasets.R")

test_that("macav2 dataset listing is available", {

  datset_listing <- "https://cida.usgs.gov/thredds/ncss/macav2metdata_monthly_future/dataset.html" %>%
    thredds::tds_ncss_list_vars()

  expect_s3_class(datset_listing, "tbl_df")
  expect_length(datset_listing, 16)
})

test_that("mco_get_macav2_monthly_datasets works",{

  datset_listing <- mco_get_macav2_monthly_datasets()
  expect_s3_class(datset_listing, "tbl_df")
  expect_length(datset_listing, 16)

})
