library(testthat)
library(httr2)

# Mock XML response
mock_xml <- "
<trials>
  <trial>
    <trial_id>ISRCTN27106947</trial_id>
    <public_title>Trial Public Title</public_title>
    <acronym>TPT</acronym>
    <scientific_title>Scientific Title</scientific_title>
    <url>https://www.isrctn.com/ISRCTN27106947</url>
    <recruitment_status>Completed</recruitment_status>
    <results_date_completed>2023-01-01</results_date_completed>
    <results_url_link>https://www.isrctn.com/results/ISRCTN27106947</results_url_link>
    <results_summary>Summary of results</results_summary>
    <results_date_posted>2023-02-01</results_date_posted>
    <results_date_first_publication>2023-03-01</results_date_first_publication>
  </trial>
</trials>"

# Define the test
test_that("generate_ISRCTN_df works correctly", {
  with_mocked_responses(list(
    "https://www.isrctn.com/api/query/format/who?q=ISRCTN27106947" = response(
      status_code = 200,
      headers = list("Content-Type" = "application/xml"),
      body = mock_xml
    )
  ), {
    # Call the function with the mocked URL
    ISRCTN_URL <- "https://www.isrctn.com/api/query/format/who?q=ISRCTN27106947"
    df <- generate_ISRCTN_df(ISRCTN_URL)

    # Check the dataframe
    expect_equal(nrow(df), 1)
    expect_equal(df$ISRCTN_No, "ISRCTN27106947")
    expect_equal(df$Public_Title, "Trial Public Title")
    expect_equal(df$Acronym, "TPT")
    expect_equal(df$Scientific_Title, "Scientific Title")
    expect_equal(df$URL, "https://www.isrctn.com/ISRCTN27106947")
    expect_equal(df$Recruitment_Status, "Completed")
    expect_equal(df$Results_date_completed, "2023-01-01")
    expect_equal(df$Results_url_link, "https://www.isrctn.com/results/ISRCTN27106947")
    expect_equal(df$Results_summary, "Summary of results")
    expect_equal(df$Results_date_posted, "2023-02-01")
    expect_equal(df$Results_date_first_publication, "2023-03-01")
  })
})
