library(shinytest2)

test_that("{shinytest2} recording: upload-copper", {
  app <- AppDriver$new(variant = platform_variant(), name = "upload-copper", height = 916, 
      width = 1382)
  app$set_inputs(viewUpload_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_state = c(1752886799699, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$upload_file(uploadData = "data-copper.csv")
  app$set_inputs(viewUpload_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
      14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 
      33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_state = c(1752886805549, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(selectUnit = "")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752886809453, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(selectUnit = "µg/L")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_values()
  app$expect_screenshot()
  app$click(selector = "a:contains('3. Predict')")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(bootSamp = "500")
  app$click("getCl")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_hidden = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(clTable_rows_current = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(clTable_rows_all = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(clTable_state = c(1752886862900, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
      TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$expect_screenshot()
  app$click("drop727334267")
  app$set_inputs(toxicant = "")
  app$set_inputs(toxicant = "copper")
  app$click("drop727334267")
  app$set_inputs(drop727334267_state = TRUE)
  app$expect_download("dl_pdf")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_hidden = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(drop727334267_state = FALSE)
  app$expect_screenshot()
})
