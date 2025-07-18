library(shinytest2)

test_that("{shinytest2} recording: shinyssdtools", {
  app <- AppDriver$new(name = "shinyssdtools", height = 916, width = 1382)
  app$set_inputs(viewUpload_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_state = c(1752809353242, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$click("demoData")
  app$set_inputs(viewUpload_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
      14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28), allow_no_input_binding_ = TRUE)
  app$set_inputs(viewUpload_state = c(1752809375350, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(selectUnit = "")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2, 3, 4, 5, 6), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809381186, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(selectUnit = "µg/L")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(selectDist = c("gamma", "lgumbel", "llogis", "lnorm", "lnorm_lnorm"))
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2, 3, 4, 5), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809398440, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(selectDist = c("gamma", "lgumbel", "llogis", "lnorm"))
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809401358, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(selectDist = c("gamma", "lgumbel", "llogis"))
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809403509, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(selectDist = c("gamma", "lgumbel"))
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809405352, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(selectDist = "gamma")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809406454, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$set_inputs(selectDist = c("gamma", "lgumbel"))
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(gofTable_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(gofTable_state = c(1752809435992, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
})
