# Tests that the example Shiny app in inst/shiny-examples/myapp builds and
# renders its summary for each data format / instrument category combination

app_inputs <- list(
  tri_z2 = list(
    fmt = "trivariate", zcats = 2,
    cp1 = 74, cp2 = 0, cp3 = 11514, cp4 = 0,
    cp5 = 34, cp6 = 12, cp7 = 2385, cp8 = 9663
  ),
  tri_z3 = list(
    fmt = "trivariate", zcats = 3,
    bp1 = .83, bp2 = .11, bp3 = .05, bp4 = .01,
    bp5 = .88, bp6 = .05, bp7 = .06, bp8 = .01,
    bp9 = .72, bp10 = .20, bp11 = .05, bp12 = .03
  ),
  biv_z2 = list(
    fmt = "bivariate", zcats = 2,
    vp1 = .0064, vp2 = .9936, vp3 = .0038, vp4 = .9962,
    tp1 = 1, tp2 = 0, tp3 = .2, tp4 = .8
  ),
  biv_z3 = list(
    fmt = "bivariate", zcats = 3,
    xp1 = 388, xp2 = 313, xp3 = 314, xp4 = 307, xp5 = 81, xp6 = 91,
    qp1 = 613, qp2 = 88, qp3 = 566, qp4 = 55, qp5 = 119, qp6 = 53
  )
)

test_that("example Shiny app builds", {
  skip_if_not_installed("shiny")
  appdir <- system.file("shiny-examples", "myapp", package = "bpbounds")
  expect_true(nzchar(appdir))
  app <- shiny::shinyAppDir(appdir)
  expect_s3_class(app, "shiny.appobj")
})

for (scenario in names(app_inputs)) {
  test_that(paste("example Shiny app renders summary:", scenario), {
    skip_if_not_installed("shiny")
    appdir <- system.file("shiny-examples", "myapp", package = "bpbounds")
    app <- shiny::shinyAppDir(appdir)
    inputs <- app_inputs[[scenario]]
    shiny::testServer(app, {
      do.call(session$setInputs, inputs)
      out <- output$bpboundsSummary
      expect_match(out, paste0("Data:\\s+", inputs$fmt))
      expect_match(out, paste0("Instrument categories:\\s+", inputs$zcats))
    })
  })
}
