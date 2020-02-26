library(shiny)
library(bpbounds)

# UI ----
ui <- fluidPage(
  titlePanel(
    "Balke and Pearl nonparametric bounds for the average causal effect"
  ),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "fmt",
        "Data format:",
        c(
          "Trivariate (Z, X, and Y in same dataset)" = "trivariate",
          "Bivariate (Two sample data: {Z,X} in one dataset; {Z,Y} in another)" =
            "bivariate"
        )
      ),
      radioButtons("zcats",
                   "Number of instrument categories:",
                   c("2" = 2,
                     "3" = 3)),
      conditionalPanel(
        condition = "input.zcats == 2 & input.fmt == 'trivariate'",
        p(
          "The example data given below are cell counts from Balke and Pearl's Vitamin A example."
        ),
        p("Edit the values in the cells to run for your own data."),
        numericInput("cp1", "P(Y=0, X=0 | Z=0) or cell count", value = 74),
        numericInput("cp2", "P(Y=0, X=1 | Z=0) or cell count", value = 0),
        numericInput("cp3", "P(Y=1, X=0 | Z=0) or cell count", value = 11514),
        numericInput("cp4", "P(Y=1, X=1 | Z=0) or cell count", value = 0),
        numericInput("cp5", "P(Y=0, X=0 | Z=1) or cell count", value = 34),
        numericInput("cp6", "P(Y=0, X=1 | Z=1) or cell count", value = 12),
        numericInput("cp7", "P(Y=1, X=0 | Z=1) or cell count", value = 2385),
        numericInput("cp8", "P(Y=1, X=1 | Z=1) or cell count", value = 9663)
      ),
      conditionalPanel(
        condition = "input.zcats == 3 & input.fmt == 'trivariate'",
        p(
          "The example data given below are conditional probabilities from the Mendelian randomization example given in the package vignette."
        ),
        p("Edit the values in the cells to run for your own data."),
        numericInput("bp1", "P(Y=0, X=0 | Z=0) or cell count", value = .83),
        numericInput("bp2", "P(Y=0, X=1 | Z=0) or cell count", value = .05),
        numericInput("bp3", "P(Y=1, X=0 | Z=0) or cell count", value = .11),
        numericInput("bp4", "P(Y=1, X=1 | Z=0) or cell count", value = .01),
        numericInput("bp5", "P(Y=0, X=0 | Z=1) or cell count", value = .88),
        numericInput("bp6", "P(Y=0, X=1 | Z=1) or cell count", value = .06),
        numericInput("bp7", "P(Y=1, X=0 | Z=1) or cell count", value = .05),
        numericInput("bp8", "P(Y=1, X=1 | Z=1) or cell count", value = .01),
        numericInput("bp9", "P(Y=0, X=0 | Z=2) or cell count", value = .72),
        numericInput("bp10", "P(Y=0, X=1 | Z=2) or cell count", value = .05),
        numericInput("bp11", "P(Y=1, X=0 | Z=2) or cell count", value = .20),
        numericInput("bp12", "P(Y=1, X=1 | Z=2) or cell count", value = .03)
      ),
      conditionalPanel(
        condition = "input.zcats == 2 & input.fmt == 'bivariate'",
        p(
          "The example data given below are conditional probabilities from Balke and Pearl's Vitamin A example."
        ),
        p("Edit the values in the cells to run for your own data."),
        numericInput("vp1", "P(Y=0 | Z=0) or cell count", value = .0064),
        # 74
        numericInput("vp2", "P(Y=1 | Z=0) or cell count", value = .9936),
        # 11514
        numericInput("vp3", "P(Y=0 | Z=1) or cell count", value = .0038),
        # 46
        numericInput("vp4", "P(Y=1 | Z=1) or cell count", value = .9962),
        # 12048
        numericInput("tp1", "P(X=0 | Z=0) or cell count", value = 1),
        # 11588
        numericInput("tp2", "P(X=1 | Z=0) or cell count", value = 0),
        # 0
        numericInput("tp3", "P(X=0 | Z=1) or cell count", value = .2),
        # 2149
        numericInput("tp4", "P(X=1 | Z=1) or cell count", value = .8) # 9675
      ),
      conditionalPanel(
        condition = "input.zcats == 3 & input.fmt == 'bivariate'",
        p(
          "The example data given below are cell counts from a hypothetical example."
        ),
        p("Edit the values in the cells to run for your own data."),
        numericInput("xp1", "P(Y=0 | Z=0) or cell count", value = 388),
        numericInput("xp2", "P(Y=1 | Z=0) or cell count", value = 313),
        numericInput("xp3", "P(Y=0 | Z=1) or cell count", value = 314),
        numericInput("xp4", "P(Y=1 | Z=1) or cell count", value = 307),
        numericInput("xp5", "P(Y=0 | Z=2) or cell count", value = 81),
        numericInput("xp6", "P(Y=1 | Z=2) or cell count", value = 91),
        numericInput("qp1", "P(X=0 | Z=0) or cell count", value = 613),
        numericInput("qp2", "P(X=1 | Z=0) or cell count", value = 88),
        numericInput("qp3", "P(X=0 | Z=1) or cell count", value = 566),
        numericInput("qp4", "P(X=1 | Z=1) or cell count", value = 55),
        numericInput("qp5", "P(X=0 | Z=2) or cell count", value = 119),
        numericInput("qp6", "P(X=1 | Z=2) or cell count", value = 53)
      )
    ),

    mainPanel(
      verbatimTextOutput("bpboundsSummary"),
      h3("Information"),
      p(
        "This app demonstrates the bpbounds R package available on CRAN and GitHub. You can install the package using:"
      ),
      code("install.packages('bpbounds')"),
      br(),
      br(),
      p(
        "Further details are available on the package website:",
        a("https://remlapmot.github.io/bpbounds", href = "https://remlapmot.github.io/bpbounds")
      ),
      p(
        "Author: Tom Palmer,",
        a("tom.palmer@bristol.ac.uk", href = "mailto:tom.palmer@bristol.ac.uk")
      )
    )

  )
)

# Server ----
server <- function(input, output) {
  observe({
    if (input$zcats == 2 & input$fmt == "trivariate") {
      cp = c(
        input$cp1,
        input$cp2,
        input$cp3,
        input$cp4,
        input$cp5,
        input$cp6,
        input$cp7,
        input$cp8
      )
      tabp =
        as.table(array(
          cp,
          dim = c(2, 2, 2),
          dimnames = list(
            x = c(0, 1),
            y = c(0, 1),
            z = c(0, 1)
          )
        ))
      output$bpboundsSummary <- renderPrint({
        res <- bpbounds(tabp, fmt = input$fmt)
        summary(res)
      })
    } else if (input$zcats == 3 & input$fmt == "trivariate") {
      cp = c(
        input$bp1,
        input$bp2,
        input$bp3,
        input$bp4,
        input$bp5,
        input$bp6,
        input$bp7,
        input$bp8,
        input$bp9,
        input$bp10,
        input$bp11,
        input$bp12
      )
      tabp = as.table(array(
        cp,
        dim = c(2, 2, 3),
        dimnames = list(
          x = c(0, 1),
          y = c(0, 1),
          z = c(0, 1, 2)
        )
      ))
      output$bpboundsSummary <- renderPrint({
        res <- bpbounds(tabp, fmt = input$fmt)
        summary(res)
      })
    } else if (input$zcats == 2 & input$fmt == "bivariate") {
      cp = c(input$vp1,
             input$vp2,
             input$vp3,
             input$vp4)
      tp = c(input$tp1,
             input$tp2,
             input$tp3,
             input$tp4)
      tabp = as.table(array(
        cp,
        dim = c(2, 2),
        dimnames = list(y = c(0, 1),
                        z = c(0, 1))
      ))
      tabt = as.matrix(as.table(array(
        tp,
        dim = c(2, 2),
        dimnames = list(x = c(0, 1),
                        z = c(0, 1))
      )))
      output$bpboundsSummary <- renderPrint({
        res <- bpbounds(p = tabp,
                        t = tabt,
                        fmt = input$fmt)
        summary(res)
      })
    } else if (input$zcats == 3 & input$fmt == "bivariate") {
      cp = c(input$xp1,
             input$xp2,
             input$xp3,
             input$xp4,
             input$xp5,
             input$xp6)
      tp = c(input$qp1,
             input$qp2,
             input$qp3,
             input$qp4,
             input$qp5,
             input$qp6)
      tabp = as.table(array(
        cp,
        dim = c(2, 3),
        dimnames = list(y = c(0, 1),
                        z = c(0, 1, 2))
      ))
      tabt = as.matrix(as.table(array(
        tp,
        dim = c(2, 3),
        dimnames = list(x = c(0, 1),
                        z = c(0, 1, 2))
      )))
      output$bpboundsSummary <- renderPrint({
        res <- bpbounds(tabp,
                        tabt,
                        fmt = input$fmt)
        summary(res)
      })
    }
  })
}

# Define as app ----
shinyApp(ui = ui, server = server)
