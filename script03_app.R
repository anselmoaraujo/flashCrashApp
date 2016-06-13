#!/usr/bin/env Rscript

#### Load necessary packages and data ####
library(shiny)
library(networkD3)

Sys.setenv(TZ='UTC')


#### Server ####
server <- function(input, output) {

  network_list <- readRDS("app_data/output_network.Rds")
  ts_data <- readRDS("app_data/time_series_data.Rds")

  output$sp <- renderForceNetwork({
    timeEnd <- input$timeEnd
    attributes(timeEnd)$tzone <- "UTC"

    index <- which(network_list$time == input$timeEnd)
    if (length(index) != 1) return(NULL)

    graph <- network_list$network[[index]]
    if (is.null(graph$links)) return(NULL)
    if (nrow(graph$links) == 0) return(NULL)

    forceNetwork(Links = graph$links,
                 Nodes = graph$nodes,
                 Target = "target", NodeID = "name", Group = "group",
                 bounded = TRUE, linkColour = "#666", linkDistance = 30,
                 fontSize = 36)
  })

  output$ts <- renderPlot({
    timeEnd <- input$timeEnd
    attributes(timeEnd)$tzone <- "UTC"
    twindow  <- strptime(c("2010-05-06 14:30:00",
                           "2010-05-06 15:00:00"), format="%Y-%m-%d %H:%M:%S")

    n_symb  <- 467 + 80

    a <- 20500
    b <- 22000
    ts_data$sp_val$trans <- (ts_data$sp_val$val - a) / (b - a)

    these <- which(ts_data$nc$time > twindow[1] & ts_data$nc$time < twindow[2])

    par(mar = c(5.1, 4.1, 0, 7.1))
    plot(ts_data$nc$time[these], ts_data$nc$val[these] / n_symb,
          type = 'l', axes = FALSE, xlab = "", ylab = "", ylim = c(0,1),
          col = "#ffffff", lwd=2, main = "")
    box()

    axis(1, at = ts_data$nc$time, format(ts_data$nc$time,"%H:%M:%S"), las = 2, cex = 0.7)
    axis(2, las = 2)
    axis(4, at = seq(0,1,length.out=6), seq(a, b, length.out=6), las = 2)

    mtext("proportion of symbols in giant component", side = 2L, line = 3)
    mtext("S&P 500", side = 4L, line = 4)

    lines(ts_data$nc$time, ts_data$nc$val / n_symb,
          col = "#268bd2", lwd = 2)
    lines(ts_data$sp_val$time, ts_data$sp_val$trans)


    points(timeEnd, ts_data$nc$val[ts_data$nc$time == timeEnd] / n_symb, pch = 19, col = "#268bd2", cex = 1.5)
    points(timeEnd, ts_data$sp_val$trans[ts_data$sp_val$time == timeEnd], pch = 19, cex = 1.5)

  })

}

#### UI ####
ui <- shinyUI(fluidPage(

  titlePanel("Flash Crash Detection (2010-05-06)"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("timeEnd", label = "Time range",
        min = as.POSIXct("2010-05-06 14:30:00"),
        max = as.POSIXct("2010-05-06 14:58:00"),
        step = 60,
        value = c(
          as.POSIXct("2010-05-06 14:30:00")
        ),
        timeFormat = "%T",
        timezone = "+0000",
        animate = TRUE
      )
    ),
    mainPanel(
      plotOutput("ts", height = "300px"),
      tabsetPanel(
        #tabPanel("ETFs", simpleNetworkOutput("etf", height = "300px")),
        tabPanel("", forceNetworkOutput("sp", height = "300px"))
      )
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)

