library(shiny)

## First experiments with Shiny for the SPR app. 
# Define UI for application that draws the SPR curve from raw data

shinyUI(
  navbarPage("SPR Explorer",
             tabPanel("Raw data", fluidPage(
               titlePanel("Raw SPR data"),
               sidebarLayout(sidebarPanel(
                 textInput("device", "SPR device location", "/dev/cu.usbmodem1411"), 
                 actionButton("acquire", "Acquire data"),
                 sliderInput("window", "Moving Average Window:",
                             min = 1, max = 20, value = 5), 
                 numericInput("calibration", "Enter LED intensity (0-100)", 70, 
                              min = 0, max=100, step=1), 
                 actionButton("calibrate", "Calibrate")
               ),
               mainPanel(plotOutput("pixelPlot"))
               ))), 
             tabPanel("SPR Stream", fluidPage(
               titlePanel("Streaming Data"), 
               sidebarLayout(sidebarPanel(
                 textInput("device", "SPR device location", "/dev/cu.usbmodem1411"),
                 numericInput("numreads", "Number of readings", 10, 
                              min = 1, max = 1000, step = 1), 
                 actionButton("stream", "Stream"),
                 textInput("save", "Save data to CSV", 
                           paste0("Untitled-", format(Sys.time(), "%H:%M:%S"))), 
                 actionButton("savefile", "Save!")
               ),
               mainPanel(plotOutput("streamPlot"))
               )))
  )
)