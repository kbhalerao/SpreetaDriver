library(shiny)



shinyServer(function(input, output) {
  
  datfun <- reactive({
    input$acquire
    data.frame(pixel = 0:127, intensity = rnorm(128, 2.5, 0.2))
  })
  
  streamdat <- reactive({
    input$stream
    dat <- 1:input$numreads
    riu <- runif(input$numreads, 1.320, 1.368)
    data.frame(dat, riu)
  })
  
  output$pixelPlot <- renderPlot({
    dat <- datfun()
    ma_coeffs <- rep(1/input$window, input$window)
    filtered <- filter(dat$intensity, ma_coeffs, 
                       method=c("convolution"), sides=2)
    plot(dat$pixel, dat$intensity, 
         main = "SPR Raw Data", 
         xlab = "Pixel", 
         ylab = "Intensity (V)", 
         type = "l", 
         ylim = c(0,3.5))
    lines(dat$pixel, filtered, col="red")
  })
  
  output$streamPlot <- renderPlot({

    plot(dat, riu, type="l", 
         main = "RIU change over time", 
         xlab = "Time", 
         ylab = "RI Units")
  })
})