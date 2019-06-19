

#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export
app_server <- function(input, output, session) {


  imageData <- reactiveValues(data =
                                EBImage::channel(
                                  EBImage::readImage(
                                    system.file("www/capture.png",
                                                package = "mzFromImage")
                                  ),
                                  "gray")
  )

  dimensions <- reactive({
    validate(need(class(imageData$data) == "Image", "Not an image"))
    dim(imageData$data)
    })

  output$cuttingFuncs <- renderUI({
    validate(need(class(imageData$data) == "Image", "Not an image"))
    tagList(
      sliderInput("hvc",
                  "Horizontal cut",
                  min = 1,
                  max = dimensions()[[2]],
                  value = 783),
      sliderInput("lvc",
                  "Left vertical cut",
                  min = 1,
                  max = dimensions()[[1]],
                  value = dimensions()[[1]]*.06),
      sliderInput("rvc",
                  "Right vertical cut",
                  min = 1,
                  max = dimensions()[[1]],
                  value = dimensions()[[1]] * .88)
    )
  })

  observeEvent(input$inputFile, {
    if (is.null(input$inputFile)) {
      temp <- system.file("www/capture.png", package = "mzFromImage")
      imageData$data <- EBImage::readImage(temp)
    } else {
      validate(need(file.exists(input$inputFile$datapath), "Input file doesn't exist"))
      imageData$data <- tryCatch(EBImage::readImage(input$inputFile$datapath),
                                 error = function(x) validate(need(F, "unable to read image")),
                                 finally = function(x) print("unable to read image"))
    }

    if(class(imageData$data) != "Image") {
      temp <- system.file("www/capture.png", package = "mzFromImage")
      imageData$data <- EBImage::readImage(temp)
    }
    imageData$data <- EBImage::channel(imageData$data,"gray")
  })


  imageSlice <- reactive({
    validate(need(class(imageData$data) == "Image", "Not an image"))
    EBImage::imageData(imageData$data)[input$lvc:input$rvc, input$hvc]
  })


  massPeaks <- reactive({

    a <- MALDIquant::createMassSpectrum(mass = 1L:length(imageSlice()),
                                        intensity = 1L/imageSlice())
    a <- MALDIquant::detectPeaks(a, halfWindowSize = 1L)

    a@mass <- a@mass[-1]
    a@intensity <- a@intensity[-1]
    a

  })

  predicted <- reactive({
    pred <- data.frame(b = massPeaks()@mass)
    train <- cbind.data.frame(g = c(input$lowMass,
                                    input$highMass),
                              b = c(head(massPeaks()@mass, n = 1),
                                    tail(massPeaks()@mass, n = 1)))
    modelOut <- lm(g~b, data = train)
    predict(modelOut, pred)

  })


  output$modelOut <- renderTable({
    a <- as.data.frame(predicted())
    colnames(a) <- c("Extrapolated Masses")
    a
  })


  output$distPlot <- renderPlot({
    validate(need(isolate(class(imageData$data)) == "Image", "Not an image"))
    plot(imageData$data)
    abline(h = input$hvc, col = "red")
    abline(v = input$rvc, col = "red")
    abline(v = input$lvc, col = "red")
  })

  output$distPlot2 <- renderPlot({
    plot(1L/imageSlice(), type = "l")
    points(massPeaks()@mass, massPeaks()@intensity, col="red")

  })


}
