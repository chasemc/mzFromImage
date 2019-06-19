#' main app ui
#'
#' @return shiny ui
#' @export
#'
app_ui <- function(){

  fluidPage(
    titlePanel("Extrapolate m/z from Image"),
    sidebarLayout(
      sidebarPanel(
        p("The horizontal line should be adjusted to be over the peaks you want.
          The left and right red lines define your left and right cutoff."),

        p("You must input the lowest and highest mass peak. (This should be where a user clicks two points to set the points to fit the linear model, but since this is just a proof of concept...)"),

        shiny::fileInput(inputId = "inputFile",
                         label = "Select an image (only png has been tested)",
                         multiple = FALSE,
                         accept = NULL),
        uiOutput("cuttingFuncs"),
        numericInput("lowMass",
                     "Enter lowest mass peak found",
                     value = 153.89,
                     min = NA,
                     max = NA,
                     step = NA,
                     width = NULL),
        numericInput("highMass",
                     "Enter highest mass peak found",
                     value = 514.11,
                     min = NA,
                     max = NA,
                     step = NA,
                     width = NULL),
        tableOutput("modelOut")

      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"),
        plotOutput("distPlot2")
      )
    )
  )
}
