ui <-
  function ()
  {
    require(shiny)
    require(shinyjs)
    shinyUI(pageWithSidebar(
      headerPanel("Upload WetQC Data"),
      sidebarPanel(
        fileInput(
          "goButton",
          "select wave files",
          accept = c("asyr", ".asyr"),
          multiple = T
        ),
        width = 1.5
      ),
      mainPanel(
        actionButton("Quit", "Exit", icon = icon("times-circle")),
        shiny::downloadButton("exprt", "download"),
        actionButton("upload",
                     "Database Upload", icon = icon("send")),
        br(),
        strong("Highlighted runs are not uploaded to the database"),
        tabsetPanel(
          tabPanel(
            "Selection & Validation",
            DT::dataTableOutput("foo"),
            actionButton("desel", "Deselect All Rows", icon = icon("undo"))
          ),
          tabPanel(
            "Terms",
            strong("pH.Status & O2.Status"),
            p("Taken from calibration data,checks if each status is \"Good\""),
            br(),
            strong("Positives"),
            p(
              "Checks if the assay is minimally performing where the median Gain or Ksv is greater than 0 and ignores NA values where values are not applicable to the assay"
            ),
            br(),
            strong("Injection"),
            p(
              "Determines if 95% or greater of the F0 for the injections are < 10% difference from the expected F0"
            )
          ),
          tabPanel("Data", DT::dataTableOutput("foo2"))
        )
      )
    ))
  }
