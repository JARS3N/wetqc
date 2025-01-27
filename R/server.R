server <- function() {
  require(shiny)
  require(RMySQL)
  require(XML)
  require(DT)
  require(dplyr)
  require(foam)

  shinyServer(function(input, output, session) {
    
    # Initialize variables
    DATA <- NULL
    sum_tbl <- NULL

    session$onSessionEnded(function() { print("OUT!"); stopApp() })

    # Clear tables and data on purge
    purge_data <- function() {
      DATA <<- NULL
      sum_tbl <<- NULL
      output$foo <- DT::renderDataTable(data.frame())
      output$foo2 <- DT::renderDataTable(data.frame())
    }

    # Initialize outputs
    output$foo <- DT::renderDataTable(data.frame())
    output$foo2 <- DT::renderDataTable(data.frame())

    observeEvent(input$Quit, {
      stopApp(returnValue = invisible())
    })

    observeEvent(input$goButton, {
      if (!is.null(DATA)) {
        output$MSG <- renderText("Please purge the previous data before processing a new set.")
        return()
      }

      output$MSG <- renderText("Ready")
      if (length(input$goButton$name) > 0) {
        output$MSG <- renderText("Processing new dataset...")
        
        # Process uploaded files
        fls <- input$goButton$datapath
        print(fls)
        
        # Convert to foam objects
        procd <- purrr::map(fls, foam::new)
        print(procd[[1]])
        
        # Process foam objects into dataframes for kraken
        DATA <<- purrr::map(procd, wetqc::format_kraken)
        DATA <<- lapply(DATA, function(u) {
          x <- names(u)
          names(u) <- gsub("pH_target", "Target", x)
          u
        })

        # Render the main table
        output$foo2 <- DT::renderDataTable(dplyr::bind_rows(DATA))
        sum_tbl <<- purrr::map_df(procd, sum_table_row) %>%
          arrange(sn)

        if (!is.null(sum_tbl)) {
          sum_tbl$use <- TRUE
          output$foo <- DT::renderDataTable(
            sum_tbl,
            selection = list(selected = which(sum_tbl$valid == FALSE)),
            server = FALSE,
            options = list(dom = "t", pageLength = nrow(sum_tbl)),
            rownames = FALSE
          )
        }
      }
    })

    # Handle purging of data
    observeEvent(input$purge, {
      purge_data()
      output$MSG <- renderText("Data purged. Ready for new upload.")
    })

    # Download handler
    output$exprt <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        OUT <- DATA %>%
          wetqc::remove_deselected(., input$foo_rows_selected) %>%
          dplyr::bind_rows() %>%
          arrange(sn, Well)
        write.csv(OUT, file, row.names = FALSE)
      }
    )

    # Handle upload and clear data
    observeEvent(input$upload, {
      if (is.null(DATA)) {
        output$MSG <- renderText("No data to upload. Please process a dataset first.")
        return()
      }

      print("Uploading data...")
      wetqc::upload_all(remove_deselected(DATA, input$foo_rows_selected))
      print("Upload complete.")
      
      purge_data()  # Clear data after upload
      output$MSG <- renderText("Data uploaded and cleared. Ready for new dataset.")
    })
  })
}




# server <-function (){
#   require(shiny)
#   require(RMySQL)
#   require(XML)
#   require(DT)
#   require(dplyr)
#   require(foam)
#   
#   shinyServer(function(input, output, session) {
#     
#     session$onSessionEnded(function() {print("OUT!");stopApp()})
#     
#     output$foo <- clean_selections()
#     
#     observeEvent(input$Quit, {
#       stopApp(returnValue = invisible())
#     })
#     
#     observeEvent(input$goButton, {
#       output$MSG <- renderText("Ready")
#       if (length(input$goButton$name) > 0) {
#         output$MSG <- renderText("Select Directory")
#         output$MSG <- renderText("Munging Data...")
#         # point to files uploaded to temp directory
#         fls <- input$goButton$datapath
#         print(fls)
#         # process them into foam objects
#         procd <- purrr::map(fls, foam::new)
#         print(procd[[1]])
#         # process foam objects into dataframe for kraken
#         DATA <- purrr::map(procd,wetqc::format_kraken)
#         DATA <- lapply(DATA, function(u) {
#           x <- names(u)
#           names(u) <- gsub("pH_target", "Target", x)
#           u
#         })
#         head(DATA[[1]])
#         output$foo2 <- DT::renderDataTable(dplyr::bind_rows(DATA))
#         sum_tbl <- purrr::map_df(procd, sum_table_row) %>%
#           arrange(sn)
#         
#         if (exists("sum_tbl")) {
#           sum_tbl$use <- T
#           output$foo <- DT::renderDataTable(
#             sum_tbl,
#             selection = list(selected = which(sum_tbl$valid ==
#                                                 FALSE)),
#             server = F,
#             options = list(dom = "t",
#                            pageLength = nrow(sum_tbl)),
#             rownames = F
#           )
#         }
#         observeEvent(input$foo_rows_selected, {
#           sum_tbl
#           sum_tbl$use <- T
#           sum_tbl$use[input$foo_rows_selected] <- F
#           output$foo <- DT::renderDataTable(
#             sum_tbl,
#             selection = list(selected = input$foo_rows_selected),
#             server = F,
#             options = list(dom = "t", pageLength = nrow(sum_tbl)),
#             rownames = FALSE
#           )
#           last <- input$foo_rows_selected
#           print(last)
#         })
#         observeEvent(input$desel, {
#           sum_tbl$use <- T
#           output$foo <-
#             output$foo <- DT::renderDataTable(
#               sum_tbl,
#               selection = list(selected = NULL),
#               server = F,
#               options = list(dom = "t", pageLength = nrow(sum_tbl)),
#               rownames = FALSE
#             )
#         })
#         output$exprt <-
#           downloadHandler(filename <- function() {
#             paste("data-", Sys.Date(), ".csv", sep = "")
#           }, content <- function(file) {
#             OUT <- DATA %>%
#               wetqc::remove_deselected(., input$foo_rows_selected) %>%
#               dplyr::bind_rows() %>%
#               arrange(., sn, Well)
#             
#             write.csv(OUT, file, row.names = F)
#           })
#         observeEvent(input$upload, {
#           print("before upload")
#           wetqc::upload_all(remove_deselected(DATA,
#                                               input$foo_rows_selected))
#           print("post upload")
#           output$foo <- clean_selections()
#           DATA<-NULL
#         })
#       }
#     })
#   })
# }
