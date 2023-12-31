server <-function (){
    require(shiny)
    require(RMySQL)
    require(XML)
    require(DT)
    require(dplyr)
    require(foam)

shinyServer(function(input, output, session) {

      session$onSessionEnded(function() {print("OUT!");stopApp()})

      output$foo <- clean_selections()

      observeEvent(input$Quit, {
        stopApp(returnValue = invisible())
      })

      observeEvent(input$goButton, {
        output$MSG <- renderText("Ready")
        if (length(input$goButton$name) > 0) {
          output$MSG <- renderText("Select Directory")
          output$MSG <- renderText("Munging Data...")
          # point to files uploaded to temp directory
          fls <- input$goButton$datapath
          # process them into foam objects
          procd <- purrr::map(fls, foam::new)
          # process foam objects into dataframe for kraken
          DATA <- purrr::map(procd,wetqc::format_kraken)

          output$foo2 <- DT::renderDataTable(dplyr::bind_rows(DATA))
          sum_tbl <- purrr::map_df(procd, sum_table_row) %>%
            arrange(sn)

          if (exists("sum_tbl")) {
            sum_tbl$use <- T
            output$foo <- DT::renderDataTable(
              sum_tbl,
              selection = list(selected = which(sum_tbl$valid ==
                                                  FALSE)),
              server = F,
              options = list(dom = "t",
                             pageLength = nrow(sum_tbl)),
              rownames = F
            )
          }
          observeEvent(input$foo_rows_selected, {
            sum_tbl
            sum_tbl$use <- T
            sum_tbl$use[input$foo_rows_selected] <- F
            output$foo <- DT::renderDataTable(
              sum_tbl,
              selection = list(selected = input$foo_rows_selected),
              server = F,
              options = list(dom = "t", pageLength = nrow(sum_tbl)),
              rownames = FALSE
            )
            last <- input$foo_rows_selected
            print(last)
          })
          observeEvent(input$desel, {
            sum_tbl$use <- T
            output$foo <-
              output$foo <- DT::renderDataTable(
                sum_tbl,
                selection = list(selected = NULL),
                server = F,
                options = list(dom = "t", pageLength = nrow(sum_tbl)),
                rownames = FALSE
              )
          })
          output$exprt <-
            downloadHandler(filename <- function() {
              paste("data-", Sys.Date(), ".csv", sep = "")
            }, content <- function(file) {
              OUT <- DATA %>%
                wetqc::remove_deselected(., input$foo_rows_selected) %>%
                dplyr::bind_rows() %>%
                arrange(., sn, Well)

              write.csv(OUT, file, row.names = F)
            })
          observeEvent(input$upload, {
            wetqc::upload_all(remove_deselected(DATA,
                                                input$foo_rows_selected))
          })
        }
      })
    })
  }
