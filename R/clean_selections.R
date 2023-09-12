clean_selections <-function(){
  #cleans summary table selections
  DT::renderDataTable(
    NULL,
    selection = list(selected = NULL),
    server = F,
    options = list(dom = "t"),
    rownames = FALSE
  )
}
