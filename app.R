#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(stringr)

zebrafish <- dbPool(RSQLite::SQLite(), dbname = "zfish.sqlite")

responses_df <- data.frame(Batch=integer(),
                   Name=character(),
                   Birthday=as.Date(character()),
                   Death=as.Date(character()),
                   Status=character(),
                   User=character(),
                   Strain=character(),
                   Generation=character(),
                   Adults=integer(),
                   Nursery=integer(),
                   Mother=integer(), 
                   Father=integer(),
                   Notes=character())

dbWriteTable(zebrafish, "responses_df", responses_df, overwrite = FALSE, append = TRUE)

jsCode <- 'shinyjs.winprint = function (){
              var divToPrint=document.getElementById("print-form");
              newWin= window.open("");
              newWin.document.write(divToPrint.outerHTML);
              newWin.print();
              newWin.close();
          }'



#Label mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

# ui
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  extendShinyjs(text = jsCode, functions = c("winprint")),
  titlePanel("Baraban Lab Zebrafish Manager"),
  fluidRow(
    actionButton("add_button", "Add", icon("fish")),
    actionButton("edit_button", "Edit", icon("edit")),
    actionButton("copy_button", "Copy", icon("copy")),
    actionButton("delete_button", "Delete", icon("trash-alt")),
    actionButton("print_button", "Print", icon("print")),
    downloadButton("zebrafish_csv", "Download CSV", class="butt")
  ),
  br(),
  fluidRow(width="100%",
           dataTableOutput("responses_table", width = "100%")
  ), 
  fluidRow(width="25%", 
           dataTableOutput("total_table", width="25%"))
  
)

server <- function(input, output, session) {
  
  #load responses_df and make reactive to inputs  
  responses_df <- reactive({
    
    #make reactive to
    input$submit
    input$submit_edit
    input$copy_button
    input$delete_button
    input$print_button

    dbReadTable(zebrafish, "responses_df")
    
  })  

  
  #List of mandatory fields for submission
  fieldsMandatory <- c("Name", "Status", "User", "Strain", "Generation")
  
  observe({
    
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  entry_form <- function(button_id){
    SQL_df <- dbReadTable(zebrafish, "responses_df")
    Batch_Select <- SQL_df[, "Batch"]
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:400px}")),
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
            fluidPage(
              fluidRow(
                textAreaInput("Name", labelMandatory("Name"), placeholder = "", height = 100, width = "354px"),
                dateInput("Birthday", labelMandatory("Birthday"), value = Sys.Date()),
                selectInput("Status", labelMandatory("Status"), 
                            choices=c("Alive", "Dead"), selected = "Alive"),
                conditionalPanel(condition = "input.Status == 'Dead'",
                                 textInput("Death", "Date of Death (Format:yyyy-mm-dd)")),
                textInput("User",labelMandatory("User")),
                selectInput("Strain", labelMandatory("Strain"), 
                            choices=c("TL", "AB", "TLAB")),  
                selectInput("Generation", labelMandatory("Generation"),
                              choices=c("NA","F0","F1", "F2", "F3", "F4", "F5"), selected = "NA"),
                numericInput("Adults", "Adults", 0),
                numericInput("Nursery", "Nursery", 0),
                selectInput("Mother", "Mother", choices = c(" ", Batch_Select)),
                selectInput("Father", "Father", choices = c(" ", Batch_Select)),
                textAreaInput("Notes", "Notes", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
  
  fieldsAll <- c("Name", "Birthday", "Death", "User","Status","Strain", "Generation", "Adults", "Nursery","Mother","Father","Notes")
  
  #save form data into data_frame format
  SQL_df <- 
  formData <- reactive({
    
    formData <- data.frame(Batch = ifelse((length(dbReadTable(zebrafish, "responses_df")[,c("Batch")]) == 0), 1, 
                                          (max(dbReadTable(zebrafish, "responses_df")[,c("Batch")]) + 1)),
                           Name = input$Name,
                           User = input$User,
                           Strain = input$Strain,
                           Status = input$Status,
                           Death = format(as.Date(input$Death)),
                           Generation = input$Generation,
                           Adults = input$Adults,
                           Nursery = input$Nursery,
                           Birthday = format(as.Date(input$Birthday)),
                           Mother = input$Mother,
                           Father = input$Father,
                           Notes = input$Notes,
                           stringsAsFactors = FALSE)
    return(formData)
    
  })
  
  appendData <- function(data){
    quary <- sqlAppendTable(zebrafish, "responses_df", data, row.names = FALSE)
    dbExecute(zebrafish, quary)
  }
  
  observeEvent(input$add_button, priority = 20,{
    
    entry_form("submit")
    
  })
  
  output$zebrafish_csv = downloadHandler(
    filename = function() {
      paste("Baraban_Lab_DB_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      SQL_df <- dbReadTable(zebrafish, "responses_df")
      write.csv(data.frame(SQL_df), file, row.names = F)
    }
  )
  
  observeEvent(input$print_button, priority = 20,{
    showModal(
      if(length(input$responses_table_rows_selected)==1 ){
        
        SQL_df <- dbReadTable(zebrafish, "responses_df")
        user_selection <- SQL_df[input$responses_table_rows_selected,"User"]
        batch_selection = SQL_df[input$responses_table_rows_selected, "Batch"]
        name_selection = SQL_df[input$responses_table_rows_selected, "Name"]
        birthday_selection = format(as.Date(SQL_df[input$responses_table_rows_selected, "Birthday"]))
        obj = str_split(name_selection, ";")
        modalDialog(
          div(id=("print-form"),
            renderUI(HTML(do.call(paste, c(obj, collapse = "<br/>")))),
            renderText(paste("Batch: ",batch_selection), quoted=FALSE),
            renderText(paste("D.O.B: ", birthday_selection), quoted=FALSE),
            renderText(user_selection, quoted=FALSE)), easyClose=TRUE, 
            size = "s",
            footer = tagList(
              modalButton("Dismiss"),
              actionButton("print_window", "Print")
            )
        )
      } else if (length(input$responses_table_rows_selected)>1){
        modalDialog(
          title = "Warning",
          paste("Please select only 1 row." ),easyClose = TRUE
        )
      } else if (length(input$reponses_table_rows_selected)<1){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      }
      )
    
    
  })
  
  observeEvent(input$print_window, priority = 20, {
    js$winprint()
  })
  observeEvent(input$submit, priority = 20,{
    
    appendData(formData())
    shinyjs::reset("entry_form")
    removeModal()
    
  })
  
  #delete data
  deleteData <- reactive({
    
    SQL_df <- dbReadTable(zebrafish, "responses_df")
    row_selection <- SQL_df[input$responses_table_rows_selected, "Batch"]
    
    quary <- lapply(row_selection, function(nr){
      
      dbExecute(zebrafish, sprintf('DELETE FROM "responses_df" WHERE "Batch" == ("%s")', nr))
    })
  })
  
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      deleteData()
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
  })
  
  unique_id <- function(data){
    replicate(nrow(data),round(as.numeric(Sys.time())))
  }
  
  copyData <- reactive({
    
    SQL_df <- dbReadTable(zebrafish, "responses_df")
    row_selection <- SQL_df[input$responses_table_rows_selected, "Batch"] 
    SQL_df <- SQL_df %>% filter(Batch %in% row_selection)
    SQL_df$Batch <- unique_id(SQL_df)
    
    quary <- sqlAppendTable(zebrafish, "responses_df", SQL_df, row.names = FALSE)
    dbExecute(zebrafish, quary)
    
  })
  
  observeEvent(input$copy_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      copyData()
    }
    
    showModal(
      
      if(length(input$responses_table_rows_selected) < 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select row(s)." ),easyClose = TRUE
        )
      })
    
  })
  
  observeEvent(input$edit_button, priority = 20,{
    
    SQL_df <- dbReadTable(zebrafish, "responses_df")
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Warning",
          paste("Please select only one row." ),easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Warning",
          paste("Please select a row." ),easyClose = TRUE)
      })  
    
    if(length(input$responses_table_rows_selected) == 1 ){
      
      entry_form("submit_edit")
      
      updateTextAreaInput(session, "Name", value = SQL_df[input$responses_table_rows_selected, "Name"])
      updateDateInput(session, "Birthday", value = SQL_df[input$responses_table_rows_selected, "Birthday"])
      updateTextInput(session, "User", value = SQL_df[input$responses_table_rows_selected, "User"])
      updateSelectInput(session, "Status", choices= c("Alive", "Dead"), 
                        selected = SQL_df[input$responses_table_rows_selected, "Status"])
      conditionalPanel(condition = "input.Status == 'Dead'", 
                       updateTextInput(session, "Death", value = SQL_df[input$responses_table_rows_selected, "Death"]))
      updateSelectInput(session, "Generation", choices=c("NA","F0", "F1", "F2", "F3", "F4", "F5"), 
                        selected = SQL_df[input$responses_table_rows_selected, "Generation"])
      updateSelectInput(session, "Strain", choices = c("TL", "AB", "TLAB"), 
                        selected = SQL_df[input$responses_table_rows_selected, "Strain"])
      updateNumericInput(session, "Adults", value = SQL_df[input$responses_table_rows_selected, "Adults"])
      updateNumericInput(session, "Nursery", value = SQL_df[input$responses_table_rows_selected, "Nursery"])
      updateSelectInput(session, "Mother", choices = c(" ", SQL_df[, "Batch"]), 
                        selected = SQL_df[input$responses_table_rows_selected, "Mother"])
      updateSelectInput(session, "Father", choices = c(" ", SQL_df[, "Batch"]), 
                        selected = SQL_df[input$responses_table_rows_selected, "Father"])
      updateTextAreaInput(session, "Notes", value = SQL_df[input$responses_table_rows_selected, "Notes"])
      
    }
    
  })
  
  observeEvent(input$submit_edit, priority = 20, {
    
    SQL_df <- dbReadTable(zebrafish, "responses_df")
    row_selection <- SQL_df[input$responses_table_row_last_clicked, "Batch"] 
    dbExecute(zebrafish, sprintf('UPDATE "responses_df" SET "Name" = ?, "Birthday" = ?, "User" = ?, "Status" = ?, "Death" = ?,
                            "Generation" = ?, "Strain" = ?, "Adults" = ?, "Nursery" = ?, "Mother" = ?, "Father" = ?, "Notes" = ? 
                                 WHERE "Batch" = ("%s")', row_selection), 
              param = list(input$Name,
                           format(as.Date(input$Birthday)),
                           input$User,
                           input$Status,
                           format(as.Date(input$Death)), 
                           input$Generation,
                           input$Strain, 
                           input$Adults,
                           input$Nursery,
                           input$Mother,
                           input$Father,
                           input$Notes))
    removeModal()
    
  })
  
  output$responses_table <- DT::renderDataTable({
    
    table <- responses_df()
    table <- table[,c(1:12)]
    names(table) <- c("Batch", "Name", "Birthday","Death", "Status", "User", "Strain", "Generation",
                      "Adults", "Nursery","Mother", "Father")
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(searching = TRUE, lengthChange = FALSE, 
                                      pageLength = 20, order = list(0, 'desc'),
                                      columnDefs = list(
                                        list(targets=1, 
                                             render = JS("function(data){return data.replace(/;/g, '<br>');}"))
                                      ))
    )
    
  })
  
  output$total_table <- DT::renderDataTable({
    
    tot_table <- responses_df()
    sum_adults <- sum(tot_table[, "Adults"])
    sum_nursery <- sum(tot_table[, "Nursery"])
    
    sum_table <- expand.grid(Total_Adults = sum_adults, Total_Nursery = sum_nursery)
    table <- datatable(sum_table, rownames = FALSE, 
                       options = list(searching = FALSE, lengthChange = FALSE, dom = 't'))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)