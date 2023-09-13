#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## SUPERK

library(shiny)
library(tidyverse)
library(readxl)
library(zip)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(HTML("<title>SUPERKnee Data Selector</title>")),
  
  # Application title
  titlePanel(
    fluidRow(
      column(8, "SUPERKnee Data Selector"),
      column(4, img(height = 50, src="SUPER-KNEE.png"))
    )
  ),
  HTML("This app is to extract relevant data from the SUPERKnee project for analysis. <br/>
       It requires the latest version of the database file to be uploaded, before selecting relevant variables from the menu."),
  br(),
  
  fluidRow(
    column(2, 
           h4("Choose XLSX File"),
           HTML("<em>Please make sure you use the most up-to-date version of the data.</em>"),
           fileInput("file1", "",
                     multiple = FALSE)
    ),
    column(2,
           h4("Select Timepoints"),
           checkboxGroupInput("timepoints", "", choices = c("Pre-Baseline (Recruitment)" = "mpre",
                                                            "Baseline" = "m0", 
                                                            "4 months" = "m4", 
                                                            "8 months" = "m8", 
                                                            "12 months" = "m12"),
                              selected = "m0")
    ),
    column(2, 
           h4("Select PROMs"),
           checkboxGroupInput("proms", "Variables", choices = c("KOOS" = "koos", 
                                                                "ACL QOL" = "aclqol",
                                                                "Tampa" = "tsk",
                                                                "Health Labour Questionnaire" = "hlq",
                                                                "Work Limitations Questionnaire" = "wlq",
                                                                "EQ-5D" = "eq5d",
                                                                "Pain Scores" = "pain",
                                                                "Patient Acceptable Symptom Scale (PASS)" = "pass",
                                                                "Global Rating of Change" = "groc",
                                                                "REALM" = "realm"
           ))
    ),
    column(2,
           h4("Select Physical Measures"),
           checkboxGroupInput("physical", "Variables", choices = c("Anthropometrics" = "anthro",
                                                                   "Clinical Measures" = "clinical",
                                                                   "Physical Function" = "function",
                                                                   "Biodex" = "biodex"
                                                                   
                                                                   
           ))
    ),
    column(2,
           h4("Select Other Information"),
           checkboxGroupInput("other", "Variables", choices = c("Baseline demographics" = "baseline",
                                                                "ACL Knowledge/Beliefs" = "aclknowledge",
                                                                "Goals, expectations and satisfaction" = "goals",
                                                                "4 month questions" = "m4",
                                                                "12 month questions" = "m12"
           ))
    ),
    column(2,
           h4("Select Activity Information"),
           checkboxGroupInput("activity", "Activity", choices = c("Pre-ACL injury information" = "preacl",
                                                                  "Current Sport" = "currentsport",
                                                                  "Current Activity" = "currentactivity",
                                                                  "Tegner" = "tegner"
           ))
    ),
    column(2,
           h4("Extra Information"),
           checkboxGroupInput("extra", "Select", choices = c("Fortnightly and Monthly monitoring" = "fn",
                                                             "Surgery Report Details" = "sx"
           ))
    ),
  ),
  sidebarLayout(
    sidebarPanel(style = "background-color:#ffeae8;",
                 fluidRow(
                   column(6,
                 h4("Download File"),
                 tags$em("If unblinded is selected treatment name and data fields which reveal allocation will be included.
                          If blinded group allocation will be included but coded and concealed."),
                 radioButtons("blinded", "Blinding", choices = c("Fully blinded" = "blind"), selected = "blind"),
                 downloadButton("zipdownload","Download")),
                 column(6,
                        HTML("<br><br>"),
                        radioButtons("format", "Output Format", choices = c("Long format" = "long",
                                                                            "Wide format" = "wide"),
                                     selected = "long"),
                        radioButtons("missing", "Missing Data Format", choices = c("NA (Default)" = "na",
                                                                                   "Blank - \"\" " = "blank"),
                                     selected = "na")
                 )
                 
    )),
    mainPanel()
  )
  # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  varnamegroup <- read.csv("namegroups.csv", header = TRUE, na = "")
  
  data <-  reactive({
    req(input$file1) # wait until file uploaded
    
    readxl::read_xlsx(input$file1$datapath, 
                      sheet = 1, na = "NA")
  })
  
  fnmonth <-  reactive({
    req(input$file1) # wait until file uploaded
    
    readxl::read_xlsx(input$file1$datapath, 
                      sheet = 2, na = "NA")
  })
  
  sxdetails <-  reactive({
    req(input$file1) # wait until file uploaded
    
    readxl::read_xlsx(input$file1$datapath, 
                      sheet = 3, na = "NA")
  })
  
  namesout <- reactive({
    varnamegroup %>%
      dplyr::filter(namegroup %in% c(input$proms, input$physical, input$other, input$activity))
  })
  
  dataout <- reactive({
    data() %>%
      dplyr::filter(timepoint %in% input$timepoints) %>%
      dplyr::select(id, sex, gender, age, dominantleg, dominanthand, aclrside, timepoint, timepoint_actual, baseline_date, 
                    completed, group, dob, postcode, any_of(namesout()$varname))
    # need to add firstphysiosession_date back here one add unblinded back.  
  })
  
  dataout2 <- eventReactive(input$blinded,{
    if(input$blinded == "blind") {
      dataout() %>%
        dplyr::select(!any_of(c("firstphysiosession_date", "expect_tegner_4m", "expect_pain_4m", "expect_qol_4m", "adherence_selfrated", "treatment_satisfaction",
                                contains("blgoals"))))
    }
    else
    { dataout() %>%
        dplyr::mutate(group = recode(group, "4" = "Control", "7" = "Super"))
    }
  })
  
  dataout3 <- eventReactive(input$format,{
    if(input$format == "wide") {
      dataout2() %>%
        tidyr::pivot_longer(-c(id, sex, gender, age, dominantleg, dominanthand, aclrside, group, dob, postcode, timepoint), 
                            names_to = "var",
                            values_to = "val",
                            values_transform = list(val = as.character)) %>%
        tidyr::pivot_wider(id_cols = c(id, sex, gender, age, dominantleg, dominanthand, aclrside, group, dob, postcode),
                           names_from = c(timepoint, var),
                           names_sep = "_",
                           values_from = "val")
    }
    else
      dataout2()
  })
  
  
  fnmonthout <- reactive({
    fnmonth() 
  })
  
  # Add Data file if Data are selected in menu
  file1 <- reactive({
    if(length(input$proms) > 0 | length(input$physical) > 0 | length(input$other) > 0 | length(input$activity) > 0) 
    { c(paste(paste("SUPERK Data", Sys.Date(), sep = "_"), "csv", sep = ".")) }
  })
  
  # Add Injury testing file if selected in menu
  
  file2 <-reactive({
    if("fn" %in% input$extra) { c(paste(paste("SUPERK Fortnightly", Sys.Date(), sep = "_"), "csv", sep = ".")) }
  })
  
  # Add Surgical details  file if selected in menu
  
  file3 <-reactive({
    if("sx" %in% input$extra) { c(paste(paste("SUPERK Surgical Details", Sys.Date(), sep = "_"), "csv", sep = ".")) }
  })
  
  
  # Create list of which files to download
  files <- reactive({
    c(file1(), file2(), file3())
  })
  
  
  output$zipdownload = downloadHandler(
    
    filename = paste(paste("SUPERK Data", Sys.Date(), sep = "_"), "zip", sep = "."),
    content = function(file){
      
      missingval <- switch(input$missing,
                           "na" = "NA",
                           "blank" = "")
      
      # Set temporary working directory
      owd <- setwd( tempdir())
      on.exit( setwd( owd))
      
      # Save the histograms (a loop can be used here for a bunch of plots)
      write_csv(dataout3(), paste(paste("SUPERK Data", Sys.Date(), sep = "_"), "csv", sep = "."), na = missingval)
      write_csv(fnmonthout(), paste(paste("SUPERK Fortnightly", Sys.Date(), sep = "_"), "csv", sep = "."), na = missingval)
      write_csv(sxdetails(), paste(paste("SUPERK Surgical Details", Sys.Date(), sep = "_"), "csv", sep = "."), na = missingval)
      
      # Zip them up
      zip(file, files())
    },
    contentType = "application/zip"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)