#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(gargle)
library(googledrive)
library(googlesheets4)
library(tidyverse)

Sys.setlocale(locale = "es_ES.UTF-8")
load("./CBD.rdata")
load("./CBDF2.rdata")
load("./data_consolidada.rdata")
rm(dff2,programados)
listado<-data_consolidada%>%select(codgeo=CODGEO,d_dpto=D_DPTO,d_prov=D_PROV,d_dist=D_DIST) %>% unique() %>% arrange(codgeo)
rm(data_consolidada)
googledrive::drive_auth(path = "your-app-folder-name/.secrets/calcium-chalice-291219-c0edbd3ab245.json")

jp_folder <- "https://drive.google.com/drive/folders/1UjvV0CPbu3kQKLanWTDxRUoNdYsUehSL"
folder_id <- drive_get(as_id(jp_folder))
files <- drive_ls(folder_id)
files<-files %>% 
    mutate(fec=trimws(gsub("[a-zA-Z.-]","",name)),
           fec=paste(substr(fec,1,4),substr(fec,5,6),substr(fec,7,8),sep="-"),
           fec=as.Date(fec))

file<-files %>% filter(fec==max(files$fec)) %>% head(1)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Generador de Ayudas Memorias de Cierre de Brecha Digital"),
    h4(paste("Información de distribución Fase 1 al ",format(as.Date(actualizacion),"%d-%m-%Y"),sep="")),
    h4(paste("Información de distribución Fase 2 al ",format(as.Date(fecha2),"%d-%m-%Y"),sep="")),
    h4(paste("Información de actualización de tabletas al ",format(as.Date(file$fec),"%d-%m-%Y"),sep="")),
    
    selectInput("selection", 
                h3("Ayuda Memoria a nivel:"),
                list("Nacional", "Departamental","Provincial")),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
            condition = "input.selection == 'Nacional'",
            h5("Se imprimirá la AM a nivel Nacional.")),
        uiOutput("inputdpto"),
        uiOutput("inputprov"),
        textOutput("result"),
        downloadButton(outputId = "printAM",label = "Descargar la AM")),
      mainPanel()),
    )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  output$inputdpto<- renderUI({
    if(input$selection %in% c('Departamental','Provincial'))
      {selectInput(inputId = "Departamento", "Departamento:",choices = var_dpto(),multiple = F)}
    })
  output$inputprov<- renderUI({
    if(input$selection == 'Provincial')
      {selectInput(inputId = "Provincia", "Provincia:",choices = var_prov(),multiple = F)}
    })
  dpto <- reactive({
    if (is.null(input$Departamento)) sort(unique(listado$d_dpto)) else input$Departamento})
  var_dpto <- reactive({
    if(is.null(listado)){return()}
    as.list(sort(unique(listado$d_dpto)))
  })
  
  var_prov<- reactive({
      filter(listado, d_dpto %in% as.character(dpto())) %>% 
        pull(d_prov) %>% 
        unique()
    })
    output$result <- renderText({
      if(input$selection == "Departamental")
        paste("Se imprimirá la AM del departamento de", input$Departamento)
      else if(input$selection == "Provincial")
        paste("Se imprimirá la AM de la provincia",input$Provincia,"del departamento de", input$Departamento)
    })
    
    output$printAM <- downloadHandler(
        content = function(file) {
            params <- list(my_class = input$selection,
                           dpto = input$Departamento,
                           prov = input$Provincia
                           )
            rmarkdown::render(input = "./Plantilla_AM_CBD.Rmd", 
                              output_file = file,
                              output_dir = tempdir(),
                              intermediates_dir = tempdir(),
                              params = params,
                              envir = new.env(parent = globalenv()),
                              encoding="UTF-8")
        },
        filename = function() {paste(gsub(":","",Sys.time()),"AM_CBD",ifelse(input$selection=='Nacional',"Nacional",paste(input$Provincia,input$Departamento)),".docx")}
    )
    session$allowReconnect(TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
