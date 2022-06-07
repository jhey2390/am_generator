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
ui <- (fluidPage(
    titlePanel("Generador de Ayudas Memorias de Cierre de Brecha Digital"),
    h4(paste("Información de distribución Fase 1 al ",format(as.Date(actualizacion),"%d-%m-%Y"),sep="")),
    h4(paste("Información de distribución Fase 2 al ",format(as.Date(fecha2),"%d-%m-%Y"),sep="")),
    h4(paste("Información de actualización de tabletas al ",format(as.Date(file$fec),"%d-%m-%Y"),sep="")),
    
    
    selectInput("selection", 
                h3("Ayuda Memoria a nivel:"),
                list("Nacional", "Departamental")),
    sidebarPanel (
        conditionalPanel(
            condition = "input.selection == 'Nacional'",
            h5("Se imprimirá la AM a nivel Nacional.")
        ),
        conditionalPanel(
            condition = "input.selection == 'Departamental'",
            selectInput("Departamento", "Departamento:",
                        list("Amazonas","Áncash","Apurímac","Arequipa","Ayacucho",
                             "Cajamarca","Cusco","Huancavelica","Huánuco",
                             "Ica","Junín","La Libertad","Lambayeque","Lima",
                             "Loreto","Madre de Dios","Moquegua","Pasco","Piura",
                             "Puno","San Martín","Tacna","Tumbes","Ucayali")),
            textOutput("result")
            
        ),
        downloadButton(outputId = "printAM",label = "Descargar la AM")),
    
    
))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    output$result <- renderText({
        paste("Se imprimirá la AM del departamento de", input$Departamento)
    })
    output$printAM <- downloadHandler(
        content = function(file) {
            params <- list(my_class = ifelse(input$selection=='Nacional',"Nacional",input$Departamento))
            rmarkdown::render(input = "./Plantilla_AM_CBD.Rmd", 
                              output_file = file,
                              output_dir = tempdir(),
                              intermediates_dir = tempdir(),
                              params = params,
                              envir = new.env(parent = globalenv()),
                              encoding="UTF-8")
        },
        filename = function() {paste(gsub(":","",Sys.time()), "AM_CBD",ifelse(input$selection=='Nacional',"Nacional",input$Departamento),".docx")}
    )
    session$allowReconnect(TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
