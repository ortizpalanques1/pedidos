## Libraries
library(shinydashboard)
library(mongolite)
library(shiny)
library(DT)
## Functions and variables
title <- tags$a(href='http://www.multilimpiezas.com/',
                tags$img(src = "multilogo.png", height = '50', width = '77'),'Multilimpiezas')
fecha <- Sys.time()
year <- format(as.Date(fecha, format="%Y-%m-%d"),"%Y")
month <- format(as.Date(fecha, format="%Y-%m-%d"),"%m")
day <- format(as.Date(fecha, format="%Y-%m-%d"),"%d")
source(file="opcionesMongoDB.R", local=T)[1]
# options(mongodb = list(
#     "host" = "qem.rodsc.mongodb.net",
#     "username" = "DBQEM",
#     "password" = "dbqem"
# ))
cargador <- function(x,y){
  databaseName <- x
  collectionName <- y
  db <<-  mongo(collection = collectionName,
                url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s?retryWrites=true&w=majority",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
}
## Load relevant data
cargador("ISO","productos")
productos <- db$find()
cargador("ISO","proveedores")
proveedores <- db$find()
cargador("ISO","trabajadores")
trabajadores <- db$find()
nombreCompleto <- paste(trabajadores$Nombres, trabajadores$Apellido_1, sep=" ")
cargador("ISO","implantacion")
implantacion <- db$find()
## Initial data frame
pedidoTabla<- NULL
print(pedidoTabla)
## App
shinyApp(
  ui <-  dashboardPage(title = "PEDIDOS",
      dashboardHeader(title=title,titleWidth = 300),
      dashboardSidebar(disable = TRUE ),
      dashboardBody(
        fluidRow(
          div( name = "hacerPedido",
            column(
              width = 3,
              selectInput("supplier",
                          "Proveedor",
                          choices = levels(as.factor(productos$PROVEEDOR))),
              uiOutput("secondSelection"),
              numericInput("quantity", "Cantidad", 1,
                           min = 1, max = 100)
            ),
            column(
              width = 3,
              actionButton("add","Añadir",icon = icon("plus-square")),
              numericInput(inputId = "row.selection", label = "Select row to be deleted", min = 1, max = 100, value = ""),
              actionButton(inputId = "delete", label = "Borrar", icon = icon("minus"))
          )
         ),
         div( name = "detallePedido",
           column(
             width = 3,
             selectInput("direccion", "Direccion de envío", choices = implantacion$Direccion),
             selectInput("contacto","Responsable", choices = nombreCompleto)
           ),
           column(
             width = 3,
             numericInput("desde","Hora Inicial",8,min = 8, max = 16),
             numericInput("hasta","Hora Final", 8, min = 8, max = 20),
             downloadButton("report", "Hacer Pedido")
           )
         )
         , style = "height:300px"
        ),
        fluidRow(
          column(
            width = 6,
            DT::dataTableOutput("table")
          )
        )
      )
    ),
  server = function(input, output) {
    #################################################################################
    ## Add and delete products
    values <- reactiveValues()
    values$df <- pedidoTabla
    
    observeEvent(input$add,{
      cat("addEntry\n")
      newRow <- data.frame("Codigo" = productos[productos$PRODUCTO==input$product,"CODIGO"],
                           "Producto" = input$product, 
                           "Cantidad" = input$quantity)
      values$df <- rbind(values$df,newRow)
      print(nrow(values$df))
    })
    
    observeEvent(input$delete,{
      cat("deleteEntry\n")
      if(is.na(input$row.selection)){
        values$df <- values$df[-nrow(values$df), ]
      } else {
        values$df <- values$df[-input$row.selection, ]
      }
    })  
    
    output$table = renderDataTable({
      values$df 
    })
    #################################################################################
    ## Controller for the second selection
    output$secondSelection <- renderUI({
      selectInput("product", 
                  "Productos", 
                  choices = as.character(productos[productos$PROVEEDOR==input$supplier,"PRODUCTO"]))
    })
    ##################################################################################
    ## Download report
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(n = input$slider,
                       e = data.frame("LOG"="","ABC"="PCM-06-01 ORDEN DE PEDIDO VERBAL","EDI"="Edición 01/01/2019"),
                       f = values$df)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)