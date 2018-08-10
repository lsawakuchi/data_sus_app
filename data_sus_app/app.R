## carregar a tabela notas_especialidades de uma especialidade de um cnes
## 
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(rmarkdown)
library(RMySQL)

dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}


loadBase <-function(cnes, especialidade){
  mydb <- dbConnect(MySQL(), user="master", dbname = "mais_saude", password = "analytics", host = "35.193.221.159")
  df <- dbSendQuery(mydb, paste0("select * from mais_saude.notas_especialidades where especialidade='",
                                 especialidade, "' and cnes=",cnes))
  
  dta <- fetch(df, n=-1)
  
  dbDisconnect(mydb)
  
  dta["data"] <- substr(dta$data, 1, 7)
  
  return(dta)
}

plotData <- function(data, item){
  
  if(item == 5 | item == 9){
    leg <- "Equipamentos"
  }else if(item == 6 | item == 10){
    leg <- "Leitos"
  }else if(item == 7 | item == 11){
    leg = "Profissionais"
  }else{
    leg = "Habilitações"
  }
  M <- data$data
  H <- data[item]
  H <- unlist(H, use.names = FALSE)
  y_max <- max(H)
  par(mar=c(10,10,3,3),cex.axis=1, cex.lab=1.2)
  barplot(H, names.arg = M, ylab = "comparação com o valor médio", main = paste("Evolução das notas de ", leg), 
          col="#151D3E", las=2, ylim = c(0, y_max))
  
  
}

plotData2 <- function(data, item){
  if(item == 5 | item == 9){
    leg <- "Equipamentos"
  }else if(item == 6 | item == 10){
    leg <- "Leitos"
  }else if(item == 7 | item == 11){
    leg = "Profissionais"
  }else{
    leg = "Habilitações"
  }
  M <- data$data
  H <- data[item]
  H <- unlist(H, use.names = FALSE)
  y_max <- max(H)
  par(mar=c(10,10,5,5),cex.axis=1, cex.lab=1.2)
  
  ggplot(data = data, aes(x = M, y = H)) + geom_bar(stat = "identity", fill = "#3895E1") + 
    theme(panel.background = element_rect(fill = "white"),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1),
          axis.text.x =  element_text(angle = 45, hjust = 0, vjust = 0)) + ggtitle(paste("Evolução das notas de ", leg)) + 
    labs(y = "Comparação com o valor médio", x = "")
  
}


ui <- fluidPage(theme = shinytheme("superhero"), img(src = "cap2.png", heigh = 90,width = 2000),
  #titlePanel(h1(strong("Planos Privados"), style="color:#151D3E")),
  titlePanel(h1("Notas por Especialidade", style="color:white")),
  sidebarLayout(
    sidebarPanel(
      numericInput("cnes", h4("Entre com um cnes: "), value = 3808),
      selectInput("especialidade", h4("Selecione a especialidade"),
                  choices = c("Cardiologia",
                              "Fisioterapia",
                              "Fonoaudiologia",
                              "Hematologia",
                              "Nefrologia",
                              "Neurologia",
                              "Obstetricia",
                              "Odontologia",
                              "Oftalmologia",
                              "Oncologia",
                              "Ortopedia",
                              "Pediatria",
                              "Psiquiatria",
                              "Radiologia"),
                  selected = "Cardiologia"),
      selectInput("item", h4("Selecione um item:"),
                  choices = c("Equipamentos",
                              "Leitos",
                              "Profissionais",
                              "Habilitações"),
                  selected = "Equipamentos"),
      radioButtons("comp", h4("Comparar com: "), 
                         choices = list("Uf" = 1,
                                        "Brasil" = 2),
                         selected = 1, inline = TRUE),
      numericInput("obs", label = h4("Numero de obsevacoes para exibir: "), value = 10 ),
      downloadButton("report", "Gerar Report")
    ),
    mainPanel(
              h3(textOutput("caption", container = span)),
              plotlyOutput("plot"),
              #br(),
              h4(textOutput("leg_summary")),
              verbatimTextOutput("summary"),
              h4(textOutput("leg_view", container = span)),
              tableOutput("view")
              )
  )
  
)

server <- function(input, output){
  
  output$caption <- renderText({
    "Resumo dos Dados"
  })
  output$plot <- renderPlotly({
    dta <- loadBase(input$cnes, input$especialidade)
    if(dim(dta)[1] == 0){
      return()
    }
    if(is.null(input$comp)){
      Sys.sleep(2)
    }
    if(input$item == "Equipamentos" & input$comp == "1"){
      plotData2(dta, 5)
    }else if (input$item == "Equipamentos" & input$comp == "2"){
      plotData2(dta, 9)
    }else if (input$item == "Leitos" & input$comp == "1"){
      plotData2(dta, 6)
    }else if(input$item == "Leitos" & input$comp == "2"){
      plotData2(dta, 10)
    }else if(input$item == "Profissionais" & input$comp == "1"){
      plotData2(dta, 7)
    }else if (input$item == "Profissionais" & input$comp== "2"){
      plotData2(dta, 11)
    }else if(input$item == "HabilitaÃ§Ãµes" & input$comp == "1"){
      plotData2(dta, 8)
    }else{
      plotData2(dta, 12)
    }
    
    
  })
  output$leg_summary <- renderText({
    paste("Resumo dos dados de ", input$especialidade)
  })
  
  output$summary <- renderPrint({
    dta <- loadBase(input$cnes, input$especialidade)
    if (dim(dta)[1] == 0){
      "Especialidade nao disponivel para o cnes consultado"
    }else{
      summary(dta)
    }
    
  })
  
  output$leg_view <- renderText({
    paste("Primeiras ", input$obs, "observacoes da tabela de ", input$especialidade)
  })
  output$view <- renderTable({
    head(loadBase(input$cnes, input$especialidade), n=input$obs)
    
  })
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # copy the report file to a temporary directory before processing it,
      # in case we don't have permissions to the current working dir (which
      # can happen when deployed)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(a = (input$cnes), 
                     b = (input$especialidade), 
                     c = (input$item), 
                     d = (input$comp), 
                     e = (output$plot),
                     f = (output$summary), 
                     g = (output$view))
     # rmarkdown::render(tempReport, output_file = "report.pdf", params = params)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        options(tinytex.verbose = TRUE))
      # envir = new.env(parent = globalenv())
    }
  )

  
}


shinyApp(ui, server)