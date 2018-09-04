library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinyjs)

# Lê os dados do csv
temp_data <- read.table("tas_1991_2015.csv", sep=";", header = TRUE)
temp_data$tas <- as.numeric(sub(",", ".", temp_data$tas, fixed=TRUE))

# Função personalizada para fazer um gráfico com coordenadas polares
# mas linhas retas
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# UI Principal do programa
ui <- fluidPage(
  titlePanel("Temperaturas Médias no Brasil"),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "sld_date",
        label = "Ano",
        min = 1991,
        max = 2015,
        value = 1991,
        animate = TRUE
      ),
      checkboxInput(
        inputId = "chk_multiple",
        label = "Intervalo de Datas",
        value = FALSE
      ),
      selectInput(
        inputId =  "date_from", 
        label = "Ano Inicial", 
        choices = 1991:2015
      ),
      selectInput(
        inputId =  "date_to", 
        label = "Ano Final", 
        choices = 1991:2015,
        selected=2015
      ),
      radioButtons("cbx_coordtype", "Tipo de Coordenada",
                         c("Polar" = "pol",
                           "Cartesiana" = "car"))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Gráfico", plotOutput("temp_plot")),
                  tabPanel("Tabela", tableOutput("temp_table"))
      )
    )
  )
)

# Lógica do servidor
server <- function(input, output) {
  output$temp_plot <- renderPlot(
    {
      # Múltiplos anos ou ano único?
      if(input$chk_multiple) {
        start_year <- input$date_from
        end_year <- input$date_to
      } else {
        start_year <- input$sld_date
        end_year <- input$sld_date
      }
      total_year = as.numeric(end_year) - as.numeric(start_year) + 1
      
      sub_data <- subset(temp_data, temp_data$Year >= start_year & temp_data$Year <= end_year)
      sub_data$Year <- as.factor(sub_data$Year)
      
      getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
      
      if(input$cbx_coordtype == "pol") {
        ggplot(data=sub_data, aes(x=Month, y=tas, color=Year, group=Year)) + 
          geom_line() + 
          geom_point() +
          coord_radar() +
          ylim(20, 30) +
          xlab("Mês") +
          ylab("Temperatura (ºC)") +
          scale_x_continuous(breaks=1:12) +
          theme_minimal()
          # + scale_color_manual(values = getPalette(total_year))
      } else {
        ggplot(data=sub_data, aes(x=Month, y=tas, color=Year, group=Year)) + 
          geom_line() + 
          geom_point() +
          coord_cartesian() +
          ylim(20, 30) +
          xlab("Mês") +
          ylab("Temperatura (ºC)") +
          scale_x_continuous(breaks=1:12) +
          theme_minimal()
        # + scale_color_manual(values = getPalette(total_year))
      }
    }
  )
  
  # Tabela de dados
  output$temp_table <- renderTable(
    {
      if(input$chk_multiple) {
        start_year <- input$date_from
        end_year <- input$date_to
      } else {
        start_year <- input$sld_date
        end_year <- input$sld_date
      }
      total_year = as.numeric(end_year) - as.numeric(start_year) + 1
      
      sub_data <- subset(temp_data, temp_data$Year >= start_year & temp_data$Year <= end_year)
      tbl <- select(sub_data, Month, Year, tas)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

