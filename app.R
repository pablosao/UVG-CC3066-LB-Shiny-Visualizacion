#
#           UVG-Reportes-COVID-19
#
#   Modifico: Pablo Sao
#   Fecha: 14-10-2020
#   Descripción: Se incorpora filtro de fecha, y actualización de datos de la gráfica
#                de sintomas reportados
#

library(dplyr)
library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(DT)
library(ECharts2Shiny)
#install.packages("ini")
library(ini)
#source("queryManager.R")
library(plotly)
library(leaflet)
library(viridis)
library(rworldmap)
library(maps)
library(ggmap)




data <- read.csv("owid-covid-data.csv") 

#View(data)


ui <- dashboardPage(
    title = "Reportes COVID-19",
    dashboardHeader(title= "Reportes COVID-19 UVG"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("General", tabName = "general", icon = icon("chart-pie"))
            #menuItem("Casos", tabName = "cases", icon = icon("caret-right")),
            #menuItem("Afectados", tabName = "afected", icon = icon("caret-right")),
            #menuItem("Regiones", tabName = "regions", icon = icon("caret-right")),
            #menuItem("Sintomas Comunes", tabName = "symptoms", icon = icon("caret-right"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML('
              .skin-blue .main-header .navbar{
                background-color: #21822b;
              }
              .skin-blue .main-header .logo{
                background-color: #21822b;
              }
              .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a{
                border-left-color: #21822b;
              }
              .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
                background-color: #3b3a3b;
              }
        '))
        ),
        tabItems(
            tabItem("general",
                    fluidPage(
                        #h1("General"),
                        #hr(),
                        # PSAO / 31-05-2020 / Se agrega filtro de rango de fechas
                        box(
                            dateRangeInput('RangoFechas',
                                           label = 'Rango de Fechas',
                                           start = as.Date('2020-05-27') , end = as.Date('2020-05-31')
                            ),
                            width = 15
                        ),
                        
                        # PSAO / 31-05-2020 / Se colocan dos gráficas por columna
                        fluidRow(
                            column(6,
                                   # PSAO / 19-05-2020 / se cambia por grafica de plotly
                                   plotlyOutput("Gsintomas_reportados", height = "400px")
                            ),
                            
                            column(6,
                                   # PSAO / 02-06-2020 / Se agrega grafica de usuarios registrados
                                   plotlyOutput("Gregistro_sexo", height = "400px")
                            )
                        ),
                        
                        # Cartograma
                        h1("Regiones"),
                        plotlyOutput("p", height = "400px"),
                        
                        h3("Casos por Municipio"),
                        box(
                            DT::dataTableOutput("cpm"),
                            width = 15
                        )
                        
                        
                    )
            )
            # tabItem("cases",
            #         fluidPage(
            #             h1("Casos")
            #         )
            # ),
            # tabItem("afected",
            #         fluidPage(
            #             h1("Afectados")
            #         )
            # ),
            # tabItem("regions",
            #         fluidPage(
            #             
            #         )
            # ),
            # tabItem("symptoms",
            #         fluidPage(
            #             h1("Sintomas Comunes")
            #         )
            # )
        )
    )
)

server <- function(input, output){
    
    output$cpm = DT::renderDataTable({
        data
    })


}

shinyApp(ui, server)