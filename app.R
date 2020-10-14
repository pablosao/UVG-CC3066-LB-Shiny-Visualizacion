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
source("queryManager.R")
library(plotly)
library(leaflet)
library(viridis)
library(rworldmap)
library(maps)
library(ggmap)




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
        casosPorMunicipio <- dbGetQuery(con, getDatos_Cartograma(input$RangoFechas[1],input$RangoFechas[2]))
        
    })
    
    # Se asigna libreria de plotly para graficas
    output$Gsintomas_reportados <- renderPlotly({
        
        # Se obtienen datos a graficar 
        cantDeSintomas <- (input$RangoFechas[1],input$RangoFechas[2])
        
        Gsintomas_reportados <- plot_ly(
            # dataset = cantDeSintomas
            # x = sintomas
            # y = cantidad reporta
            cantDeSintomas, x = ~sintoma, y = ~cantidad_reportada,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            ) 
        )
            
        # Seteamos el layout de la grafica
        Gsintomas_reportados <- Gsintomas_reportados %>% layout(title = "",
                              xaxis = list(title = "Síntomas"),
                              yaxis = list(title = "Cantidad de Personas que Reportaron"))
    })
    
    
    # Se asigna libreria de plotly para graficas
    output$Gregistro_sexo <- renderPlotly({
        
        # Se obtienen datos a graficar 
        cantReg_sexo <- (input$RangoFechas[1],input$RangoFechas[2])
        
        Gregistro_sexo <- plot_ly(
            # dataset = cantReg_sexo
            # x = sexo
            # y = cantidad_solicitudes
            cantReg_sexo, x = ~sexo, y = ~cantidad_solicitudes,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            ) 
        )
        
        # Seteamos el layout de la grafica
        Gregistro_sexo <- Gregistro_sexo %>% layout(title = "",
                                        xaxis = list(title = "Sexo"),
                                        yaxis = list(title = "Cantidad de Personas Registradas"))
    })
    
    
    
    # mapa por medio de coordenadas, lo comento, se puede integrar al final
    """
    output$p <- renderPlotly({
       
        data <- dbGetQuery(con, getDatos_Cartograma(input$RangoFechas[1],input$RangoFechas[2]))
        data$Province.State <- lapply(data$departamento, as.character)
        
        str(data$Province.State)
        
        
        key = "AIzaSyB8hwaB2EEOsucgdc80NCaGp7p7XiSPBvQ" #please use your own API key, I have input the key previously.
        register_google(key=key)
        
        #obtain the coordinates
        country <- data$pais
        cities <- data$departamento
        #use lapply to iterate and obtain the coordinates information.
        loc1 <- do.call(rbind, lapply(cities, function(x) geocode(x)))
        loc1 <- as.data.frame(loc1)
        loc2 <- do.call(rbind, lapply(country, function(x) geocode(x)))
        loc2 <- as.data.frame(loc2)
        
        #create them as data.frame
        as.data.frame(data[,1:2]) -> data2
        data2$lon <- ifelse(data2[,1]== "", loc2[,1], loc1[,1])
        data2$lat <- ifelse(data2[,1]== "", loc2[,2], loc1[,2])
        data2 <- data.frame(data2, sospechosos=data$cantidad_solicitudes,municipios=data$municipio)
        
        
        
        
        # geo styling
        g <- list(
            scope = 'world',
            projection = list(type = 'kavrayskiy-vii'),
            showland = TRUE,
            landcolor = toRGB('gray85'),
            subunitcolor = 'rgb(0,128,0)',
            countrycolor = 'rgb(0,128,0)',
            countrywidth = 0.5,
            subunitwidth = 0.5
        )
        
        fig <- plot_geo(data2, lat = ~lat, lon = ~lon)
        fig <- fig %>% add_markers(
            text = ~paste(paste('Municipios:', municipios),paste('Cantidad Sospechosos:', sospechosos), sep = '<br />'), hoverinfo = 'sospechosos'
        )
        fig <- fig %>% colorbar(title = 'Cantidad de Sospechosos')
        fig <- fig %>% layout(
            title = 'Sospechosos', geo = g
        )
        
        
        
        
        p <- ggplotly(fig, tooltip='text')
    })"""


}

shinyApp(ui, server)