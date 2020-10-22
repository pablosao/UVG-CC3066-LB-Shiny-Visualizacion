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
library(lubridate)



data <- read.csv("owid-covid-data.csv") 
# Seleccionando columnas
data <- data %>% select(geoId,countriesAndTerritories,dateRep,cases,deaths)
data %>%
    mutate(dateRep = as.Date(dateRep, format= "%d/%m/%Y"))
#View(data)


ui <- dashboardPage(
    title = "Reportes COVID-19",
    dashboardHeader(title= "Reportes COVID-19 UVG"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("General", tabName = "general", icon = icon("caret-right")),
            menuItem("Casos", tabName = "casos", icon = icon("chart-pie"))
            
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
            tabItem("casos",
                    fluidPage(
                        
                        #h1("General"),
                        #hr(),
                        # PSAO / 31-05-2020 / Se agrega filtro de rango de fechas
                        box(
                            dateRangeInput('RangoFechas',
                                           label = 'Rango de Fechas',
                                           start = as.Date('2020-10-13') , 
                                           end = as.Date('2020-10-14'),
                                           format = "dd/mm/yyyy",
                                           language = "es",
                                           separator = " a "
                            ),
                            width = 15
                        ),
                        
                        h3("Tabla de Casos"),
                        box(
                            DT::dataTableOutput("cpm"),
                            width = 15
                        ),
                        
                        h3("Cantidad de Casos Positivos a Nivel Mundial"),
                        box(
                            plotlyOutput("Gr_casos_nmundial", height = "400px"),
                            width = 15
                        ),

                        h3("Cantidad de Muertes a Nivel Mundial"),
                        box(
                            plotlyOutput("Gr_muertes_mundial", height = "400px"),
                            width = 15
                        ),
                        
                        h3("Casos en Guatemala"),
                        box(
                            plotlyOutput("casos_guate", height = "400px"),
                            width = 15
                        ),

                        h3("Muertes en Guatemala"),
                        box(
                            plotlyOutput("muertes_guate", height = "400px"),
                            width = 15
                        ),
						
						            h3("Casos en Afganistan"),
                        box(
                            plotlyOutput("casos_afganistan", height = "400px"),
                            width = 15
                        ),
						
						            h3("Muertes en Afganistan"),
                        box(
                            plotlyOutput("muertes_afganistan", height = "400px"),
                            width = 15
                        ),
						
						
                        
                        fluidRow(
                            #column(6,
                                   
                            #),
                            
                            column(6,
                                   # PSAO / 02-06-2020 / Se agrega grafica de usuarios registrados
                                   plotlyOutput("Gregistro_sexo", height = "400px")
                            )
                        ),

                        
                    )
            ),
            tabItem("general",
                    fluidPage(
                      # Cartograma  
                        h1("Mapa"),
                        
                        plotlyOutput("map", height = "400px")
                    )
            )
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
        data %>% filter(geoId == 'GT' )
        
    })
    
    # Se asigna libreria de plotly para graficas
    output$Gr_casos_nmundial <- renderPlotly({
        
        # Se obtienen datos a graficar 
        cantDeCasos <- aggregate(data[,3:4], list(data$countriesAndTerritories), mean)
        
        Gr_casos_nmundial <- plot_ly(
            # dataset = cantDeSintomas
            # x = sintomas
            # y = cantidad reporta
            cantDeCasos, x = ~Group.1, y = ~cases,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            ) 
        )
        
        # Seteamos el layout de la grafica
        Gr_casos_nmundial <- Gr_casos_nmundial %>% layout(title = "",
                                                                xaxis = list(title = "Países"),
                                                                yaxis = list(title = "Cantidad de Personas Reportadas"))
    })

    output$Gr_muertes_mundial <- renderPlotly({

        #obtener datos a graficar
        cantDeMuertes <- aggregate(data[,3:5], list(data$countriesAndTerritories), mean)

        Gr_muertes_mundial <- plot_ly(
            cantDeMuertes, x = ~Group.1, y = ~deaths,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            )
        )

        # Seteamos el layout de la grafica
        Gr_muertes_mundial <- Gr_muertes_mundial %>% layout(title = "",
                                                                xaxis = list(title = "Países"),
                                                                yaxis = list(title = "Cantidad de muertes"))
    })


    output$muertes_guate <- renderPlotly({

        #obtener datos a graficar
        data_guate <- data %>% filter(geoId == 'GT' )

        muertes_guate <- plot_ly(
            data_guate, x = ~dateRep, y = ~deaths,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            )
        )

        # Seteamos el layout de la grafica
        muertes_guate <- muertes_guate %>% layout(title = "",
                                                                xaxis = list(title = "Fechas"),
                                                                yaxis = list(title = "Cantidad de muertes"))
    })

    output$casos_guate <- renderPlotly({

        #obtener datos a graficar
        data_guate <- data %>% filter(geoId == 'GT' )

        casos_guate <- plot_ly(
            data_guate, x = ~dateRep, y = ~cases,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            )
        )

        # Seteamos el layout de la grafica
        casos_guate <- casos_guate %>% layout(title = "",
                                                                xaxis = list(title = "Fechas"),
                                                                yaxis = list(title = "Cantidad de casos"))
    })
	
	output$muertes_afganistan <- renderPlotly({

        #obtener datos a graficar
        data_afganistan <- data %>% filter(geoId == 'AF' )

        muertes_afganistan <- plot_ly(
            data_afganistan, x = ~dateRep, y = ~deaths,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            )
        )

        # Seteamos el layout de la grafica
        muertes_afganistan <- muertes_afganistan %>% layout(title = "",
                                                                xaxis = list(title = "Fechas"),
                                                                yaxis = list(title = "Cantidad de muertes"))
    })
	
	output$casos_afganistan <- renderPlotly({

        #obtener datos a graficar
        data_afganistan <- data %>% filter(geoId == 'AF' )

        casos_afganistan <- plot_ly(
            data_afganistan, x = ~dateRep, y = ~deaths,type = "bar",
            marker = list(
                color = 'rgb(0,128,0)'
            )
        )

        # Seteamos el layout de la grafica
        casos_afganistan <- casos_afganistan %>% layout(title = "",
                                                                xaxis = list(title = "Fechas"),
                                                                yaxis = list(title = "Cantidad de muertes"))
    })
	
	  # Mapa
	
	output$map <- renderPlotly({
	  
	  
	  
	  # geo styling
	  g <- list(
	    scope = 'world',
	    projection = list(type = 'kavrayskiy-vii'),
	    showland = TRUE,
	    landcolor = toRGB("gray85"),
	    subunitcolor = 'rgb(0,128,0)',
	    countrycolor = 'rgb(0,128,0)',
	    countrywidth = 0.5,
	    subunitwidth = 0.5
	  )
	  
	  # data2 = variable donde estan todos los datos y la longitud y latitud
      data2 <- read.csv("country-capitals.csv") 
      data2 <- data2 %>% select(CountryName, CapitalName, CapitalLatitude, CapitalLongitude)
	  # lat = se asigna la columna donde esta la latitud
      lat <- data2 %>% select(CapitalLatitude)
	  # lon = se asigna la columna donde esta la longitud
	  lon <- data2 %>% select(CapitalLongitude)
	  
	  fig <- plot_geo(data2, lat = ~lat, lon = ~lon)
	  
	  
	  fig <- fig %>% add_markers(
	    
	    #
	    # pais = se debe cambiar el nombre de la columna donde se encuentra el nombre del pais
	    # casos = se debe cambiar el nombre de la columna donde se encuentra el total de casos
	    #
	    text = ~paste(paste("Pais:", pais),paste("Casos:", casos), sep = "<br />"), hoverinfo = "casos"
	  )
	  
	  fig <- fig %>% colorbar(title = "Casos")
	  fig <- fig %>% layout(
	    title = 'Confirmados', geo = g
	  )
	  
	  p <- ggplotly(fig, tooltip="text")
	})
	
    

}

shinyApp(ui, server)