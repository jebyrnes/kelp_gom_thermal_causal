#'---------------------------------------------
#' Shiny App for Notes from the Past Show How Local 
#' Variability can Stymie Urchins and the Rise of the Reds in the Gulf of Maine
#' 
#' @author Jarrett Byrnes
#' See about section of app for more info
#'---------------------------------------------

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(glue)
library(wesanderson)
colors_for_pal <- wes_palette("Zissou1", 6, type = "continuous")



if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")


# The Data
geodata <- readRDS("site_data.rds")

geodata <- geodata %>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))


#colors_for_pal <- RColorBrewer::brewer.pal(length(levels(geodata$region)), name = "BrBG")

pal <- colorFactor(palette = as.character(colors_for_pal), 
                         ordered = TRUE,
                         levels = levels(geodata$region))

# A Basic Map and some extra info (like colors!)
basemap <- leaflet() %>% 
    # addTiles() %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>%
    fitBounds(-70.66749, 43.00184, -66.95433 , 44.81128) 



# Define UI for application 
ui <- fluidPage(title  = "Maps of Sites Sampled",
                theme = shinytheme("journal"),
                
                fluidRow(column(
                    12,
                    leafletOutput("mymap", height = "1000"),
                    
                    absolutePanel(
                        id = "controls",
                        class = "panel panel-default",
                        top = 20,
                        left = 65,
                        width = 200,
                        fixed = TRUE,
                        draggable = TRUE,
                        height = "auto",
                        sliderTextInput("plot_date",
                                        label = h5("Select year"),
                                        choices = sort(unique(geodata$year)),
                                        selected = sort(unique(geodata$year))[1],
                                        grid = FALSE,
                                        animate=animationOptions(interval = 3000, loop = TRUE)))
 
                )))
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #filter the data
    geodata_dat <- reactive({
        if(is.null(input$plot_date)) input$plot_date <- min(geodata$year)
        
        geodata %>%
            filter(year == input$plot_date)
        
    })
    
    #make the map
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    
    #the update
    observeEvent({input$plot_date}, {
            leafletProxy("mymap") %>%
                clearMarkers() %>%
                addCircleMarkers(data = geodata_dat(),
                             fillColor = ~ pal(region),
                             fillOpacity = 0.4,
                             stroke = FALSE,
                             popup = ~ paste(year, region, sep = "<br>"))
        },
        ignoreInit = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

    
