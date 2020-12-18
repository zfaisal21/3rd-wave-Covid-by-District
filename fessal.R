# Install R packages
# install.packages('shiny')
# install.packages('shinythemes')
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages('ggiraph')
# install.packages('ggiraph')
# install.packages('leaflet')


# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(leaflet)

# data <-read.csv("C:Users/Asus/OneDrive/Desktop/COVID19/ActiveCases.csv", header = TRUE, sep = ",")
data <- read.csv("C:/Users/Asus/OneDrive/Desktop/COVID19/3rdwave/ActiveCases.csv")
# fdata <-read.csv("C:Users/Asus/OneDrive/Desktop/COVID19/GovernHospTreatCOVID19.csv", header = TRUE, sep = ",")
fdata <- read.csv("C:/Users/Asus/OneDrive/Desktop/COVID19/3rdwave/GovernHospTreatCOVID19.csv")
ffdata <- read.csv("C:/Users/Asus/OneDrive/Desktop/COVID19/3rdwave/screening.csv")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
              navbarPage("COVID-19",
                           
                           tabPanel("COVID-19 Information",
                                    titlePanel("How danger COVID-19 is ?"),
                                    h3('Covid 19'),
                                    h4('jangan mkn nasi ayam')
                                    
                                    
                           ), # Navbar 1, tabPanel
########################## page 2 - symptom checker part ##########################
                           tabPanel("Symptom Checker", sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", "")
                             
                           ),# sidebarPanel
                           mainPanel(
                             h1("Patient details"),
                             h4("Your Name"),
                             verbatimTextOutput("txtout"))
                           ),
                           
########################## page 3 - Map-Screening ##########################
                           tabPanel("Map - Screening",
                                    leafletOutput("mymap2"),
                                    # p(),
                                    # actionButton("recalc2", "New points")
                           ),
########################## page 4 - Map-Hospital treat COVID-19 ##########################
                          tabPanel("Map - Hospital treat COVID-19",
                            leafletOutput("mymap"),
                            # p(),
                            # actionButton("recalc", "New points")
                            ),

########################## page 5 - Active Cases ##########################  
                           tabPanel("Active Cases - Interstates", 
                                    basicPage(DT::dataTableOutput("climatetableCity"))),
                           
########################## page 6 - Interstates comparison ##########################                    
                           tabPanel("Interstates Comparison", "Azim figure out k",
                                    #####padam            
                                    selectInput("outlook", label = "Outlook:", 
                                                choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                                                selected = "Rainy"),
                                    sliderInput("temperature", "Temperature:",
                                                min = 64, max = 86,
                                                value = 70),
                                    sliderInput("humidity", "Humidity:",
                                                min = 65, max = 96,
                                                value = 90),
                                    selectInput("windy", label = "Windy:", 
                                                choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                                selected = "TRUE"),
                                    
                                    actionButton("submitbutton", "Submit", class = "btn btn-primary"))
                ) # navbarPage
) # fluidPage




# Define server function  
server <- function(input, output) {
  ####### output Symptom checker start #######
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  ####### output Symptom checker end #######
  
  ####### Map-screening start #######
  # points <- eventReactive(input$recalc2, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  template_pop_up2 <-  tags$dl(class = "dl-horizontal",
                              tags$dt("Name: "), tags$dd("%s"),
                              tags$dt("Address:"), tags$dd("%s"),
                              tags$dt("District:"), tags$dd("%s"),
                              tags$dt("State:"), tags$dd("%s"),
                              tags$dt("Tel:"), tags$dd("%s")) %>% paste()
  
  popup_info2 <- sprintf(template_pop_up2,
                        ffdata[["Name"]],
                        ffdata[["Address"]],
                        ffdata[["District"]],
                        ffdata[["State"]],
                        ffdata[["Tel"]])
  
  getColor <- function(df) 
  {
    sapply(df$Type, function(x) {
      if(x == 1) {"lightblue"}
      else{"blue"}
    })}
  
  icons <- awesomeIcons(
    icon = 'fa-blank',
    library = 'fa',
    iconColor = 'white',
    markerColor = getColor(ffdata))
  
  output$mymap2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(ffdata$longitude, ffdata$latitude, icon = icons, popup = popup_info2)
  })  
  ####### Map-screening end ####### 
  
  
  
  ####### Map-Hospital treat COVID-19 start #######
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  template_pop_up <-  tags$dl(class = "dl-horizontal",
                              tags$dt("Name: "), tags$dd("%s"),
                              tags$dt("Address:"), tags$dd("%s"),
                              tags$dt("District:"), tags$dd("%s"),
                              tags$dt("State:"), tags$dd("%s"),
                              tags$dt("Tel:"), tags$dd("%s")) %>% paste()
  
  popup_info <- sprintf(template_pop_up,
                        fdata[["Name"]],
                        fdata[["Address"]],
                        fdata[["District"]],
                        fdata[["State"]],
                        fdata[["Tel"]])
  
  getColor <- function(df) 
    {
    sapply(df$Type, function(x) {
      if(x == 1) {"lightblue"}
      else{"blue"}
      })}
  
  icons <- awesomeIcons(
    icon = 'fa-blank',
    library = 'fa',
    iconColor = 'white',
    markerColor = getColor(fdata))
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(fdata$longitude, fdata$latitude, icon = icons, popup = popup_info)
  })  
  ####### Map-Hospital treat COVID-19 end #######  
  
  ####### Active Cases- Interstates start #######
  output$climatetableCity = DT::renderDataTable({data})
  ####### Active Cases- Interstates end #######
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)

