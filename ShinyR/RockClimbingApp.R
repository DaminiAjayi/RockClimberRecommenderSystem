# table interactivity: https://antoineguillot.wordpress.com/2017/03/01/three-r-shiny-tricks-to-make-your-shiny-app-shines-33-buttons-to-delete-edit-and-compare-datatable-rows/
# More maps can be found here: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
# Leaflet help: https://rstudio.github.io/leaflet/popups.html

library(shiny)
library(dplyr)
library(shinythemes)
library(leaflet)
library(DT)
library(ggplot2)
library(ggpubr)
library(ggvis)
library(stringr)
library(httr)
library(jsonlite)
library(mongolite)
library(RcppCNPy)
library(stringr)
source("GetGeo.r")
source("Recommenders.r")


# Define locations here to make the code easier to read
locationChoices = c("All","Alabama",  "Alaska", "Arizona", "Arkansas",
                    "California", "Colorado","Connecticut","Delaware","Florida",
                    "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                    "Iowa", "Kansas", "Kentucky", "Maine", "Maryland",
                    "Massachusetts", "Michigan", "Minnesota", "Missouri", "Montana",
                    "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
                    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                    "Texas", "Utah", "Vermont", "Virginia", "Washington",
                    "West Virginia", "Wisconsin", "Wyoming")

text_Purpose1 <- ("American rock climbing is enjoying a boom in popularity - ballooning to 4.6 million total climbers in 2016 (CBJ, 2016). 
                  The proliferation of rock climbing in recent years is exciting for both novices and seasoned climbers. 
                  The large influx of newcomers and intermediate climbers has created an information gap. 
                  More specifically, climbers may not have the information on how to advance their climbing skills efficiently and quickly, 
                  which local routes to climb, and what routes other climbers around their skill level are climbing.")

text_Purpose2 <- ("With the growth in climbing comes a vast increase in data collected on the activity. 
                  In 2019, we have reached a critical mass of climbers which creates great potential for big data systems and visual analytics to 
                  answer these questions and support climbing as a form of sport and recreation. 
                  This project seeks to build a visual analytics system to help climbers build 
                  their skills with achievable, challenging recommendations.")


## test code if you want to read any data from localhost
#mp_tick <- mongo(collection= "test3",
#                 db = "MP",
#                 url = "mongodb://localhost:27017",
#                verbose = TRUE)
#ticks <- mp_tick$find('{}')


 # get route
ROUTE <- mongo(collection= "route",
                db = "MP",
                url = "mongodb://user1:cse6242@cluster0-shard-00-00-38kfo.mongodb.net:27017/?ssl=true",
                verbose = TRUE)
 
# routes <- ROUTE$find('{}')
# routes$rating
# 
# # get tick
TICK<- mongo(collection= "tick",
             db = "MP",
             url = "mongodb://user1:cse6242@cluster0-shard-00-00-38kfo.mongodb.net:27017/?ssl=true",
             verbose = TRUE)
# 
# ticks <- TICK$find('{}')
# 
# # get user
USER<- mongo(collection= "user",
             db = "MP",
             url = "mongodb://user1:cse6242@cluster0-shard-00-00-38kfo.mongodb.net:27017/?ssl=true",
             verbose = TRUE)

# users<- USER$find('{}')


######################################################################


# Define UI for application that draws a histogram
ui <- navbarPage("RC Recommender",
                 tabPanel("Map",
                          sidebarLayout(position = "right",
                                        mainPanel(
                                          h2("Recommender Map"),
                                          p(),
                                          leafletOutput("mymap", height = 450),
                                          br(),
                                          br(),
                                          DT::dataTableOutput("table")
                                          ),
                                        
                                        
                                        sidebarPanel(
                                          # Sidebar heading
                                          h2("Controls"),
                                          ## File upload
                                          #fileInput("file", "Upload data"),
                                          # Enter user id
                                          textInput("userid", "User ID:", placeholder = 'Enter your user ID'),
                                          textInput("cityname", "City, State:", placeholder = 'e.g. Houston, TX'),
                                          textInput("searchradius", "Routes search radius (mi)", placeholder = 'e.g. 20'),
                                          # sliderInput("searchradius", "Routes search radius (mi)", min = 0, max = 100, value = 50, step=10),
                                          actionButton("search", "Search"),
                                          
                                          h2("Filter"),
                                          # Minimum rating (stars)
                                          sliderInput("rating", "Minimum Stars", min = 0, max = 5, value = 0,step=0.5),
                                          # Selector for type of climb
                                          selectizeInput("type", "Type of Climb", choices = c("All","Aid","Alpine", "Boulder","Ice","Mixed", "Rock","Snow" , "Sport","TR","Trad"),
                                                         options = list(
                                                           placeholder = 'Please select climb type below',
                                                           onInitialize = I('function() { this.setValue(""); }'))
                                                         ),
                                          sliderInput("pitches", "No. of Pitches", min = 0, max = 10, value = 0,step=1),
                                          # # Slider for V-level (whatever that is)
                                          # sliderInput("vlevel", "V Level", min = 0, max = 14, value = c(3, 5)),
                                          actionButton("filter", "Filter")))),
                              
                
                 
                 tabPanel("Social Network",
                          h4("http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html")),


                          
                          
                 tabPanel("Custom Training Guide",
                          sidebarLayout(position = "right",
                                        mainPanel(
                                          h2("Customized Skill Progression"),
                                          br(),
                                          plotOutput("scatter",
                                                     hover = "scatter_hover"),
                                          br(),
                                          br(),
                                          DT::dataTableOutput("table3")),
                                        sidebarPanel(
                                          # Sidebar heading
                                          h2("Controls"),
                                          # File upload
                                          fileInput("file", "Upload data"),
                                          # Enter user id
                                          textInput("userid", "Enter your Mountain Project ID number"),
                                          # Selector for type of climb
                                          selectizeInput("type", "Type of Climb", choices = c("Aid", "Boulder", "Ice", "Mixed", "Rock")),
                                          # Slider for V-level (whatever that is)
                                          sliderInput("vlevel", "V Level", min = 0, max = 14, value = c(3, 5)),
                                          # Minimum rating (stars)
                                          numericInput("rating", "Minimum rating", 5, 1, 2, 0.5),
                                          # Number of pitches
                                          radioButtons("pitches", "Number of pitches", choices = c("Exactly one", "At least 2",
                                                                                                   "At least 3", "At least 4",
                                                                                                   "At least 5", "At least 6")),
                                          
                                          # Location
                                          selectizeInput("location", "Select location", multiple = TRUE, choices = locationChoices),
                                          # Popularity
                                          selectizeInput("popularity", "Select popularity of climb", choices = c("Highly Popular", "Moderately Popular", "Not Popular")),
                                          # Dfficulty
                                          numericInput("difficulty", "Select difficulty of climb", 4, 1, 10, 1)                                          
                                          
                                          
                                          
                                          
                                          
                                        ))),
   
                 
                 
                 tabPanel("Route Explorer",
                          sidebarLayout(position = "right",
                                        mainPanel(
                                          h2("Route Explorer"),
                                          br(),
                                          p("Use the controls on the left to find new routes"),
                                          br(),
                                          p("https://shiny.rstudio.com/gallery/movie-explorer.html"),
                                          #plotOutput("scatter", hover = "scatter_hover"),
                                          br(),
                                          br()
                                          #DT::dataTableOutput("table3")
                                          ),
                                        sidebarPanel(
                                          # Sidebar heading
                                          h2("Controls"),
                                          # File upload
                                          fileInput("file", "Upload data"),
                                          # Enter user id
                                          textInput("userid", "Enter your Mountain Project ID number"),
                                          # Selector for type of climb
                                          selectizeInput("type", "Type of Climb", choices = c("Aid", "Boulder", "Ice", "Mixed", "Rock")),
                                          # Slider for V-level (whatever that is)
                                          sliderInput("vlevel", "V Level", min = 0, max = 14, value = c(3, 5)),
                                          # Minimum rating (stars)
                                          numericInput("rating", "Minimum rating", 5, 1, 2, 0.5),
                                          # Number of pitches
                                          radioButtons("pitches", "Number of pitches", choices = c("Exactly one", "At least 2",
                                                                                                   "At least 3", "At least 4",
                                                                                                   "At least 5", "At least 6")),
                                          
                                          # Location
                                          selectizeInput("location", "Select location", multiple = FALSE, choices = locationChoices),
                                          # Popularity
                                          selectizeInput("popularity", "Select popularity of climb", choices = c("Highly Popular", "Moderately Popular", "Not Popular")),
                                          # Dfficulty
                                          numericInput("difficulty", "Select difficulty of climb", 4, 1, 10, 1)                                          
 
                                          
                                          
                                        ))),
                 
                 
                 
                 navbarMenu("About",
                            tabPanel("Overview",
                                     h2("Purpose"),
                                     text_Purpose1, br(), br(),
                                     text_Purpose2, br(), br(),
                                     h2("How it works"), br(), br(),
                                     h3("Collaborative Filtering"), br(), br(),
                                     h3("Singular Value Decomposition"), br(), br()
                                     ),
                            tabPanel("Authors",
                                     h3("CSE 6242 Team"),
                                     strong("Damini Ajayi"),
                                     br("Email: daminiajayi@gmail.com"), br(),
                                     strong("Ben Croft"),
                                     br("Email: benjaminhcroft@gmail.com"), br(),
                                     strong("Joshua Demeo"),
                                     br("Email: jdemeo91@gmail.com"), br(),
                                     strong("Jeff Sayre"),
                                     br("Email: jpsayre@gmail.com"), br(),
                                     strong("Jiandao Zhu"),
                                     br("Email: jiandaoz@gmail.com"), br()),
                            tabPanel("Works Cited")),
                 
                 theme = shinytheme("flatly"),
                 shinythemes::themeSelector()
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactiveValues(dataframe = NULL,df_sub=NULL)
  
  observeEvent(input$search, {
    cityname <- isolate(input$cityname)
    print(cityname)
    userid <- isolate(input$userid)
    searchradius <- isolate(input$searchradius)
    # numofrecom <- isolate(input$numofrecom)
    
    long_lat<- getgeo(cityname)
    print(long_lat)
    print("Getting tick data for user")
    # userid <- 200220441
    ticks <- TICK$find(paste0('{"user":','"', userid, '"}'))
    if(length(ticks$ticks[[1]]$routeId) >= 100){
      print("Running SVD code")
      # svd_setup()
      routeid <- getroutesid(userid, long_lat[[1]], long_lat[[2]], searchradius)
    } else {
      print("running KNN code")
      routeid <- getKNN(userid, long_lat[[1]], long_lat[[2]], searchradius)
    }
    
    print("Writing output to dataframe")  
    output$mymap <- renderLeaflet({
  
    dataframe <- NULL
    for (ir in 1:length(routeid)){
      route <- ROUTE$find(paste0('{"id" : ', routeid[ir], '}'))
      content<- paste(
        "<b><a href=",
        route$url,
        ">",
        route$name,
        "</a></b>",
        "<br/>Type:",
        route$type,
        "<br/>Stars:",
        route$stars)
      name <- route$name
      state <- route$location[[1]][1]
      longitude <- route$longitude
      latitude <- route$latitude
      type <- route$type
      stars <- route$stars
      votes <- route$starVotes
      pitches <- route$pitches
      if (pitches ==""){pitches <- 0}
      dataframe=rbind(dataframe,data.frame(content,longitude,latitude,name, state, type,pitches, stars,votes))
    }

    data$dataframe <- dataframe

    print(dataframe$longitude)
    print(dataframe$latitude)
    print(dataframe$content)
    leaflet(data = dataframe) %>%
    setView(lng = -98, lat = 39, zoom = 4) %>%
      
    addProviderTiles(providers$Esri.WorldTopoMap, 
                       options = providerTileOptions(noWrap = TRUE)) %>%  
    addMarkers(lng = dataframe$longitude, lat = dataframe$latitude, popup = dataframe$content) %>%
    addMiniMap() 
    })
    
  output$table <- DT::renderDataTable({
      DT::datatable(data$dataframe[,c("name","state","type","pitches","stars","votes")])
    })
  })
  
  
  observeEvent(input$filter, {
    output$mymap <- renderLeaflet({
      selectrating <- isolate(input$rating)
      selectpitches <- isolate(input$pitches)
      # selectlocation <- isolate(input$location)
      selecttype <- isolate(input$type)
      dataframe <- data$dataframe
      df_sub <- subset(dataframe,stars>=selectrating)
      # df_sub <- subset(dataframe,pitches>=selectpitches)
      # if(selectlocation !="" & selectlocation !="All"){
      #   df_sub <- subset(df,state==selectlocation)
      # }
      if(selecttype!="" & selecttype !="All"){
        df_sub <- df_sub %>% filter(str_detect(type, selecttype))
      }     
      data$df_sub <- df_sub
      leaflet(data = df_sub) %>%
        setView(lng = -98, lat = 39, zoom =4) %>%
        
        addProviderTiles(providers$Esri.WorldTopoMap, 
                         options = providerTileOptions(noWrap = TRUE)) %>%  
        
        addMarkers(lng = 	df_sub$longitude, lat = df_sub$latitude, popup = df_sub$content) %>%
        addMiniMap()  
      })
    output$table <- DT::renderDataTable({
      DT::datatable(data$df_sub[,c("name","state","type","pitches","stars","votes")])
    })
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -98, lat = 39, zoom = 4) %>%

      addProviderTiles(providers$Esri.WorldTopoMap, 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMiniMap()
  })
  
  

  
  
  
  output$scatter_info <- renderText
  
  
  output$scatter <- renderPlot({
    
    x <- c(2, 4, 6, 7, 7, 8, 11)
    y <- c(2, 4, 3, 8, 9, 10, 9)
    z <- c("g2", "g2", "g1", "g1", "g1", "g1", "g2")
    d <- data.frame(x, y, z)
    
    
    ggplot(d, aes(x = x, y = y, label = z, color = z)) +
      geom_vline(xintercept = median(d$x), linetype = "dashed", alpha = 0.5) +
      geom_hline(yintercept = median(d$y), linetype = "dashed", alpha = 0.5) +
      geom_point(size = 12, alpha = 1) +
      geom_text(color = "white", size = 5) +  
      theme_pubr() +
      labs(x = "Order to climb",
           y = "Difficulty",
           #title = "Course Item Minutes by Accesses",
           #subtitle = "Average Activity for Gradable Content",
           #caption = "Source: Blackboard Analytics for Learn",
           color = "Whatever the user selects as color") + 
      theme(plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            title = element_text(color = "#000059",  face = "bold", size = 16),
            plot.subtitle = element_text(color = "#000059",  face = "plain", size = 16),
            axis.title = element_text(color = "#000059",  face = "plain", size = 16),
            axis.text = element_text(color = "#000059",  face = "plain", size = 16),
            axis.line = element_line(color = "#000059", size = 0.5, linetype = "solid"),
            plot.caption = element_text(color = "#000059",  face = "plain", size = 16),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.title = element_text( face = "plain")) +
      scale_color_manual(values = c("#1a2a6c", "#FF533D", "#E89005", "#D25630", "#b21f1f"))
    
  })

}


# Run the application 
shinyApp(ui = ui, server = server)





