


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

data <- read.csv("toydata.csv")



# Define locations here to make the code easier to read
locationChoices = c("International", "Alabama",  "Alaska", "Arizona", "Arkansas",
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
                                          numericInput("difficulty", "Select difficulty of climb", 4, 1, 10, 1)))),
      
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
                                          br(),
                                          ggvisOutput("explorer_plot")
                                          #DT::dataTableOutput("table3")
                                          ),
                                        sidebarPanel(
                                          # Sidebar heading
                                          h2("Controls"),
                                          # File upload
                                          fileInput("explorer_file", "Upload data"),
                                          # Enter user id
                                          textInput("explorer_userid", "Enter your Mountain Project ID number"),
                                          # Selector for type of climb
                                          selectizeInput("explorer_type", "Type of Climb", choices = c("Aid", "Boulder", "Ice", "Mixed", "Rock"), selected = "Rock", multiple =TRUE),
                                          # Slider for V-level (whatever that is)
                                          sliderInput("explorer_vlevel", "V Level", min = 0, max = 14, value = c(0, 14)),
                                          # Number of pitches
                                          sliderInput("explorer_pitches", "Number of Pitches", min = 0, max = 6, value = c(0, 6)),
                                          # Difficulty
                                          sliderInput("explorer_difficulty", "Select difficulty of climb", min = 0, max = 10, value = c(3,6)),   
                                          # Minimum rating (stars)
                                          numericInput("explorer_rating", "Minimum rating", 5, 1, 2, 0.5),
                                          # Location
                                          selectizeInput("explorer_location", "Select location", selected = c("California", "Colorado"), multiple = TRUE, choices = locationChoices),
                                          # Popularity
                                          selectizeInput("explorer_popularity", "Select popularity of climb", choices = c("Highly Popular", "Moderately Popular", "Not Popular"))
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
  
  content <- paste(sep = "<br/>",
                   "<b><a href='https://www.mountainproject.com/area/105720495/joshua-tree-national-park'>Joshua Tree National Park</a></b>",
                   "State: California",
                   "Elevation: 4200 ft.")
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -115.900650, lat = 33.881866, zoom = 7) %>%

      addProviderTiles(providers$Esri.WorldTopoMap, 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPopups(-115.900650, 33.881866, content, options = popupOptions(closeButton = )) %>% 
      addMarkers(lng = 	-115.900650, lat = 33.881866, popup = "Joshua Tree National Park") %>%
      addMiniMap()
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    Type <- c("Ice", "Boulder", "Rock", "Boulder", "Rock")
    Pitches <- c(2, 1, 4, 1, 5)
    Name <- c("Joshua Tree", "Echo Rock", "Wonderland of Rocks", "Live Oak", "Desert Queen")
    Difficulty <- c(5.5, 7, 9, 3.5, 6)
    RecommendScore <- c(89, 75, 82, 91, 67)
    mydata <- data.frame(Name, Type, Pitches, Difficulty, RecommendScore)
    mydata
  }))
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    Type <- c("Ice", "Boulder", "Rock", "Boulder", "Rock")
    Pitches <- c(2, 1, 4, 1, 5)
    Name <- c("Joshua Tree", "Echo Rock", "Wonderland of Rocks", "Live Oak", "Desert Queen")
    Difficulty <- c(5.5, 7, 9, 3.5, 6)
    RecommendScore <- c(89, 75, 82, 91, 67)
    mydata <- data.frame(Name, Type, Pitches, Difficulty, RecommendScore)
    mydata
  }))
  
  output$table3 <- DT::renderDataTable(DT::datatable({
    Type <- c("Ice", "Boulder", "Rock", "Boulder", "Rock")
    Pitches <- c(2, 1, 4, 1, 5)
    Name <- c("Joshua Tree", "Echo Rock", "Wonderland of Rocks", "Live Oak", "Desert Queen")
    Difficulty <- c(5.5, 7, 9, 3.5, 6)
    RecommendScore <- c(89, 75, 82, 91, 67)
    mydata <- data.frame(Name, Type, Pitches, Difficulty, RecommendScore)
    mydata
  }))

  
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
  
  
  
  
  # EXPLORER ---------------
  
  
  routes <- reactive({
    vlevel_min <- input$explorer_vlevel[1]
    vlevel_max <- input$explorer_vlevel[2]
    rating <- input$explorer_rating
    pitches_min <- input$explorer_pitches[1]
    pitches_max <- input$explorer_pitches[2]
    difficulty_min <- input$explorer_difficulty[1]
    difficulty_max <- input$explorer_difficulty[2]
    location <- input$explorer_location
    popularity <- input$explorer_popularity
    type <- input$explorer_type
    
    d <- data %>%
      filter(data_vlevel >= vlevel_min,
             data_vlevel <= vlevel_max,
             data_type %in% type,
             data_pitches >= pitches_min,
             data_pitches <= pitches_max,
             data_rating >= rating,
             data_difficulty >= difficulty_min,
             data_difficulty <= difficulty_max)
  })
  
  
  # TOOLTIP FOR EXPLORER
  explorer_tooltip <- function(x) {
    if(is.null(x)) return(NULL)
    if(is.null(x$data_name)) return(NULL)
    dat = routes()
    row = dat[dat$data_name %in% x$data_name, ]
    paste0("<b>", "Route Name: ", row$data_name, "</b><br>",
           "Difficulty: ", row$data_difficulty, "<br>",
           "Popularity: ", row$data_popularity, "<br>",
           "Location: ", row$data_location)
  }

  
  vis <- reactive({
    routes %>%
      ggvis(~data_difficulty, ~data_pitches, key := ~data_name) %>%
      layer_points(size := 100, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~data_popularity, fill = ~data_popularity, key := ~data_name) %>%
      add_tooltip(explorer_tooltip, "hover") %>%
      add_axis("x", title = "Difficulty of Route") %>%
      add_axis("y", title = "Number of Pitches") %>%
      set_options(width = 1000, height = 750) %>%
      #add_legend("stroke", title = "Popularity", values = c("Highly Popular", "Moderately Popular", "Not Popular")) %>%
      scale_nominal("stroke", domain = c("Highly Popular", "Moderately Popular", "Not Popular"),
                    range = c("green", "orange", "red")) %>%
      scale_nominal("fill", domain = c("Highly Popular", "Moderately Popular", "Not Popular"),
                    range = c("green", "orange", "red"))
  })
  
  
  vis %>% bind_shiny("explorer_plot")
}



# Run the application 
shinyApp(ui = ui, server = server)








