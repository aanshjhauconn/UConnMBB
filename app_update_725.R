library(shiny)
library(DT)
library(png)
library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Basketball Shot Tracker"),
  sidebarLayout(
    sidebarPanel(
      plotOutput("court_plot", click = "court_click", width = "400px", height = "400px"),
      radioButtons("player_name", "Player:",
                   choices = c("Ahmad Nowell", "Solomon Ball", "Jaylin Stewart","Isaiah Abraham",
                               "Tarris Reed Jr","Hassan Diarra","Alex Karaban","Aidan Mahaney",
                               "Jayden Ross","Youssouf Singare","Liam McNeely","Samson Johnson"),inline=T),
      textInput("drill","Drill:"),
      radioButtons("shot_result", "Shot Result:",
                   choices = c("Made", "Missed"), selected = NULL,inline=TRUE),
      radioButtons("assisted","Assisted:",choices=c('Yes','No'),selected=NULL,inline=T),
      radioButtons("shot_type","Shot Type:",choices=c('Off Dribble','Catch and Shoot','Floater/Moving','Layup/Dunk','Post Move'),selected=NULL,inline=T),
      radioButtons("shot_distance","Shot Distance:",choices=c('Layup/Dunk','Paint','Long 2','3'),selected=NULL,inline=T),
      radioButtons("paint_touches","Paint Touches:",choices=c('0','1','2','3+'),selected=NULL,inline=T),
      radioButtons("poss_type","Possession Type",choices=c('Halfcourt','Transition'),selected=NULL,inline=T),
      actionButton("record_btn", "Record Shot"),
      actionButton("reset_btn", "Reset Shots"),
      downloadButton("download_btn", "Download to CSV")
    ),
    
    mainPanel(
      h4("Shot Data"),
      DTOutput("shot_table") #change output number
    )
  )
)

server <- function(input, output, session) {
  # Initialize empty shot data
  shot_data <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), result = character(), assisted=character(), player = character(),
                                                drill=character(),shot_type=character(), shot_distance=character(), paint_touches=character(),poss_type=character()))
  
  # Function to update the shot plot
  updateShotPlot <- function() {
    output$court_plot <- renderPlot({
      # NCAA basketball half court dimensions
      Picture<-readPNG('/Users/aansh/OneDrive/Desktop/UConn MBB/basketball_court.png')
      plot(1:50,ty="n",xlab='',ylab='',pty='s')
      rasterImage(Picture,0,0,50,47)
      
      # Display shots
      if (!is.null(shot_data$data)) {
        made_shots <- shot_data$data$result == "Made"
        missed_shots <- shot_data$data$result == "Missed"
        points(shot_data$data$x[made_shots], shot_data$data$y[made_shots], pch = 16, cex=1, col = "green")
        points(shot_data$data$x[missed_shots], shot_data$data$y[missed_shots], pch = 16, cex=1, col = "red")
      }
      
      # Add dot for the most recent shot
      if (!is.null(input$court_click)) {
        x <- round(input$court_click$x)
        y <- round(input$court_click$y)
        result <- input$shot_result
        color <- ifelse(result == "Made", "green", "red")
        points(x, y, pch = 16, col = color, cex = 1)
      }
    })
  }
  
  # Record a shot
  observeEvent(input$court_click, {
    x <- round(input$court_click$x)
    y <- round(input$court_click$y)
    assisted <- input$assisted
    player <- input$player_name
    drill <- input$drill
    shot_type <- input$shot_type
    shot_distance <- input$shot_distance
    paint_touches <- input$paint_touches
    poss_type <- input$poss_type
    
    # Update the shot data
    new_shot <- data.frame(x = x, y = y, result = NA, assisted=assisted,player = player, drill = drill,
                           shot_type=shot_type,shot_distance=shot_distance,paint_touches=paint_touches,poss_type=poss_type)
    shot_data$data <- rbind(shot_data$data, new_shot)
    
    # Update the shot plot
    updateShotPlot()
  })
  
  # Record the shot result
  observeEvent(input$record_btn, {
    result <- input$shot_result
    
    # Get the index of the last shot recorded
    last_shot_index <- nrow(shot_data$data)
    
    # Update the result of the last shot
    shot_data$data$result[last_shot_index] <- result
    
    # Update the shot table
    output$shot_table <- renderDT({
      shot_data$data
    })
  })
  
  # Reset shots
  observeEvent(input$reset_btn, {
    # Clear shot data
    shot_data$data <- data.frame(x = numeric(), y = numeric(), result = character(), assisted=character(), player = character(), 
                                 drill = character(),shot_type=character(), shot_distance = character(),paint_touches=character(),poss_type=character())
    
    # Update the shot plot
    updateShotPlot()
    
    # Update the shot table
    output$shot_table <- renderDT({
      shot_data$data
    })
  })
  
  # Download shot data as CSV
  output$download_btn <- downloadHandler(
    filename = "shot_data.csv",
    content = function(file) {
      write.csv(shot_data$data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)