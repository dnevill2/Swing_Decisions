
library(baseballr)
library(tidyverse)
library(DBI)
library(RSQLite)
library(xgboost)
library(caret)
library(Matrix)
library(skimr)
library(rsconnect)
library(scales)
library(plotly)
library(ggiraph)

shinySwing <- readRDS("ShinyData/shinySwing.rds")
shinyStrike <- readRDS("ShinyData/shinyStrike.rds")
shinyWhiff <- readRDS("ShinyData/shinyWhiff.rds")
atbat <- readRDS(file = "ShinyData/shinyAtBat.rds") %>%
  mutate(game_date = as.Date(game_date))

# Create dataframe of Swings Above Average by Player and Zone
SAA_playerzone <- shinySwing %>%
  mutate(zone = factor(zone, levels = c(1,2,3,4,5,6,7,8,9,11,12,13,14))) %>%
  group_by(player_name, zone) %>%
  summarize(pitches = n(), SAA = mean(swingAA), SAA100 = mean(swingAA100)) %>%
  ungroup()
# Make sure each player has entries for all zones
all_combinations <- expand_grid(
  player_name = unique(shinySwing$player_name),
  zone = factor(c(1,2,3,4,5,6,7,8,9,11,12,13,14), levels = c(1,2,3,4,5,6,7,8,9,11,12,13,14))
)
SAA_playerzone2 <- all_combinations %>%
  left_join(SAA_playerzone, by = c("player_name","zone")) %>%
  replace_na(list(pitches = 0, SAA = 0, SAA100=0)) %>%
  arrange(player_name)
# Specify polygon coordinates for ggplot
positions <- data.frame(
  zone = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14),
  x = c(-0.71, -0.2366666667, -0.2366666667, -0.71, -0.2366666667, 0.2366666667, 0.2366666667, -0.2366666667, 0.2366666667, 0.71, 0.71, 0.2366666667, -0.71, -0.2366666667, -0.2366666667, -0.71, -0.2366666667, 0.2366666667, 0.2366666667, -0.2366666667, 0.2366666667, 0.71, 0.71, 0.2366666667, -0.71, -0.2366666667, -0.2366666667, -0.71, -0.2366666667, 0.2366666667, 0.2366666667, -0.2366666667, 0.2366666667, 0.71, 0.71, 0.2366666667, -1.06, 0, 0, -0.71, -0.71, -1.06, 0, 1.06, 1.06, 0.71, 0.71, 0, -1.06, -0.71, -0.71, 0, 0, -1.06, 0.71, 1.06, 1.06, 0, 0, 0.71),
  y = c(3.5, 3.5, 2.833333333, 2.833333333, 3.5, 3.5, 2.833333333, 2.833333333, 3.5, 3.5, 2.833333333, 2.833333333, 2.833333333, 2.833333333, 2.166666667, 2.166666667, 2.833333333, 2.833333333, 2.166666667, 2.166666667, 2.833333333, 2.833333333, 2.166666667, 2.166666667, 2.166666667, 2.166666667, 1.5, 1.5, 2.166666667, 2.166666667, 1.5, 1.5, 2.166666667, 2.166666667, 1.5, 1.5, 3.85, 3.85, 3.5, 3.5, 2.5, 2.5, 3.85, 3.85, 2.5, 2.5, 3.5, 3.5, 2.5, 2.5, 1.5, 1.5, 1.15, 1.15, 2.5, 2.5, 1.15, 1.15, 1.5, 1.5)
)

# Create dataframe of Strikes Above Average by Player and Count
strikeAA_wide2 <- shinyStrike %>%
  group_by(player_name, count) %>%
  summarize(strikesAA100 = round(mean(strikesAA) * 100,2)) %>%
  pivot_wider(names_from = count, values_from = strikesAA100)
strikeAA_wide <- shinyStrike %>%
  group_by(player_name) %>%
  summarize(taken_pitches = n()) %>%
  left_join(strikeAA_wide2, by = c("player_name"))

# Create dataframe of Swings Above Average by Player and Zone
whiffAA_playerzone <- shinyWhiff %>%
  mutate(zone = factor(zone, levels = c(1,2,3,4,5,6,7,8,9,11,12,13,14))) %>%
  group_by(player_name, zone) %>%
  summarize(pitches = n(), whiffAA = round(mean(whiffAA, na.rm = TRUE),2), whiffAA100 = round(mean(whiffAA100, na.rm = TRUE),2)) %>%
  ungroup()

whiffAA_playerzone2 <- all_combinations %>%
  left_join(whiffAA_playerzone, by = c("player_name","zone")) %>%
  replace_na(list(pitches = 0, whiffAA = 0, whiffAA100=0)) %>%
  arrange(player_name)
 
# Shiny app
ui <- fluidPage(
  titlePanel("Swing Decisions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("batter", "Select Batter:", choices = unique(SAA_playerzone2$player_name))
    ),
    mainPanel(
      tabsetPanel(
        # At-Bat Summary tab
        tabPanel("At-Bat Summary", 
                 dateInput("game_date", "Select a Date", value = NULL),
                 selectInput("at_bat", "select an At-Bat", choices = NULL),
                 girafeOutput('plot')
        ),
        tabPanel("Swings Above Average", plotOutput("saaPlot")),
        tabPanel("Strikes Above Average",
                 fluidRow(
                 column(12, h3("Strikes Above Average per 100 Taken Pitches")),
                 column(12, tableOutput("strikeAATable"))
                 )),
        tabPanel("Whiffs Above Average",
                 plotOutput("whiffaaPlot"))
      ),
      fluidRow(
        column(6, tags$p("Source: MLB Statcast", style = "text-align: left; font-size: 12px; color: black;")),
        column(6, tags$p("By: dnevBaseball", style = "text-align: right; font-size: 12px; color: black;"))
      )
    )
  )
)

server <- function(input, output, session) {
# Swings Above Average Plot  
  output$saaPlot <- renderPlot({
  # Set batter variable to user selection  
    batter <- input$batter
  
  # Swings Above Average graph  
    values <- SAA_playerzone2 %>%
      filter(player_name == batter) %>%
      select(zone, SAA100)
    
    datapoly <- merge(values, positions, by = c("zone"))
    text_positions <- datapoly %>% group_by(zone) %>% summarise(x = mean(x), y = mean(y), SAA100 = mean(SAA100))
    text_positions[10, "y"] <- 3.7
    text_positions[11, "y"] <- 3.7
    text_positions[12, "y"] <- 1.35
    text_positions[13, "y"] <- 1.35
    
    ggplot(datapoly, aes(x = x, y = y)) +
      geom_polygon(aes(fill = SAA100, group = zone)) +
      geom_text(data = text_positions,
                aes(label = round(SAA100, 1)), color = "black") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-16, 16), oob = scales::squish) +
      coord_fixed(ratio = 1) +
      labs(title = paste("Swings Above Average per 100 Pitches by Zone for", batter), x = "", y = "") +
      theme_minimal()
  })
  
# Strikes Above Average Table
  output$strikeAATable <- renderTable({
    batter <- input$batter
    
    strikeAA_filtered <- strikeAA_wide %>%
      filter(player_name == batter)
    
    strikeAA_filtered
  })

# Whiffs Above Average Plot  
  output$whiffaaPlot <- renderPlot({
    # Set batter variable to user selection  
    batter <- input$batter
    
    values <- whiffAA_playerzone2 %>%
      filter(player_name == batter) %>%
      select(zone, whiffAA100)
    
    datapoly <- merge(values, positions, by = c("zone"))
    text_positions <- datapoly %>% group_by(zone) %>% summarise(x = mean(x), y = mean(y), whiffAA100 = mean(whiffAA100))
    text_positions[10, "y"] <- 3.7
    text_positions[11, "y"] <- 3.7
    text_positions[12, "y"] <- 1.35
    text_positions[13, "y"] <- 1.35
    
    ggplot(datapoly, aes(x = x, y = y)) +
      geom_polygon(aes(fill = whiffAA100, group = zone)) +
      geom_text(data = text_positions,
                aes(label = round(whiffAA100, 1)), color = "black") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-16, 16), oob = scales::squish) +
      coord_fixed(ratio = 1) +
      labs(title = paste("Whiffs Above Average per 100 Pitches by Zone for", batter), x = "", y = "") +
      theme_minimal()
  })
  
# AtBat Summary Data
  
  # Reactive expression for filtering data based on the selected batter
  batter_data <- reactive({
    atbat %>%
      filter(player_name == input$batter)
  })  
  
  # Observe the batter selection and update the game_date input
  observeEvent(input$batter, {
    dates <- batter_data()$game_date
    # Update the date input based on available dates for the selected batter
    updateDateInput(session, "game_date",
                    min = min(dates, na.rm = TRUE),
                    max = max(dates, na.rm = TRUE),
                    value = max(dates, na.rm = TRUE))
  })
  
  # Observe the batter and game_date selection and update the at_bat input
  observeEvent(c(input$batter,input$game_date), {
    # Filter the at-bats for the selected batter and game_date
    filtered_atBat <- batter_data() %>% filter(game_date == input$game_date)
    # Update the at-bat input based on available at-bats for the selected batter and game date
    updateSelectInput(session, "at_bat", choices = sort(unique(filtered_atBat$at_bat_number)))
  })
  
  # Reactive of filtered data based on selected inputs
  selected_AB <- reactive({atbat %>%
      filter(player_name == input$batter, game_date == input$game_date, at_bat_number == input$at_bat)
  })

  output$plot <- renderGirafe({
    gf <- ggplot(data = selected_AB(), aes(x = plate_x, y = plate_z, color = SDr, shape = swing)) +
      geom_point_interactive(aes(
        tooltip = paste(
          "Count:", Count, "\n",
          "SDr:", SDr, "\n",
          "Event:", Event, "\n",
          "Pitch Value (Take):", Pitch_Value_Take, "\n",
          "Pitch Value (Swing):", Pitch_Value_Swing, "\n",
          "Swing Prob:", percent(swingProb), "\n",
          "Strike Prob:", percent(strikeProb), "\n",
          "Foul Prob:", percent(foulProb), "\n",
          "Whiff Prob:", percent(whiffProb), "\n",
          "In Play Prob:", percent(inplayProb), "\n",
          "xR (In Play):", xwOBAProb)
      ), size = 3) + 
      geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = szLow, ymax = szHigh), 
                color = "black", linetype = "dashed", fill = NA) +
      scale_color_gradientn(colors = c("red", "burlywood", "green"), 
                            limits = c(-0.08, 0.08), name = "SDr",
                            oob = scales::squish) +
      scale_shape_discrete(name = "Swing Decision") +
      labs(x = "Catcher's Perspective", y = "") +
      coord_cartesian(xlim = c(-3, 3), ylim = c(0, 6)) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white"))
    
    girafe_plot <- girafe(ggobj = gf)
    
    girafe_plot
  })
    
#  output$plot <- renderPlotly({
#   p <- plot_ly(
#      data = selected_AB(),
#      x = ~plate_x,
#      y = ~plate_z,
#      symbol = ~swing,  # Define symbols based on swing
#      text = ~paste(
#        "Count:", Count, "<br>",
#        "SDr:", SDr, "<br>",
#        "Event:", Event, "<br>",
#        "Pitch Value (Take):", Pitch_Value_Take, "<br>",
#        "Pitch Value (Swing):", Pitch_Value_Swing, "<br>",
#        "Swing Prob:", label_percent()(swingProb), "<br>",
#        "Strike Prob:", label_percent()(strikeProb), "<br>",
#        "Foul Prob:", label_percent()(foulProb), "<br>",
#        "Whiff Prob:", label_percent()(whiffProb), "<br>",
#        "In Play Prob:", label_percent()(inplayProb), "<br>",
#        "xR (In Play):", xwOBAProb
#      ),  # Concatenate multiple columns into hover text
#      hoverinfo = 'text',  # Set hover info to display the text
#      type = 'scatter',
#      mode = 'markers',
#      marker = list(size = 10, 
#                    color = ~SDr,
#                    colorscale = "Viridis",
#                    colorbar = list(title = "SDr", len = 0.5),
#                    cauto = F,
#                    cmin = -0.1,
#                    cmax = 0.1), 
#      width = 600,
#      height = 600
#    ) %>%
#      layout(
#        xaxis = list(title = "Catcher's Perspective", range = c(-3, 3)),
#        yaxis = list(title = "", range = c(0, 6)),
#        shapes = list(
#          list(
#            type = "rect",
#            x0 = -0.708, x1 = 0.708, y0 = 1.5, y1 = 3.5,
#            line = list(color = "black", dash = "dash")
#          )
#        ),
#        showlegend = TRUE,
#        plot_bgcolor = 'white'
#      ) 
   # Add dummy traces to ensure Swing and Take are always in the legend
#   p <- p %>%
#     add_trace(
#       x = NA, y = NA,  # Dummy trace for 'Swing'
#       type = 'scatter', mode = 'markers',
#       marker = list(size = 10, color = "red"),  # Customize as needed
#       name = "Swing", showlegend = TRUE,
#       legendgroup = "Swing", inherit = F
#     ) %>%
#     add_trace(
#       x = NA, y = NA,  # Dummy trace for 'Take'
#       type = 'scatter', mode = 'markers',
#       marker = list(size = 10, color = "red"),  # Customize as needed
#       name = "Take", showlegend = TRUE,
#       legendgroup = "Take", inherit = F
#     )
#   p #return the final plotly plot
#  })
  
}

shinyApp(ui, server)