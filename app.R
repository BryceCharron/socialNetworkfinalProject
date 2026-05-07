# libraries and data
library(shiny); library(igraph); library(ggraph); library(tidyverse); library(ggplot2);
library(tidygraph); library(backbone); library(ggthemes)

ui <- navbarPage(
  "Major League Baseball Network Analysis",
  tags$head(tags$link(href = "https://fonts.googleapis.com/css2?family=Computer+Modern+Serif&display=swap",
      rel = "stylesheet"),
    tags$style(HTML("body {font-family: 'Computer Modern Serif', serif;}"))),
  tabPanel("Home",
           fluidPage(h1("Project Overview"),
                     p("This project explores trade relationships among Major League Baseball teams, 
                     analyzing how trading patterns relate to team performance and how front-office relationships may change over time.
                     Network analysis will seek to investigate patterns of trade with regard to player value. It's possible that teams tend to trade with specific segments of the league
                     or trade prospects and act as talent farms for teams with divergent managerial thinking. 
                     Thus, this analysis may be valuable in informing team trading strategy."),
                     
                     h3("Data Description"),
                     p("The following analysis will make use of two key sources: the ",
                       tags$em("baseballr"),
                       " library in R and the MLB Statistics API. ",
                       
                       tags$em("baseballr"),
                       " is a package in R that contains various functions which scrape baseball data ",
                       "from websites such as Baseball Reference, FanGraphs, and MLB Baseball Savant. ",
                       
                       "The final dataset includes player and team statistics from 2010–2024. ",
                       
                       "Notable variables include ",
                       tags$code("lagWAR"), ", ",
                       tags$code("lagAVG"), ", ",
                       tags$code("lagERA"),
                       ", and ",
                       tags$code("lagTeamwinPct"), ".", 
                       "Further information on the data collection and cleaning process 
                       can be found in the ReadMe and Data Collection documentation on GitHub."
                     ),
           
           h3("How to Use"),
           tags$ul(
             tags$li("Navigate the network analysis by clicking on the tabs above"),
             tags$li("Use filters in each tab to explore specific teams or players"),
             tags$li("Hover over plots for additional detail.")))),
  
  tabPanel("Team Analysis",
           fluidPage(h2("Team-Level Analysis"),
                                    p("This section explores team-level trends and comparisons."),
                      
                      sliderInput("teamSeason", "Select Season", 
                                   min = 2010, max = 2024,
                                  value = 2010, sep = ""), 
                       verbatimTextOutput("value"),
                     plotOutput("barplot")),
           plotOutput("network")),
  tabPanel("Player Analysis",
           fluidPage(h2("Player-Level Analysis"),
                     p("This section focuses on individual player performance and metrics."),
                     sliderInput("season", "Select Season", 
                                 min = 2010, max = 2024,
                                 value = 2010, sep = ""),
                     plotOutput("position_plot"))),
  hr(),
  p("Created by Bryce Charron | SOC 0226A", style = "font-size: 12px; color: gray;"))

# read in data
mlb <- read_csv('mlb_clean.csv')
edges <- read_csv('mlb_edges.csv')
nodes <- read_csv('mlb_nodes.csv')

# create network object using igraph
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = F)

# convert to bipartite projection
bp <- bipartite_projection(net)

# create player and team projections
p_net <- bp$proj1
t_net <- bp$proj2

# let's make tidygraph objects
p_tidy <- as_tbl_graph(p_net)
t_tidy <- as_tbl_graph(t_net)

trade_counts <- edges |>
  group_by(Team, Season) |>
  summarize(count = n(), .groups = "drop") |> 
  left_join(nodes |> select(Name, League), by  = c("Team" = "Name"))

pos_df <- edges |>
  left_join(nodes, by = c("To" = "ID")) |> 
  group_by(Position, positionGroup, Season) |>
  summarize(count = n(), .groups = "drop") 

server <- function(input, output) {
  
  # include an interactive bar chart that shows top ten most active trading teams (by year)
  output$barplot <- renderPlot({
  top_teams <- trade_counts |>
    filter(Season == input$teamSeason) |>
    slice_max(count, n = 10) |>
    arrange(count)
  
  p1 <- ggplot(top_teams, aes(x = count, y  = reorder(Team, count))) +
    geom_col(aes(fill = League)) +
    scale_fill_manual(values = c("AL" = "#C8102E", "NL" = "#002D72"))+
    labs(title = paste("Top 10 Trading Teams in", input$teamSeason),
      x = "Team",
      y = "Number of Trades") +
    theme_tufte()

    p1
  })
  
  output$network <- renderPlot({
    # plot team trading network
    net <- edges |>
      filter(Season == input$teamSeason) |>
      graph_from_data_frame(vertices = nodes, directed = F) |>
      as_tbl_graph()
    
    bp <- bipartite_projection(net)
    
    t_net <- bp$proj2
    
    t_tidy <- as_tbl_graph(t_net) |>
      activate(nodes) |>
      mutate(btwn = centrality_betweenness(normalized = T),
             degree = centrality_degree(mode = "all"),
             strength = centrality_degree(weights = weight, mode = "all"),
             cluster_walk = group_walktrap(steps = 3),
             cluster_louv = group_louvain(weights = weight))
    
    p2 <- ggraph(t_tidy, layout = "fr") +         
      geom_edge_link(aes(width = weight, alpha = weight),  
                     color = "gray20",
                     show.legend = T) +
      geom_node_point(aes(size = strength, color = League)) +
      scale_color_manual(values = c("AL" = "#C8102E", "NL" = "#002D72"))+
      geom_node_label(aes(label = Name),    
                      repel = T,
                      label.size = 0.2,
                      fill = "white") +
      scale_edge_width(range = c(0.3, 4),    
                       name = "Players exchanged") +
      scale_edge_alpha(range = c(0.1, 0.7),  
                       guide = "none") +
      scale_size(range = c(2, 10),
                 name = "Degree") +
      labs(title = "Which teams exchange players most?",
           subtitle = "Edge width = number of exchanged players | Node size = number of total players traded"
      ) +
      theme_tufte() +
      theme(legend.position = "right")
    
    p2
  })
    # include an interactive bar chart that shows top ten most traded players by position
    output$position_plot <- renderPlot({
      
    top_pos <- pos_df |>
      filter(Season == input$season) |>
      slice_max(count, n = 10) |>
      arrange(count)
      
      p2 <- ggplot(top_pos, aes(x = count,
                               y = reorder(Position, count),
                               fill = positionGroup)) +
        geom_col() +
        labs(
          title = paste("Trades by Position in", input$season),
          x = "Number of Trades",
          y = "Position"
        ) +
        theme_minimal()
      
     p2
    })

}
# run the app
shinyApp(ui, server)