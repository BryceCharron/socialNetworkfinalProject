# libraries and data
library(shiny); library(igraph); library(ggraph); library(tidyverse); library(ggplot2);
library(tidygraph); library(backbone); library(ggthemes)

ui <- navbarPage(
  "Major League Baseball Network Analysis",
  tags$head(tags$link(href = "https://fonts.googleapis.com/css2?family=Computer+Modern+Serif&display=swap",
      rel = "stylesheet"),
    tags$style(HTML("body {font-family: 'Computer Modern Serif', serif;}
    /* Top navbar background */.navbar 
    {background-color: #001F3F;
        border-color: #001F3F;}

      /* Navbar text */
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white;
      }
      /* Hover color */
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-brand:hover {
        background-color: #001F3F;
        color: #4682b4;
      }"))),
  tabPanel("Home",
           fluidPage(h1("Project Overview"),
                     p("This project explores trade relationships among Major League Baseball teams, 
                     analyzing how trading patterns relate to player performance and how front-office relationships may change over time.
                     The analysis will specifically seek to investigate patterns of trade with regard to player value. 
                     It's possible that teams tend to trade with specific segments of the league
                     or trade prospects and act as talent farms for teams with divergent managerial thinking. 
                    Using the Louvain community detection algorithm, I identify three trading blocs of teams that tend 
                    to trade with one another. An analysis of degree centrality also finds that the number of trading partners
                    generally decreases in 2020, likely due to lowered incentives
                     during the shortened COVID-19 season. It is apparent that 
                     season-to-season, the amount of trading activity a given team engages in is variable.
                     This likely reflects varying strategic priorities by year. 
                     Overall, through the various forms of interactivity, this app
                       may be valuable in informing future team trading strategy."),
           h3("How to Use"),
           p("Directions on how to navigate the application are displayed at the top of each tab. Generally speaking, one can "),
           tags$ul(
             tags$li("Navigate the network analysis by clicking on the tabs above"),
             tags$li("Use sliders on each tab to explore trade dynamics in different seasons"),
             tags$li("Toggle between various statistical measures to gain different insights")))),
  tabPanel("Data",
           fluidPage(h3("Data Description"),
                     p("The following analysis will make use of 1,708 nodes and 5,252 edges
                       spanning the 2010-2024 seasons. ", "Nodes denote teams or players,
                       and edges represent undirected trades between them. ",
                       "The data come from two key sources: the ",
                       tags$em("baseballr"),
                       " library in R and the MLB Statistics API. ",
                       tags$em("baseballr"),
                       " is a package in R that contains various functions which scrape baseball data ",
                       "from websites such as Baseball Reference, FanGraphs, and MLB Baseball Savant. ",
                       "Notable variables include ",
                       tags$code("lagWAR"), ", ",
                       tags$code("lagAVG"), ", and ",
                       tags$code("lagERA"), ". ",
                       "Further information on the data collection and cleaning process 
                       can be found in the ReadMe and Data Collection documentation on GitHub. ",
                       "Below, you may switch between different statistical measures 
                       to explore the data from multiple perspectives."),
                     h3("Node and Edge Attributes"),
                     selectInput("select", 
                                 "Select an Attribute", 
                                 choices = list("Position Type" = "positionGroup", 
                                                "Age" = "Age",
                                                "lagERA" =  "lagERA",
                                                "lagWAR" = "lagWAR",
                                                "lagAVG" = "lagAVG"),
                                 selected = 1),
                     textOutput("attrs_text"),
                     plotOutput("attrs"))),
      tabPanel("Team Analysis",
           fluidPage(h2("Team-Level Analysis"),
                                    p("This section explores team-level trends and comparisons.
                                      Play with the Season slider to view how the degree centrality
                                      (number of trading partners) of a team varies over time.
                                      Click on the Networks tab to view team-by-team projection networks."),
                     tabsetPanel(id = "tabset",
                       tabPanel("Bar Chart",
                         sliderInput("teamSeason",
                           "Select Season",
                           min = 2010,
                           max = 2024,
                           value = 2010,
                           sep = ""),
                         verbatimTextOutput("value"),
                         plotOutput("barplot")),
                       tabPanel("Networks",
                                p("Adjust the size of the nodes by specifying a centrality measure. 
                                Degree centrality captures the total number of trading partners (teams)
                                a specific team has in a given season. Eigenvector centrality measures how active a teams 
                                trading partners are in a given season. 
                                This means that teams involved in large exchanges 
                                become more prominent."),
                                fluidRow(column(6, sliderInput("teamSeason",
                                           "Select Season",
                                           min = 2010,
                                           max = 2024,
                                           value = 2010,
                                           sep = "")),
                                         column(6,radioButtons("size_by",
                                                             "Centrality Measure",
                                                             choices = c("Degree Centrality" = "degree", 
                                                                         "Eigenvector Centrality" = "eigen"),
                                                             selected = "degree"))),
                         plotOutput("network"),
                         p("The following network visualization showcases a rough measure of 
                           'trading blocs' within the league using the Louvain algorithm. Nodes 
                           of the same color therefore represent blocs that tend to trade with one another over
                           the period 2010-2024."),
                         plotOutput("network2"))))),
  tabPanel("Player Analysis",
           fluidPage(h2("Player-Level Analysis"),
                     p("This section focuses on individual player performance metrics. Specifically, one can
                     toggle various season, team, statistic combinations to view how teams make trades based
                     on player value. If a specific combination fails to render a network, no corresponding data exists.
                       I recommend selecting four to five teams of interest for greater data availability. Although edges
                       are undirected in this visualization, general patterns of trade can be identified. 
                       Nodes are colored by player position type, and edge widths are sized by player age."),
                     fluidRow(column(4,
                                     sliderInput("season", "Select Season",
                                          min = 2010, max = 2024,
                                          value = 2010, sep = "")),
                              column(4,
                                     selectizeInput(inputId = "teams",
                                                    label = "Choose up to 5 Teams to Visualize",
                                                    choices = c("SF", "CLE", "MIA",
                                                                "BOS", "KC", "TOR",
                                                                "COL", "ATL", "CHC",
                                                                "PIT", "BAL", "STL",
                                                                "PHI", "SEA", "SD", 
                                                                "MIN", "TEX", "MIL",
                                                                "NYY", "DET", "LAD",
                                                                "NYM", "WSH", "LAA",
                                                                "HOU", "AZ", "CIN",
                                                                "ATH", "CWS", "TB"),
                                selected = c("SF", "CHC", "BOS", "LAD", "BAL"),
                                multiple = T,
                                options = list(maxItems = 5))),
                              column(4,
                                     radioButtons("stat",
                                           "Player Statistic",
                                           choices = c("Lag WAR" = "lagWAR", 
                                                       "Lag ERA" = "lagERA",
                                                       "Lag AVG" = "lagAVG"),
                                           selected = "lagWAR",
                                           inline = T))),
                     plotOutput("network3"))),
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
t_tidy <- as_tbl_graph(t_net) |> 
  activate(nodes) |>
  mutate(btwn = centrality_betweenness(normalized = T),
         degree = centrality_degree(mode = "all"),
         strength = centrality_degree(weights = weight, mode = "all"),
         cluster_walk = group_walktrap(steps = 5),
         cluster_louv = group_louvain(weights = weight))

server <- function(input, output) {
  
  output$attrs_text <- renderText({
    
    if (input$select == "positionGroup") {
      
      "This dataset is heavily weighted toward pitchers (52%) and includes 20% infielders,
    15% outfielders, and 13% utility players. Utility players are defined as
    those who are listed to play both infield and outfield positions."
      
    } else if (input$select == "Age") {
      
      "This dataset appears to reflect a typical professional baseball age distribution.
    The median age is 28 years old, with the first quartile at 26 and the third quartile at 31."
      
    } else if (input$select == "lagERA") {
      
      "The ERA distribution in this dataset appears heavily right-skewed, with a median around 5.
    There are several extreme outliers in the 30–55 range. These values are likely driven by players
    who threw very few innings, where even a small number of earned runs can dramatically inflate ERA."
      
    } else if (input$select == "lagAVG") {
      
      "Batting average in this dataset appears somewhat left-skewed, with a median around .150.
    This is likely influenced by the large number of pitchers in the dataset, many of whom recorded
    only a few at-bats and no hits, resulting in batting averages near zero."
      
    } else {
      
      "The WAR distribution in this dataset appears broadly consistent with typical MLB player performance,
    with an interquartile range between 0 and 1.3 WAR. This suggests that most players in the dataset
    perform slightly below league-average regulars."
      
    }
  })
      
  output$attrs <- renderPlot({
    if (input$select == "positionGroup") {
      
      # drop NAs and then reorder in order of frequency (for cleaner display)
      nodes |>
        drop_na(positionGroup) |>
        mutate(positionGroup = fct_infreq(positionGroup)) |>
        ggplot(aes(x = factor(1), fill = positionGroup)) +
        geom_bar(position = "fill") +
        scale_fill_manual(values = c("Infield"  = "#B23A48",  
          "Outfield" = "#0B1F3A",  
          "Utility"  = "#C9A227", 
          "Pitcher"  = "#86A9C6"))+
        scale_y_continuous(labels = scales::percent) +
        labs(x = NULL, fill = "Position Type", y  = "Percent") +
        theme_tufte() +
        theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
      
    } else {
      p("This dataset is heavily weighted toward pitchers (52%) and includes 20% infielders,
      15% outfielders, and 13% utility players. Utility players are defined as 
        those who are listed to play both infield and outfield positions.")
      
      edges |>
        drop_na(.data[[input$select]]) |>
        ggplot(aes(x = factor(1), y = .data[[input$select]])) +
        geom_boxplot() +
        labs(x = NULL, y = input$select) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+ 
        theme_tufte() +
        theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
      
    }
    
  })
  
  # include an interactive bar chart that shows team degree centrality (by year)
  output$barplot <- renderPlot({
  
    # plot team trading network
    net <- edges |>
      filter(Season == input$teamSeason) |>
      graph_from_data_frame(vertices = nodes, directed = F) |>
      as_tbl_graph()
    
    # projection
    bp <- bipartite_projection(net)
    
    # focus on teams
    t_net <- bp$proj2
    t_tidy_react <- as_tbl_graph(t_net) |>
      activate(nodes) |>
      mutate(btwn = centrality_betweenness(normalized = T),
             degree = centrality_degree()) |> 
      as_tibble()
  
  p1 <- ggplot(t_tidy_react, aes(x = degree, y = reorder(Name, degree))) +
    geom_col(aes(fill = League)) +
    scale_fill_manual(values = c("AL" = "#B23A48", "NL" = "#0B1F3A"))+
    labs(title = paste("Degree Centrality: Number of trading partners in", input$teamSeason),
      x = "Number of Teams",
      y = "Team") +
    theme_tufte()

    p1
  })
  
  output$network <- renderPlot({
    # plot team trading network
    net <- edges |>
      filter(Season == input$teamSeason) |>
      graph_from_data_frame(vertices = nodes, directed = F) |>
      as_tbl_graph()
    
    # projection
    bp <- bipartite_projection(net)
    
    # focus on teams
    t_net <- bp$proj2
    t_tidy_react <- as_tbl_graph(t_net) |>
      activate(nodes) |>
      mutate(btwn = centrality_betweenness(normalized = T),
             degree = centrality_degree(),
             strength = centrality_degree(weights = weight),
             cluster_walk = group_walktrap(steps = 5),
             cluster_louv = group_louvain(weights = weight),
             eigen = centrality_eigen(weights = weight)) 
    
    p2 <- ggraph(t_tidy_react, layout = "fr") +         
      geom_edge_link(aes(width = weight), 
                     alpha = 0.45,  
                     color = "#B0B0B0",
                     show.legend = T) +
      geom_node_point(aes(size = .data[[input$size_by]], color = League)) +
      scale_color_manual(values = c("AL" = "#B23A48", "NL" = "#0B1F3A"))+
      geom_node_label(aes(label = Name),    
                      repel = T,
                      label.size = 0.2,
                      fill = "white") +
      scale_edge_width(name = "Players exchanged") +
      scale_size(range = c(2, 10),
                 name = "Degree") +
      labs(title = "Which teams exchange players most?",
           subtitle = "Edge width = number of exchanged players | Node size = centrality measure") +
      theme_tufte() +
      theme(legend.position = "right")
    
    p2
  })
  
  # let's use a cluster algorithm to identify communities or blocs of trading partners
  output$network2 <- renderPlot({
    
    p3 <- ggraph(t_tidy, layout = "fr") +         
      geom_edge_link(aes(width = weight),
                     alpha = 0.45,  
                     color = "#B0B0B0",
                     show.legend = T) +
      geom_node_point(aes(size = strength, color = factor(cluster_louv))) +
      scale_color_manual(values = c("1" = "#4F5D75", 
                                    "2" = "#EF8354",
                                    "3" = "#2D936C")) +
      geom_node_label(aes(label = Name),    
                      repel = T,
                      label.size = 0.2,
                      fill = "white") +
      scale_edge_width(range = c(0.3, 4),   
                       name = "Players exchanged") +
      scale_size(range = c(2, 10),
                 name = "Degree") +
      labs(title = "Which teams exchange players most?",
           subtitle = "Edge width = number of exchanged players | Node size = number of total players traded",
        color = "Cluster") +
      theme_tufte() +
      theme(legend.position = "right")
    
    p3
  })
    
    output$network3 <- renderPlot({
      
    # filter to selected teams, grab players who have played for at least one of them
    spec_teams <- edges |> 
      filter(Team %in% input$teams, Season == input$season) |> 
      group_by(Player, Season) |>
      summarise(teams = n_distinct(Team)) |>
      filter(teams > 1)
    
    # make vector of specific names
    spec_names <- spec_teams$Player
    
    # make network
    p4 <- as_tbl_graph(net) |>
      activate(edges) |> 
      filter(Player %in% spec_names,
             Team %in% input$teams,
             Season == input$season) |> 
      activate(nodes) |> 
      mutate(degree = centrality_degree()) |> 
      filter(degree > 0) |> 
      ggraph(layout = "bipartite") + 
      geom_edge_link(aes(color = .data[[input$stat]], width = Age)) + 
      geom_node_point(aes(color = positionGroup), size = 3.75) + 
      scale_color_manual(name = "Position Type",
        values = c("Infield"  = "#B23A48",  
          "Outfield" = "#0B1F3A",  
          "Utility" = "#C9A227", 
          "Pitcher" = "#86A9C6",
          "Team" = "#2D6A4F")) +
      scale_edge_color_gradient(low = "#E8F4F8",
                                high = "#08306B",
                                name = paste(input$stat)) +
      geom_node_label(aes(label = Name), size = 3.75, repel = T)+
      theme_tufte()
    
    p4
})
}
# run the app
shinyApp(ui, server)