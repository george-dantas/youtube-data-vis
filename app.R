library(shiny)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(plotly)
library(tidytext)
library(magrittr)
library(wordcloud2)

US = read_csv("data/USvideos.csv")
CA = read_csv("data/CAvideos.csv")
DE = read_csv("data/DEvideos.csv")
FR = read_csv("data/FRvideos.csv")
GB = read_csv("data/GBvideos.csv")
IN = read_csv("data/INvideos.csv")
JP = read_csv("data/JPvideos.csv")
KR = read_csv("data/KRvideos.csv")
MX = read_csv("data/MXvideos.csv")
RU = read_csv("data/RUvideos.csv")

# Source helper functions -----
source("helpers.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  wellPanel(style = "background: #B22222",
            titlePanel(h1(strong("Youtube Trending Video Data Dashboard"), align='center', style = "color:white"))),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      img(src = "youtube-logo.png", height = 70, width = 200),
                
      # Input: Select the country ----
      selectInput("countries", h3("Select a country:", style = "color:#B22222") , selected = "United States",
                  choices = c("United States", "Canada","France", "Great Britain", "India",
                              "Japan", "South Korea", "Mexico", "Russia", "Germany")),
      
      br(),
      h3("Help:", style = "color:#B22222"),
      
      helpText("You can analysing what factors affect",
               "how popular a YouTube video will be."),
      helpText("Choose a country to change the plots",
               "in the box with choices to select from."),
      
      br(),
      h3("About the data:", style = "color:#B22222"),
      
      helpText("This dataset includes several months (and counting)", 
               "of data on daily trending YouTube videos",
               "over the time period of 2017-11-14 to 2018-06-14.",
               "Data is included for the following countries:",
               "USA, Great Britain, Germany, Canada, France,",
               "Russia, Mexico, South Korea, Japan and India.",
               "The datasets are available on",
               a("Kaggle website", 
                 href = "https://www.kaggle.com/datasnaek/youtube-new")),
      
      br(),
      br(),
      helpText(strong("Developed by: ", style = "color:#B22222"), "George Dantas")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Channel and Topics", 
                           plotlyOutput("plot1"),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot2"), plotOutput("plot3"))
                           ),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                                         plotOutput("plot4"), plotOutput("plot5"),
                                         plotOutput("plot6"), plotOutput("plot7"))
                           )
                  ),
                  
                  tabPanel("Trending", plotlyOutput("plot8"), plotlyOutput("plot9")),
                  
                  tabPanel("View Counting", plotOutput("plot10"), plotOutput("plot11"), plotlyOutput("plot15")),
                  
                  tabPanel("Tags",
                           
                           plotlyOutput("plot12"),
                           
                           plotOutput("plot13"),
                  ),
                  
                  tabPanel("Word Cloud", wordcloud2Output("plot14"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    #Bar plot -> Top 20 Channels for hightest # of videos
    top_ch(data) %>%
      plot_ly(
        x = ~n,
        y = ~channel_title,
        type = "bar",
        orientation='h'
      ) %>%
      layout(title = list(text = "Top 20 channels for the numbers of videos", x = 0.1),
        xaxis = list(title = ""),
        yaxis = list(title =""))
  })
  
  output$plot2 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    #Bar plot -> The # of videos per topic
    n_topic(data) %>%
      ggplot(aes(x = reorder(topic, vn_topic), y = vn_topic)) +
      geom_col(fill = 'steelblue') +
      coord_flip() +
      labs(x = NULL, y = NULL, title = 'The Number of Videos per Topic') +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
  })
  
  output$plot3 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    #Bar plot -> The # of channels per topic
    n_topic(data) %>%
      ggplot(aes(x = reorder(topic, vn_topic), y = n)) +
      geom_col(fill = 'coral1') +
      coord_flip() +
      labs(x = NULL, y = NULL, title = 'The Number of Channels per Topic') +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
  })
  
  output$plot4 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    topic_avg(data) %>%
      ggplot(aes(x = reorder(topic, avg_view), y = avg_view)) + 
      geom_col(fill = 'lightseagreen') +
      coord_flip() +
      labs(x = NULL, y = NULL, title = 'Views by topics') +
      theme_bw()
  })
  
  output$plot5 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    topic_avg(data) %>%
      ggplot(aes(x = reorder(topic, avg_comment), y = avg_comment)) + 
      geom_col(fill = 'darkorange1') +
      coord_flip() +
      labs(x = NULL, y = NULL, title = 'Comments by topics') +
      theme_bw()
  })
  
  output$plot6 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    topic_avg(data) %>%
      ggplot(aes(x = reorder(topic, avg_like), y = avg_like)) + 
      geom_col(fill = 'steelblue') +
      coord_flip() +
      labs(x = NULL, y = NULL, title = 'Likes by topics') +
      theme_bw()
  })
  
  output$plot7 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    topic_avg(data) %>%
      ggplot(aes(x = reorder(topic, avg_dislike), y = avg_dislike)) + 
      geom_col(fill = 'indianred1') +
      coord_flip() +
      labs(x = NULL, y = NULL, title = 'Dislikes by topics') +
      theme_bw()
  })
  
  output$plot8 <- renderPlotly({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    # trending period distribution
    us_time(data) %>%
      filter(period < 30) %>%
      plot_ly(x = ~topic, y = ~period, color = ~topic, type = "box") %>%
      layout(title = list(text = 'Days to be Trending', x = 0.1),
        xaxis = list(title = ""),
        yaxis = list(title =""))
  })
  
  output$plot9 <- renderPlotly({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    # trending period distribution
    us_time(data) %>%
      filter(period < 2) %>%
      group_by(topic) %>%
      count() %>%
      plot_ly(x = ~topic, y = ~n, color = ~topic, type = "bar") %>%
      layout(title = list(text='The number of trendy videos per topic', x = 0.1),
        xaxis = list(title = ""),
        yaxis = list(title =""))
  })
  
  output$plot10 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    # view count vs. title length 
    data %>%
      ggplot(aes(x = len_title, y = views)) + 
      geom_point(col = 'lightseagreen') +
      scale_x_continuous(limits = c(0, 200)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, title = 'View Count vs. Title Length') + 
      theme_grey()
  })
  
  output$plot11 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    # view count vs word count of the title  
    data %>%
      ggplot(aes(x = nword_title, y = views)) + 
      geom_point(color = 'darkorange1') +
      scale_x_continuous(limits = c(0, 50)) + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, title = 'View Count vs. Title Word Count') + 
      theme_grey()
  })
  
  output$plot12 <- renderPlotly({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    # the number of tags per topic
    data %>%
      plot_ly(x = ~topic, y = ~n_tags, color = ~topic, type = "box") %>%
      layout(title = list(text='The number of tags per topic', x = 0.1),
        xaxis = list(title = ""),
        yaxis = list(title =""))
  })
  
  output$plot13 <- renderPlot({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    # view counts vs tag numbers 
    ggplot(data, aes(x = n_tags, y = views)) + 
      geom_point(col = 'steelblue') +
      scale_y_continuous(labels = scales::comma) + 
      labs(x = NULL, title = 'View Count vs. Tag Numbers') + 
      theme_bw()
  })
  
  output$plot14 <- renderWordcloud2({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    #aux <- topic_order(n_topic(data))
    # view counts vs tag numbers 
    v = keyword(data=data)
    
    wordcloud2(v, size = 1)
  })
  
  output$plot15 <- renderPlotly({
    
    data <- switch(input$countries, 
                   "United States" = US, "Canada" = CA,"France" = FR, "Great Britain" = GB, "India" = IN,
                   "Japan" = JP, "South Korea" = KR, "Mexico" = MX, "Russia" = RU, "Germany" = DE)
    
    view_date(data) %>%
      plot_ly(y = ~total, x = ~month_year, type = 'scatter', mode = 'lines',
            line = list(width = 4)) %>%
      layout(title = list(text = "View Numbers by Year-Month", x = 0.1))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)