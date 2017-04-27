if (!require("shiny")) {
  install.packages("shiny", repos = "http://cran.us.r-project.org")
}

if (!require("ggplot2")) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}

if (!require("GGally")) {
  install.packages("GGally", repos = "http://cran.us.r-project.org")
}


facebook_data <- read.table("dataset_Facebook.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

facebook_data$percent_like <- facebook_data$like/facebook_data$Total.Interactions
facebook_data$percent_comment <- facebook_data$comment/facebook_data$Total.Interactions
facebook_data$percent_share <- facebook_data$share/facebook_data$Total.Interactions
facebook_data$Type <- factor(facebook_data$Type)

day_name_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
facebook_data$Weekday <- factor(sapply(facebook_data$Post.Weekday, function(x) day_name_list[x]), levels = day_name_list )


ui <- fluidPage(
  headerPanel("Facebook Data Plots"),
  mainPanel(
    tabsetPanel(
      tabPanel("Heat Map", sidebarLayout(
        sidebarPanel(
          selectInput("fb_actions1", label = "Facebook Actions", choices = c("Comment", "Like", "Share"), selected = c("Comment", "Like", "Share"))
        ), plotOutput("heatmap"),
        position = "right"
      )),
      tabPanel("Small Multiples Plot", sidebarLayout(
        sidebarPanel(
          selectInput("fb_actions2", label = "Facebook Actions", choices = c("Comment", "Like", "Share"), selected = c("Comment", "Like", "Share"))
        ), plotOutput("smallmult"),
        position = "right"
      )),
      tabPanel("Parallel Coordinates", sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("weekdays", label = "Weekday", choices = day_name_list, selected = day_name_list, inline = FALSE)
        ), plotOutput("parcoord"),
        position = "right"
      ))
    )
  )
)

server <- function(input, output) {
  hm_fill <- reactive({
    input$fb_actions1
    })
  
  output$heatmap <- renderPlot(
    ggplot(data = facebook_data, aes_string(x = "Type", y = "Post.Weekday")) +
      scale_fill_gradient(paste0("Percent ",hm_fill(), "s"), low = 'floralwhite', high = 'darkgreen') +
      geom_tile(aes_string(fill = tolower(paste0('percent_', tolower(hm_fill()))))) +
      labs(title = paste0("Facebook ", hm_fill(), " Percent of Total Interactions")) +
      scale_y_continuous('Weekday', labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                       breaks = c(1,2,3,4,5,6,7), expand = c(0,0)) +
      scale_x_discrete(expand = c(0,0)) +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = 'floralwhite'),
            axis.title = element_text(size = 14))
  )
  
  
  sm_column <- reactive({
    input$fb_actions2
    })
  
  output$smallmult <- renderPlot(
    ggplot(data=facebook_data, aes_string(x="Type", y= tolower(sm_column()), fill = "Type")) +
      geom_bar(stat="identity") +
      facet_wrap(~Post.Month) +
      labs(title = paste0("Facebook ", sm_column(), "s Per Month")) +
      xlab("Post Type") + ylab(sm_column()) +
      theme(plot.title = element_text(size = 18, hjust = 0.5),
            axis.text = element_text(hjust = 0.5, vjust = 0.5, angle=90),
            axis.title = element_text(size = 14)) 
  )
  
  facebook_data_opaque <- reactive({
    facebook_data %>% 
      mutate(opacity = ifelse(Weekday %in% input$weekdays, 1, 0.1))
  })
  
  
  output$parcoord <- renderPlot(
    ggparcoord(data = facebook_data_opaque(), columns = c(2, 16:18), groupColumn = 23, showPoints = FALSE,
                    alphaLines = "opacity", shadeBox = NULL, scale = "uniminmax") +
      scale_y_continuous(expand = c(0.02, 0.02)) +
      scale_x_discrete(expand = c(0.02, 0.02), labels = c("Type", "Comment", "Like", "Share")) +
      theme_minimal() +
      scale_alpha(guide = 'none') +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#bbbbbb"),
            legend.position = "bottom")
  )
  
}

shinyApp(ui = ui, server = server)