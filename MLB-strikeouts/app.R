#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

B_2017 <- read_csv("Batting_2017.csv")

so_x_ba <- B_2017 %>%
  filter(yearID %in% 2000:2017) %>%
  group_by(yearID) %>%
  summarise(total_SO = sum(SO), mean_BA = ((sum(H)) / (sum(AB)))) %>%
  arrange(desc(yearID))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MLB Strikeouts Have Increased Over the Past 13 Seasons"),
   
   # Sidebar with two inputs
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "y",
                     label = "Y-axis:",
                     choices = c("Total Strikeouts" = so_x_ba$total_SO, 
                                 "Mean Batting Avg" = so_x_ba$mean_BA),
                     selected = so_x_ba$mean_BA)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "scatterplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     ggplot(data = so_x_ba, aes(x = yearID, y = input$y)) + geom_point()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

