library(tidyverse)

adm2020 <- read_csv("adm2020.csv")

df <- na.omit(adm2020)

df$admit_rate <- df$ADMSSN / df$APPLCN
df$admit_rate <- df$admit_rate * 100

df$selectivity <- case_when(df$admit_rate <= 25 ~ "highly selective",
                            df$admit_rate <= 75 & df$admit_rate > 25 ~ "selective",
                            df$admit_rate > 75 ~ "not selective")

df$selectivity <- as_factor(df$selectivity)

df <- select(df, admit_rate, selectivity, ACTCM25, ACTCM75, UNITID)


library(shiny)

ui <- fluidPage(
  
  titlePanel("Test Scores and Selectivity"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(
        "selectivity",
        "Selectivity",
        choices = list("Highly Selective" = "highly selective",
                       "Somewhat Selective" = "selective",
                       "Less Selective" = "not selective")
      )
      
    ),
    
    mainPanel(
      
      plotOutput(outputId="show_scores")
      
    )
  )
)



server <- function(input, output) {
  
  output$show_scores <- renderPlot({
    
    x_min <- ifelse(input$selectivity == "highly selective", 0,
                    ifelse(input$selectivity == "selective", 25,
                           ifelse(input$selectivity == "not selective", 75, 0)))
    x_max <- ifelse(input$selectivity == "highly selective", 25,
                    ifelse(input$selectivity == "selective", 75,
                           ifelse(input$selectivity == "not selective", 100, 100)))
    
    ggplot(data=df, aes(x=admit_rate)) +
      geom_point(aes(y=ACTCM75, color="75th Percentile")) +
      geom_point(aes(y=ACTCM25, color="25th Percentile")) +
      xlim(x_min, x_max) +
      labs(x = "Admission Rate (%)",
           y = "ACT Composite Score") +
      guides(color = guide_legend(reverse=T, title=NULL))
    
    
  })
  
}

shinyApp(ui=ui, server=server)
