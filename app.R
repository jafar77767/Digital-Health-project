
library(shiny)
library(tidyverse)

raw_data <- read.csv("CVD_cleaned.csv")
raw_data$Age_Category <- as.factor(raw_data$Age_Category)
raw_data$Depression <- as.factor(raw_data$Depression)


ui <- fluidPage(
  
  titlePanel("Correlation between depression and heart diseases, skin cancer, diabeties and arthitis"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        inputId = "select_disease",
        label = "Select disease",
        choices = c(
          "Heart_Disease",
          "Skin_Cancer",
          "Diabetes",
          "Arthritis"
        ),
        selected = "heart diseases",
        multiple = FALSE
      )
    ),
    
    mainPanel(
      plotOutput("depression_epicurve")
    )
    
  )
)





plot_epicurve <- function(data, disease_type) {
  
  column_of_disease_type = which(colnames(data) == disease_type)
  data_copy <- data
  data <- data %>%
    select(Depression, Age_Category, {{column_of_disease_type}}) %>% 
    filter(!!as.symbol(disease_type) == "Yes") 
  glimpse(data)
  data <- data %>% group_by(Age_Category, Depression) %>% summarize(N = n())
  glimpse(data)
  total_by_depression <- data %>% group_by(Age_Category) %>% summarize(Total = sum(N))
  glimpse(total_by_depression)
  data <- inner_join(data, total_by_depression, by = "Age_Category") %>%
    mutate(Fraction = N/Total)
  glimpse(data)
  data <- data %>% filter(Depression == "Yes") 
  data <- data %>% select(Age_Category, Fraction)
  glimpse(data)
  
  data_copy <- data_copy %>%
    select(Depression, Age_Category)
  glimpse(data_copy)
  data_copy <- data_copy %>% group_by(Age_Category, Depression) %>% summarize(N = n())
  glimpse(data_copy)
  total_by_depression_copy <- data_copy %>% group_by(Age_Category) %>% summarize(Total = sum(N))
  glimpse(total_by_depression_copy)
  data_copy <- inner_join(data_copy, total_by_depression_copy, by = "Age_Category") %>%
    mutate(Fraction = N/Total)
  glimpse(data_copy)
  data_copy <- data_copy %>% filter(Depression == "Yes") 
  data_copy <- data_copy %>% select(Age_Category, Fraction)
  glimpse(data_copy)
  glimpse(data)
  d1 <- data
  d2 <- data_copy
  d1$z <- "Correllation between Depression and Deasise"
  d2$z <- "Depression in the absence of any disease"
  d3 <- rbind(d1, d2)
  glimpse(d3)
  mycolors <- c("Correllation between Depression and Deasise"="blue", "Depression in the absence of any disease"="red")
  ggplot(d3, aes(x=Age_Category, y=Fraction, group=z, color=z)) +
    geom_path() +
    geom_point() +
    scale_y_continuous(name="Correllation between Depression and Deasise", sec.axis = sec_axis(~ 2*., name="Depression in the absence of any disease")) +
    scale_color_manual(name="z", values = mycolors) +
    theme(
      axis.title.y = element_text(color = mycolors["Correllation between Depression and Deasise"]),
      axis.text.y = element_text(color = mycolors["Correllation between Depression and Deasise"]),
      axis.title.y.right = element_text(color = mycolors["Depression in the absence of any disease"]),
      axis.text.y.right = element_text(color = mycolors["Depression in the absence of any disease"])
    )
  
}








# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$depression_epicurve <- renderPlot(
    plot_epicurve(raw_data, disease_type = input$select_disease)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
