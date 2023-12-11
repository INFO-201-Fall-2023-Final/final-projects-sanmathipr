library(shiny)
library(ggplot2)
library(plotly)
library(stringr)

df <- read.csv("df.csv")

ui_page1 <- fluidPage(
  mainPanel(
    # Center-aligned main content for Page 1
    div(class = "container text-center",
        h2("Introduction"),
        h3("Exploring the Connection Between Storm Data and Mental Health"),
        h4("This data set combines a storm data and mental health data set to depict the relationship between weather patterns and how people are emotionally affected by them. The connection between mental health and surrounding climate has long been explored by psychologists all around the globe. Although everyone may not be at risk for the development of mental health issues in relation to climate change, it is under debate that “events such as extreme storms or extreme heat can lead to depression, anger and even violence” (APHA). These feelings can result in “self-harm, including substance abuse and suicidal ideation,” an unfortunate consequence. Natural disasters are the direct cause of much pain and loss–people might lose their loved ones, their homes, and many struggle to come back from such situations. Especially at the current rate of global warming, these potential mental health consequences may rise to much higher numbers. The current warming trend “is clearly the result of human activities since the mid-1800s, and is proceeding at a rate not seen over many recent millennia” (NASA)."),
        img(src = "https://media-cldnry.s-nbcnews.com/image/upload/newscms/2018_31/2514836/180730-greece-fire-ac-1047p.jpg", width = 400, height = 300),
        h4("The gases trapped in the atmosphere are resulting in “rapid changes in the atmosphere, ocean, cryosphere, and biosphere,” which we are directly seeing over the years (NASA). The United Nations states that there are “only 11 years left to prevent irreversible damage from climate change”, and if that is the case, mental health problems might only become worse as well. As climate change has gotten worse, mental health seems to be struggling as well. Although awareness about mental health problems has increased, that awareness does not seem to be changing the trajectory of illnesses. “Suicide rates have risen by about 30% since 2000” with “almost a third of U.S. adults now [reporting] symptoms of either depression or anxiety, roughly three times as many as in 2019, and about one in 25 adults has a serious mental illness like bipolar disorder or schizophrenia” (Ducharme). It seems apparent that awareness about mental health is not bringing about change, and although many are going to therapy, there may be external circumstances that are causing this severe increase in mental health issues."),
        img(src = "https://cdn.memiah.co.uk/uploads/counselling-directory.org.uk/image_gallery/weather-1693905944-hero.jpg", width = 400, height = 300),
        h4("From a third party perspective, it can be easy to push these problems aside, especially if one is not dealing with mental health issues themselves. But one must also be empathetic to the fact that “mental health is a basic human right [that is] crucial to personal, community, and socio-economic development,” meaning it directly affects the world when people are suffering with these issues (WHO). If mental health is associated with climate change, and if these extreme disasters are possibly the direct cause of the increase in mental issues, it is important to find ways to mitigate this problem. Knowing that mental health and climate change are associated with one another can help with finding potential solutions for this problem, and helping those who might be affected by natural disasters. This project is based on finding out if there is a relationship that clearly exists between extreme climate change and mental health, in hopes that there can be solutions and prevention tactics created in the case that a connection does lie between these two different sets of data."),
        h4("To learn more about the connection between mental health and weather, follow this link: "),
        a("Link to more info", href = "https://enlightenedsolutions.com/5-ways-the-weather-can-affect-your-mental-health/#:~:text=One%20of%20the%20biggest%20ways,an%20actual%20episode%20of%20depression."),
        )
  )
)
server_page1 <- function(input, output, session) {
}



ui_page2 <- fluidPage(
  titlePanel("Interactive Page 1"),
  sidebarLayout(
    sidebarPanel(
      # Add sidebar content for Page 2
    ),
    mainPanel(
      # Add main content for Page 2
      h2("Welcome to Page 2"),
      # Add additional UI components
    )
  )
)

# Define server logic for the second page
server_page2 <- function(input, output, session) {
  # Add server logic for Page 2
}

ui_page3 <- fluidPage(
  titlePanel("Mental Health Across States"),
  br(),
  p("There is a variation of mental health treatment across states and some people are able to receive treatment
    while others are not. This is data that is important to understand in order to find connections between weather
    patterns and those who need help across different states. The variation in who is able to receive treatment and 
    who is not is also an important distinction, and the variance between states is indicative of how mental health
    is viewed across the United States."),
  br(),
  p("Of all of the states that were examined, 1683 were patients that were unable to 
    receieve treatment despite needing it. This page will let you explore treatement by state further."),
  br(),
  sidebarLayout(
    sidebarPanel (
      selectInput(
        inputId = "state_name",
        label = "Choose a state",
        choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
    )
  ),
  mainPanel(
    plotOutput(outputId = "bar")
  )
)
)

ui_page4 <- fluidPage(
  titlePanel("Interactive Page 3"),
  sidebarLayout(
    sidebarPanel(
      # Add sidebar content for Page 4
    ),
    mainPanel(
      # Add main content for Page 4
      h2("Welcome to Page 4"),
      # Add additional UI components
    )
  )
)

# Define server logic for the second page
server_page2 <- function(input, output, session) {
  # Add server logic for Page 2
}

server_page3 <- function(input, output, session) {
  filtered_df <- reactive({df[df$State == input$state_name, ]})
  output$bar <- renderPlot({
    req(nrow(filtered_df()) > 0, "filtered_df is empty")
    
    prescription <- sum(str_detect(filtered_df()$Indicator, regex("prescription", ignore_case = TRUE)))
    untreated <- sum(str_detect(filtered_df()$Indicator, regex("did not get", ignore_case = TRUE)))
    either_received <- sum(str_detect(filtered_df()$Indicator, regex("Took Prescription Medication for Mental Health And/Or Received Counseling or Therapy, Last 4 Weeks", ignore_case = TRUE)))
    therapy <- sum(str_detect(filtered_df()$Indicator, regex("Received Counseling or Therapy, Last 4 Weeks", ignore_case = TRUE)))
    unknown <- sum(!str_detect(filtered_df()$Indicator, regex("prescription|did not get|Took Prescription Medication for Mental Health And/Or Received Counseling or Therapy, Last 4 Weeks|Received Counseling or Therapy, Last 4 Weeks", ignore_case = TRUE)))
    
    chart_data <- data.frame(
      category = c("Prescription", "Untreated", "Either Received", "Therapy", "Unknown"),
      count = c(prescription, untreated, either_received, therapy, unknown)
    )
    
    p <- ggplot(chart_data, aes(x = category, y = count, fill = category)) +
      geom_bar(stat = "identity") +
      labs(title = "Counts of Each Category",
           x = "Category",
           y = "Count") +
      theme_minimal()
    return(p)
  })
}

# Combine UI and server functions for the entire application
ui <- navbarPage(
  "Shiny App",
  tabPanel("Introduction", ui_page1),
  tabPanel("Interactive Page 1", ui_page2),
  tabPanel("Interactive Page 2", ui_page3),
  tabPanel("Interactive Page 3", ui_page4)
)

server <- function(input, output, session) {
  observe({
    if (!is.null(input$state_name) && input$state_name != "") {
      server_page3(input, output, session)
    }
  })
}

# Run the Shiny App
shinyApp(ui, server)
