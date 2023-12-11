library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(rsconnect)

df <- read.csv("df.csv")

ui_page1 <- fluidPage(
  mainPanel(
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
  mainPanel(
    class = "container text-center",
    h2("Connecting Stormy Weather and Mental Health"),
    h4("To understand the connection between storm data and mental health, it made sense to first understand the relationship between 
       location and mental health. This could then be compared to the relationship between location and stormy weather, ultimately detailing what stormy weather and mental health have in common."),
    h4("The interactive plot below details the relationship between location and mental health. It shows how many mental health cases 
       have been reported per state. It is important to take into account that this may also be reliant on how this data was collected and from
       where. You can interact with the plot by hovering over a certain data point, which will show you the name of the specific state 
       along with the count (n) of mental health cases."),
    plotlyOutput("scatter"),
    h4("As you can see, some states like Texas and Alabama have high mental health counts, while states like Wisconsin and District
       of Columbia have low mental health counts. Now we can plot the weather disasters in states to see the relationship between storms
       and location, with n representing the number of weather disasters per state."),
    plotlyOutput("scatter2"),
    h4("We can already see that there may be a connection between mental health and location, seeing as Texas is one of the highest states for both
       charts. If you need to see the storm data with more specificity, enter a state and find out how many storms have occured in that area."),
    textInput("state_input", "Enter State:"),
    textOutput("y_values_output")
  )
)

server_page2 <- function(input, output, session) {
  df$State <- as.factor(df$State)
  
  collected_data <- reactive({
    state_source_counts <- count(df, State, EVENT_NARRATIVE)
    state_source_counts <- arrange(state_source_counts, desc(n))
    state_source_counts <- state_source_counts[state_source_counts$n != 132, ] #wrong in the data 
    collected_data <- state_source_counts %>%
      group_by(State) %>%
      summarize(n = sum(n))
    arrange(collected_data, desc(n))
  })
  
  output$scatter <- renderPlotly({
    state_counts <- count(df, State)
    state_counts <- arrange(state_counts, desc(n))
    
    plot_data_df <- state_counts
    plot_data_df$State <- reorder(plot_data_df$State, -plot_data_df$n)
    
    plot_ly(data = plot_data_df, x = ~State, y = ~n, type = "scatter", mode = "markers",
            text = ~n, marker = list(color = ~n),
            layout = list(xaxis = list(title = "State"), yaxis = list(title = "Count")))
  })
  
  output$scatter2 <- renderPlotly({
    df$State <- as.factor(df$State)
    
    aggregated_data1 <- arrange(collected_data(), desc(n))
    
    plot_ly(data = aggregated_data1, x = ~State, y = ~n, type = "scatter", mode = "markers",
            text = ~n, marker = list(color = ~n, colorscale = 'Reds'),
            layout = list(xaxis = list(title = "State"), yaxis = list(title = "Storms")))
  })
  
  output$y_values_output <- renderText({
    entered_state <- tolower(input$state_input)  
    
    storms_value <- collected_data()[tolower(collected_data()$State) == entered_state, "n"]
    
    paste(" Storms: ", storms_value)
  })
}





ui_page3 <- fluidPage(
  titlePanel("Mental Health Across States"),
  br(),
  p("There is a disparity of mental health treatment across states and some people are able to receive treatment
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
      plotOutput(outputId = "bar"),
      p("The geographic variance in mental health treatment availability serves as a microcosm of broader societal challenges. This data is not just a collection of statistics; it is a reflection of systemic inequities that permeate our nation. The disparities go beyond mere numbers—they represent the lived experiences of individuals grappling with mental health concerns, highlighting the critical intersection of healthcare accessibility, regional demographics, and the prevailing attitudes toward mental health."),
      p("This page serves as a gateway to a deeper understanding of mental health treatment disparities across states. Through an interactive interface, you can select a specific state and delve into the nuanced landscape of mental health accessibility. The data presented here is not merely a reflection of numbers—it is a call to action, prompting us to consider the broader societal implications of mental health disparities and their potential ties to weather patterns.
      Join us in unraveling the intricate threads that weave together the disparities in mental health treatment across the United States, as we strive to shed light on the path toward a more equitable and compassionate future."),
      h4("To learn about the rankings of mental health per state, follow this link: "),
      a("Link to more info", href = "https://www.forbes.com/advisor/health-insurance/worst-states-for-mental-health-care/"),
      
      )
  )
)
server_page3 <- function(input, output, session) {
  filtered_df <- reactive({df[df$State == input$state_name, ]})
  output$bar <- renderPlot({
    req(nrow(filtered_df()) > 0, "filtered_df is empty")
    
    prescription <- sum(filtered_df()$Value[str_detect(filtered_df()$Indicator, regex("prescription", ignore_case = TRUE))])
    untreated <- sum(filtered_df()$Value[str_detect(filtered_df()$Indicator, regex("did not get", ignore_case = TRUE))])
    either_received <- sum(filtered_df()$Value[str_detect(filtered_df()$Indicator, regex("Took Prescription Medication for Mental Health And/Or Received Counseling or Therapy, Last 4 Weeks", ignore_case = TRUE))])
    therapy <- sum(filtered_df()$Value[str_detect(filtered_df()$Indicator, regex("Received Counseling or Therapy, Last 4 Weeks", ignore_case = TRUE))])
    unknown <- sum(filtered_df()$Value[!str_detect(filtered_df()$Indicator, regex("prescription|did not get|Took Prescription Medication for Mental Health And/Or Received Counseling or Therapy, Last 4 Weeks|Received Counseling or Therapy, Last 4 Weeks", ignore_case = TRUE))])
    
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


ui_page4 <- fluidPage(
  titlePanel("Assessing Your Risk"),
  br(),
  p("Due to an increase in natural disaster as a result of climate change, more and more people in the United States are put
    at risk of being victims of a storm. This endangers the mental health of all the people who are in high-risk areas for natural 
    disasters to strike. This page is for you to learn whether or not you would be in a high-risk area in recent years."),
  br(),
  textInput(inputId = "user_state",
            label = "Which state do you live in?"),
  textOutput("results"),
  tableOutput("table")
)

server_page4 <- function(input, output, session) {
  storm_data <- reactive({
    just_storms <- df[is.na(df$Subgroup) == TRUE, ]
    user_state <- toupper(input$user_state)
    if (user_state %in% toupper(just_storms$State)) {
      table <- just_storms[just_storms$State == input$user_state, c("EVENT_NARRATIVE", "EPISODE_NARRATIVE", "State", "EVENT_TYPE")]
      return(list(results = "Unfortunately, your state is a high-risk state in recent years. Be cautious, and make sure you are taking care of your mental health. Below is additional insight into the storms your state has experienced.", table = table))
    } else {
      return(list(results = "Congratulations! Your state is not a high-risk state according to recent data on natural disasters. This doesn't mean that you should forgo mental health maintenance. Take care of yourself! Below is some insight into other natural disasters within different states.", table = just_storms))
    }
  })
  
  output$results <- renderText({
    storm_data()$results
  })
  
  output$table <- renderTable({
    head(storm_data()$table, 3)
  })
}




ui_page5 <- fluidPage(
  mainPanel(
    div(class = "container text-center",
        h2("About: A Summary of the Project"),
        h3("Conducted by Natalie Delaat and Sanmathi Prabakar"),
        h4("Our journey begins with the acknowledgment that the intersection of mental health and climate has been a subject of global interest among psychologists. While not everyone may be susceptible to mental health issues related to climate change, research suggests that extreme weather events can lead to emotions such as depression, anger, and even violence (APHA). The consequences extend to self-harm, substance abuse, and suicidal ideation, making it a critical issue deserving of attention."),
        h4("Natural disasters, exacerbated by the current trend of global warming, are catalysts for immense pain and loss. As we witness rapid changes in the atmosphere, oceans, cryosphere, and biosphere, it becomes crucial to recognize the potential rise in mental health consequences (NASA). The urgency is emphasized by the United Nations' statement that we have only a limited time to prevent irreversible damage from climate change. If unaddressed, mental health problems may escalate in tandem with the worsening climate crisis."),   
        h4("It is easy to overlook these issues from a third-party perspective, but mental health is a fundamental human right crucial to personal, community, and socio-economic development (WHO). If climate change is indeed linked to the rise in mental health issues, empathetic consideration becomes imperative. This project aims to uncover the relationship between extreme climate events and mental health, with the hope that our findings will contribute to potential solutions and prevention tactics."),
        h4("Navigate through our exploration pages to delve deeper into specific aspects of the project:"),
        h4("1. Location and Mental Health Data: Analyzing the connection between location and mental health data."),
        h4("2. Mental Health Across States: Investigating variations in mental health treatment across states, highlighting disparities in access to care." ),
        h4("3.High-Risk Areas for Natural Disasters: Examining the impact of climate change on the mental health of individuals residing in high-risk areas for natural disasters. "),
        img(src = "https://www.neefusa.org/sites/default/files/vulnerable%20populations.png", width = 800, height = 600),
        h4("The graphic above shows some important issues pertinent to mental health--low income families are at risk of being in shelters 
           during natural disasters. Communities of color in high risk areas can face larger amounts of air pollution. The elderly are
           at risk during events that may require potential evacuation. These issues also cause extreme disparity, and can often be worse
           for those who are less privileged."),
        h4("To access the storm data only, follow this link: "),
        a("Link to storm data", href = "https://catalog.data.gov/dataset/ncdc-storm-events-database2"),
        h4("To access the mental health data only, follow this link: "),
        a("Link to mental health data", href = "https://catalog.data.gov/dataset/mental-health-care-in-the-last-4-weeks"),
        h4("The combined dataset is in our repository. To learn more about the connection between mental health and weather, follow this link: "),
        a("Link to more info", href = "https://enlightenedsolutions.com/5-ways-the-weather-can-affect-your-mental-health/#:~:text=One%20of%20the%20biggest%20ways,an%20actual%20episode%20of%20depression."),
    )
  )
)



ui <- navbarPage(
  "Shiny App",
  tabPanel("Introduction", ui_page1),
  tabPanel("Connecting Stormy Weather and Mental Health", ui_page2),
  tabPanel("Mental Health Across States", ui_page3),
  tabPanel("Assessing Your Risk", ui_page4),
  tabPanel("About Page: A Summary of the Project", ui_page5)
)

server <- function(input, output, session) {
 server_page2(input,output,session)
  observe({
    if (!is.null(input$state_name) && input$state_name != "") {
      server_page3(input, output, session)
    }
  })
  server_page4(input, output, session)
}


shinyApp(ui, server)
