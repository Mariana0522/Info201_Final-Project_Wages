library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)
library(RColorBrewer)
library(plotly)

wages1 <- read_delim("labor_market_college_grads.csv")
wages2 <- read_delim("wages.csv")
names(wages2) <- c("year", "bachelor_25th percentile", "bachelor_median",
                   "bachelor_75th percentile", "high_school_median")
wages2$year <- substr(wages2$year,start=5, stop=8)
wages2$year <- as.numeric(wages2$year)
filtered_wages <- wages2[, c("year", "high_school_median", "bachelor_median")]
new_wages2 <- melt(filtered_wages, id.vars = 'year', variable.name ='Education')


ui <- fluidPage(
  tabsetPanel(
    # Panel for Introduction
    tabPanel("Introduction",
             h2("Project Overview"),
             p("This report is meant to be used as a tool for high school 
              and college students. The interactive graphs and data provided 
              will assist students with multiple difficult, school-related decisions.
              Including whether or not to go to college in the first place and selecting
              their college major/career path.As current students ourselves, 
              we simply hope to assist others in making this very intimidating, 
              stressful decision in life as painless and easy as possible."),
             h2("Data Set"),
             p("The “US College Graduates Wages” Dataset was collected by Daniel Simpson, 
              a data scientist at the Birkbeck University of London. He created the dataset based
              on data collected from the Federal Reserve Bank of New York. The dataset was last
              updated two years ago and includes information about wages for specific majors,
              unemployment rates, underemployment rates, and wages based on level of education."),
             h2("Target Audience"),
             p("Our target audience are all US national and international high school and college students.
              We provide data on US college graduates’ median wages, unemployment rates, and underemployment 
              rates based on different majors. Moreover, we also compared the median wages between the two 
              education levels to help students learn their career prospects."),
             h2("Question"),
             p("The questions we focused on are:"), 
             p("1. Which major has the highest average wage? How does the median wage change from early-career to mid-career?"),
             p("2. Which majors have the lowest unemployment rates? Which have the highest? Underemployment rates?"),  
             p("3. What are the differences between high school and college students' wages? How have these wages changed over time?"),
             h2("Creators"),
             p("Mariana Li Chen"),
             p("Zitian Zeng"),
             p("Jake Sasaki")
    ),
    # Panel for Early and Middle Career Wages by Major
    tabPanel("Early and Middle Career Wages by Major",
             titlePanel("Early and Middle Career Wages by Major"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("wage", "Early career vs. Mid-career",
                              choices = c("Early career", "Mid-career")),
                 DTOutput("table3")
               ),
               mainPanel(
                 plotOutput("plot"),
                 textOutput("text")
               )
             ) 
    ),
    # Panel for Unemployment and Underemployment by Major
    tabPanel("Unemployment and Underemployment by Major",  
             titlePanel("Unemployment and Underemployment by Major"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("select", "Choose a major:", choices = wages1$Major, 
                             multiple = TRUE, selected = c("Computer Science",
                                                           "Mechanical Engineering",
                                                           "Economics", "Accounting", 
                                                           "Biology")),
                 DTOutput("table1"),
                 DTOutput("table2")
               ),
               mainPanel(
                 plotOutput("barPlot1"),
                 htmlOutput("text1", style = "font-size:12px;"),
                 div(style = "margin-top: 40px;"),
                 plotOutput("barPlot2"),
                 htmlOutput("text2", style = "font-size:12px;")
               )
             )
    ),
    # Panel for US High School Degree vs. Bachelor's Degree Wage
    tabPanel("US High School Degree vs. Bachelor's Degree Wage",
             titlePanel("US High School and Bachelor's Wage Changes Graphs"),
             sidebarLayout(
               sidebarPanel(
                 p("You can analyze the different median wages between a high school degree and a bachelor's degree. Select
                   the education that you are interested in to observe the scatterplot and the
                   connecting line"),
                 checkboxInput("line", "Line Connection"),
                 radioButtons("color", "Palette",
                              choices = c("Standard", "Color")),
                 radioButtons("wage2", "High School vs Bachelor's",
                              choices = c("high school", "bachelor's", "combine")),
               ),
               mainPanel(
                 plotOutput("plot3"),
                 textOutput("text3")
               )
             )
             
    ),
    # Panel for Conclusion
    tabPanel("Conclusion", 
             h2("Conclusion"),
             p("From our data on early and middle career wages, we discovered 
             that engineering based majors tend to be the most lucrative. 
             9 out of the top 10 majors in early career wages and 6 out of the 
             top 10 in middle career wages were based in engineering. 
             The top 4 of these include chemical, electrical, aerospace, 
             and computer engineering with early career wage landing upwards of $65,000 
             and middle career wages trending upwards from $100,000.After observing and 
             comparing high school and bachelor’s degree median wage, 
             we can see that there is a huge wage gap and bachelor’s median wage has always been higher."),
             p("This dominance in terms of engineering majors’ wages informs us how much the world 
               values innovation. The engineering field is typically in the forefront of developing 
               new technologies that shape society as a whole. Engineering’s versatility across 
               industries justifies its wages and makes it an excellent choice of major."),
             h2("Dataset"),
             p("While our data is coming from a reliable source and presents insights 
             on a multitude of questions, its lack of details on important factors 
             including gender and race may mislead some more underrepresented groups
             and induce biased results. For example, the wages displayed in the most 
             lucrative majors are likely made up of mostly white males, meaning that
             the wages may differ for other groups."),
             h2("Future Ideas"),
             p("Because our data was separated into multiple datasets,
             in the future we would like to combine them to improve convenience.
             We would also like to include more factors in our data that better
             represent minority groups."))
  )
)

server <- function(input, output) {
  # Plot for Early and Middle Career Wages by Major
  output$plot <- renderPlot({
    if(input$wage == "Early career"){
      wages1 %>%
        ggplot()+
        geom_col(aes(x = Major, y = `Median Wage Early Career`), fill = "red")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }else{
      wages1 %>%
        ggplot()+
        geom_col(aes(x = Major, y = `Median Wage Mid-Career`), fill = "blue")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
  })
  # Text for Early and Middle Career Wages by Major
  output$text <- renderText({
    paste("You can choose two graphs that display either the median wage in early career or
               the median wage in mid career by major of different majors.")
  })
  # Table for Early and Middle Career Wages by Major
  output$table3 <- renderDT({
    if(input$wage == "Early career"){
      dt3 <- wages1 %>% 
        select(Major, `Median Wage Early Career`) %>% 
        arrange(desc(`Median Wage Early Career`))
      datatable(dt3, options = list(pageLength = 5, lengthMenu = c(1, 3, 5)))
    }else{
      dt4 <- wages1 %>% 
        select(Major, `Median Wage Mid-Career`) %>% 
        arrange(desc(`Median Wage Mid-Career`))
      datatable(dt4, options = list(pageLength = 5, lengthMenu = c(1, 3, 5)))
    }
  })
  
  # First Plot for Unemployment and Underemployment by Major
  data <- reactive({
    req(input$select)
    df <- wages1 %>% 
      filter(Major %in% input$select)
  })
  output$barPlot1 <- renderPlot({
    unp <- ggplot(data(), aes(y = `Unemployment Rate`, x = Major, fill = Major))
    unp + geom_bar(stat = "sum") +
      geom_text(aes(label = round(`Unemployment Rate`, 2)), vjust = -0.5) + 
      xlab("Major") +
      ylab("Unemployment Rate") +
      theme(axis.text.x = element_text(face = "bold", angle = 50, vjust = 1, hjust=1)) +
      theme(axis.text.y = element_text(face = "bold")) +
      theme(axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 20))) +
      theme(axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 20)))
  })
  # First Text for Unemployment and Underemployment by Major
  output$text1 <- renderText({
    "This bar plot displays the unemployment rate for each college major. Future and current college
    students can use this as a resource in deciding their major. Knowing the unemployment rates of each major
    allows students to understand the how difficult it is to get a job after school if this career path is chosen."
  })
  # Second Plot for Unemployment and Underemployment by Major
  output$barPlot2 <- renderPlot({
    und <- ggplot(data(), aes(y = `Underemployment Rate`, x = Major, fill = Major))
    und + geom_bar(stat = "sum") +
      geom_text(aes(label = round(`Underemployment Rate`, 2)), vjust = -0.5) +  
      xlab("Major") +
      ylab("Underemployment Rate") +
      theme(axis.text.x = element_text(face = "bold", angle = 50, vjust = 1, hjust=1)) +
      theme(axis.text.y = element_text(face = "bold")) +
      theme(axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 20))) +
      theme(axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 20)))
  })
  # Second Text for Unemployment and Underemployment by Major
  output$text2 <- renderText({
    "This bar plot displays the underemployment rate for each college major. Underemployment is 'the siuation
    of those who are able to find employment only for shorter than normal periods' or someone whose work doesn't 
    justify their skills or education. An example of this could be someone with an engineering degree working
    part-time at a retail store. This information will inform students about which jobs are in higher demand due to
    their lower underemployment rate."
  })
  # First Table for Unemployment and Underemployment by Major
  output$table1 <- renderDT({
    dt1 <- wages1 %>% 
      select(Major, `Unemployment Rate`) %>% 
      arrange(`Unemployment Rate`) 
    datatable(dt1, options = list(pageLength = 5, lengthMenu = c(1, 3, 5)))
  })
  # Second Table for Unemployment and Underemployment by Major
  output$table2 <- renderDT({
    dt2 <- wages1 %>% 
      select(Major, `Underemployment Rate`) %>% 
      arrange(`Underemployment Rate`) 
    datatable(dt2, options = list(pageLength = 5, lengthMenu = c(1, 3, 5)))
  })
  
  
  # Plot for US High School Degree vs. Bachelor's Degree Wage
  color_palette <- reactive({
    if(input$color == "Standard") {
      brewer.pal(9, "Set1")
    }else{
      brewer.pal(9, "Set2")
    }
  })
  
  output$plot3 <- renderPlot({
    if(input$wage2 == "high school"){
      p <- ggplot(wages2, aes(x = year, y= high_school_median))+
        geom_point(color = color_palette()[[1]])+
        xlab("Year")+
        ylab("High School Wage Median")
      
      if(input$line){
        p <- p + geom_line(aes(group =1), color = color_palette()[[2]])
      }
      p
    }
    else if(input$wage2 == "bachelor's"){
      p <- ggplot(wages2,aes(x= year, y=bachelor_median))+
        geom_point(color = color_palette()[[1]])+
        xlab("Year")+
        ylab("Bachelor's Wage Median")
      
      if(input$line){
        p <- p + geom_line(aes(group=1), color = color_palette()[[2]])
      }
      p
    }
    else{
      ggplot(new_wages2, aes(year, value))+
        geom_line(aes(colour = Education))+
        xlab("Year")+
        ylab("Wage Median")
      
    }
  })
  # Text for US High School Degree vs. Bachelor's Degree Wage
  output$text3 <- renderText({
    if(input$wage == "high school"){
      "This Scatter Plot shows the median US wage for High School students
      from the years 1990 to 2019, there are no missing values in this dataset"}
    else if(input$wage == "bachelor's"){" This Scatter Plot shows the median US
      wage for Bachelor's students from the years 1990 to 2019, there are no missing
      values in this dataset "}
    else {"This is a line graph that compares the median wage between High School
      and Bachelor's in the same graph from the years 1990 to 2019, there is no missing
      values in this dataset."}
  })
}

shinyApp(ui = ui, server = server)

