library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)

source("unified_dataset.R")

controls <- sidebarPanel(
  selectInput("region", "Select Region:", choices = unique(df$Region)),
  checkboxGroupInput("type", "Select College Type:", choices = unique(df$type))
)

tuition_plot <- plotOutput("tuition_plot")
salary_plot <- plotOutput("salary_plot")
salary_scatter <- plotlyOutput("salary_scatter")
salary_scatter2 <- plotlyOutput("salary_scatter2")
takeaways_Sammyslide <- list(
  p("After analyzing the data using the slider and ggplot visualizations, several key observations emerge. Firstly, it is evident that colleges with higher tuition rates tend to have higher mean salaries, particularly for out-of-state students."),
  p("This suggests a positive correlation between tuition rates and potential career outcomes, indicating that students who invest in higher-cost education may have access to better job opportunities and higher starting salaries."),
  p("However, it is important to note that this trend is not absolute. There are instances where students who pay lower tuition still achieve high starting salaries, highlighting the presence of other influential factors such as program quality, networking opportunities, and individual skills and abilities."),
  p("These individual variations demonstrate the complexity of salary outcomes and underscore the need to consider multiple factors when making educational and career decisions."),
  p("Overall, this analysis offers valuable insights into the relationship between tuition rates and starting salaries, while emphasizing the importance of considering other relevant factors for a comprehensive understanding of career success.")
)

intro_page <- tabPanel( "Intro",
                        
                        h1("Exploring The Economics of Higher Education"),
                        
                        tags$img(src = "https://bachelors-completion.northeastern.edu/wp-content/uploads/2020/06/iStock-1162366190-1-1.jpg", height = 200, width = 400),
                        
                        
                        
                        h2("Project Overview"),
                        
                        p("This project is an interactive data exploration of higher education costs and investment returns, with a primary focus on the United States. Our goal is to reveal trends, differences, and unique insights related to students' college tuition fees, graduate income, and investment returns across different types, majors, and models of colleges."),
                        
                        
                        
                        h2("Why It Matters"),
                        
                        p("For most people, the decision to invest in higher education is of great significance and can greatly affect their future financial stability. Therefore, considering the high tuition fees of many colleges and the career income of graduates, it is crucial for today's college students to understand the financial impact of choosing different paths. We believe that this project is a resource for students, parents, educators, policy makers, and anyone interested in higher education economics."),
                        
                        
                        
                        h2("Data Sources and Collection"),
                        
                        p("The data used in this project was primarily sourced from two datasets: 'salaries-by-region.csv' and 'tuition_cost.csv'. The first dataset, which we sourced from the Wall Street Journal, provides information on post-college salaries across various regions and types of colleges. The second dataset, sourced from the US Department of Education and cleaned for the TidyTuesday project, offers comprehensive information on college tuition costs, including variables like geographical location, degree type, and potential salary.."),
                        
                        
                        
                        h2("Our Data Stories"),
                        
                        p("We contain a total of three distinct but interconnected data stories that examine the costs and potential returns on higher education from different angles with three different types of data story telling. 'Drill Down' delves into the disparity between in-state and out-of-state tuition costs at public schools. 'Factors' explores how different degree types can influence tuition costs and potential earning outcomes. Finally, 'Contrast' contrasts the tuition costs and post-college earnings of public and private institutions, including the differences between private and public colleges' impact on starting and mid-career stages."),
                        
                        tags$img(src = "https://image.cnbcfm.com/api/v1/image/106968886-1635800217529-gettyimages-1205339175-4007_09_k5_dsc9519.jpeg?v=1635800247&w=740&h=416&ffmt=webp&vtcrop=y", height = 200, width = 400),
                        
                        
                        
                        h2("Background Research/Inspiration"),
                        
                        tags$ul(
                          
                          tags$li(tags$a(href="https://www.cnbc.com/2021/11/02/the-gap-in-college-costs-and-earnings-for-young-workers-since-1980.html", "CNBC: The gap in college costs and earnings for young workers since 1980"), " discusses the rising college tuition costs and increasing demand for postsecondary education."),
                          
                          tags$li(tags$a(href="https://www.forbes.com/sites/michaeltnietzel/2022/08/30/how-forbes-top-25-colleges-stack-up-on-alumni-salaries/?sh=292238285fff", "Forbes: How Forbes' Top 25 Colleges Stack Up On Alumni Salaries"), " focuses on the trajectory of salaries from top colleges."),
                          
                          tags$li(tags$a(href="https://www.ssa.gov/policy/docs/research-summaries/education-earnings.html", "Social Security Administration: Education and Lifetime Earnings"), " emphasizes the economic benefits of a college degree."),
                          
                          tags$li(tags$a(href="https://www.forbes.com/advisor/student-loans/is-college-worth-it/", "Forbes Advisor: Is College Worth It"), " explores the pros and cons of acquiring a college degree."),
                          
                          tags$li(tags$a(href="https://www.indeed.com/career-advice/pay-salary/average-salary-in-us", "Indeed: Average Salary in the US"), " discusses demographic factors affecting salaries in the US.")
                          
                        )
                        
)



aryan_pg <- tabPanel( "College Analysis",
                      fluidPage(
                        titlePanel("College Data Visualizations"),
                        sidebarLayout(
                          controls,
                          mainPanel(
                            h4("Drill Down: Tuition and Salary"),
                            p("The cost of college has experienced a significant surge over the past few decades, 
                              surpassing inflation-adjusted rates. Consequently, individuals are compelled to rely 
                              on student loans and accumulate substantial debt in order to afford the exorbitant expenses
                              associated with higher education."),  
                              
                            p("Public schools often charge higher out-of-state tuition 
                              fees compared to in-state tuition fees, as the latter is subsidized by taxpayer dollars 
                              from the respective state. This disparity in pricing between in-state and out-of-state 
                              students will be further explored, particularly concerning specific colleges. 
                              Additionally, international students typically face substantially higher education costs 
                              in the United States."), 
                              
                              p("Conversely, private schools generally do not differentiate between 
                              in-state and out-of-state students in terms of tuition fees, regardless of the group compared,
                              the tuition fees are far lower than those of private universities without discrimination. 
                              The high tuition fees of private universities often become a reason for many people to 
                              question their investment returns."),
                            tabsetPanel(
                              tabPanel("Tuition By State",
                                       h4("Honing In On Tuition By State"),
                                       p("This bar graph provides a visual representation of the in-state total
                                                tuition for different states. Each bar on the graph represents a state,
                                                with the x-axis denoting the state code and the y-axis representing the
                                                corresponding in-state total tuition. The height of each bar directly
                                                reflects the tuition cost for that particular state, allowing for a
                                                straightforward comparison of tuition rates among states. You can see
                                                that depending on the state, the tuition varies widely, which is partially
                                                due to the cost of living in each state.
                                                "),                                             tuition_plot,),
                              
                              tabPanel("Starting Salaries",
                                       h4("The Pay Off: Focusing on Median Salary By College"),
                                       p("The bar graph offers a visual representation of the starting median salary for different colleges.
                                              Each bar on the graph corresponds to a college, with the x-axis representing the colleges, reordered based
                                                on their starting median salary in descending order. The y-axis depicts the respective starting median
                                                salary for each college. The same pattern for starting salaries is visible, and as we drill down on
                                                states such as New York, we see that they have higher salaries to compensate for the higher cost of living in those areas. "),                                             salary_plot,
                              ),
                              
                              tabPanel("Takeaways",
                                       h4("The Main Conclusion From This Story"),
                                       p("Tuition is generally more expensive for private colleges as a whole, but some states have a significantly higher increase in the
                                         difference between private and public colleges. "),                                            
                              )
                              
                            )
                          )
                        )
                      )
)


sammys_pg <- tabPanel( "Tuition-Salary analysis",
                       fluidPage(
                         setBackgroundColor(
                           color = c("#F7F8FF", "#FFDAB9"),
                           gradient = ("radial"),
                           direction = "top"
                         ),
                         titlePanel("Comparing Tuition and Starting Income: Interactive Analysis"),
                         sidebarLayout(
                           sidebarPanel(
                             h3("Cost of Tuition"),
                             sliderInput(
                               inputId = "cost",
                               label = "Filter by tuition cost(USD $)",
                               min = 0,
                               max = 80000,
                               value = 80000,
                             ),
                             tabsetPanel(
                               tabPanel("How to Use",
                                        titlePanel("How to Use"),
                                        p("Explore the relationship between tuition cost and starting median salary for various colleges. 
                               Use the slider to filter the data by tuition cost and observe how it impacts the average starting salary.
                               Hover over the data points on the scatter plot to view the name of the corresponding college. This scatterplot
                               analyzes the relationship between the cost of tuition and average staring salary per school. "),                                            
                               ),
                               tabPanel("Background",
                                        titlePanel("Background Knowledge"),
                                        p("In recent years, the cost of college education has seen a remarkable surge, 
                               exceeding inflation-adjusted rates. As a result, students and their families 
                               are increasingly relying on student loans and accumulating significant debt 
                               to afford the exorbitant expenses associated with higher education. One contributing 
                               factor to the cost disparity is the distinction between in-state and out-of-state tuition 
                               fees at public colleges and universities. In-state tuition is typically subsidized by taxpayer
                               dollars from the respective state, making it more affordable for residents. However, out-of-state
                               students often face significantly higher tuition fees, reflecting the absence of such subsidies. 
                               This discrepancy in pricing between in-state and out-of-state students will be a focal point of our analysis
                               , particularly in relation to the starting salaries of graduates from specific colleges. It is worth noting 
                               that international students generally encounter even higher education costs in the United States. 
                               On the other hand, private institutions tend to maintain consistent tuition fees for both in-state and
                               out-of-state students, with tuition rates generally lower than those of private universities. The high tuition 
                               fees charged by private universities often raise questions regarding the return on investment for prospective students.")                                           
                               )
                               
                             ),
                             
                     
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Out of State", salary_scatter),
                               tabPanel("InState", salary_scatter2),
                               
                               tabPanel("Takeaways",
                                        h4("What does this mean?"),
                                        p("After analyzing the data using the slider and ggplot visualizations, 
                                          several key observations emerge. Firstly, it is evident that colleges 
                                          with higher tuition rates tend to have higher mean salaries, particularly
                                          for out-of-state students."), 
                                          
                                          p("This suggests a positive correlation between tuition
                                          rates and potential career outcomes, indicating that students who invest in higher-cost
                                          education may have access to better job opportunities and higher starting salaries. However, 
                                          it is important to note that this trend is not absolute."), 
                                          
                                          p("There are instances where students who
                                          pay lower tuition still achieve high starting salaries, highlighting the presence of other influential 
                                          factors such as program quality, networking opportunities, and individual skills and abilities. These 
                                          individual variations demonstrate the complexity of salary outcomes and underscore the need to consider 
                                          multiple factors when making educational and career decisions."), 
                                          
                                          p("Overall, this analysis offers valuable insights
                                          into the relationship between tuition rates and starting salaries, while emphasizing the importance of considering
                                          other relevant factors for a comprehensive understanding of career success.. "),                                            
                               )
                             )
                             
                           )
                         )
                       )
)

zhikai_pg <- tabPanel("Private vs Public",
                      titlePanel("Contrast: Different Stages of Earnings and Tuition Between Public and Private Colleges"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          h3("Explore the data:"),
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Background Information", 
                                     h4("The Contrasting Worlds of Public and Private Colleges:"),
                                     textOutput("overview")),
                            
                            tabPanel("Tuition and Starting Salary Comparison", 
                                     h4("Breaking Down the Dollars: Tuition Costs and Starting Salaries"),
                                     p("Through the chart, we can see that although public universities generally charge higher fees for out state students, regardless of the group compared, the tuition fees are far lower than those of private universities without discrimination. The high tuition fees of private universities often become a reason for many people to question their investment returns."),
                                     plotOutput("comp_plot")),
                            
                            tabPanel("Career Progression",
                                     
                                     h4("Career Trajectory: The Value of Our Degree Between Private and Public Colleges Over Time"),
                                     
                                     p("When we shift our attention to the salary increase of graduates through their entire care, another interesting aspect emerges. Although private university graduates initially had a slim advantage in starting salary and this makes the investment return of private colleges looks not really great, in the middle of their care, this gap because even greater. This observation has been promoted us to have a more positive view of the investment returns provided by private universities, but one factor that people should also consider is that the impact of graduating from a university on personal income is usually the greatest in the early stages of their career."),
                                     
                                     plotOutput("career_plot")),
                            
                            tabPanel("Takeaways",
                                     h4("Why Should We Care: The Implications of Our Findings"),
                                     textOutput("takeaways"))
                          )
                        )
                      )
)
takeaways_pg <- tabPanel( "Summary",
                          h1("What Are The Main Takeaways?"),
                          p("By visualizing the relationship between the college tuition cost and potential
                          income of graduates for multiple different groups of individuals, we were able
                          to gain a more comprehensive understanding of the value of higher education and the
                          challenges faced by students and families in paying for it, and solve the questions
                          that a lot of individuals are concerned about. "),
                          titlePanel("#1:Tuition Disparity"),
                          h4("Our analysis reveals that private colleges, as a whole, tend to have higher tuition costs compared to public colleges. However, we have also identified specific states where the difference between private and public college tuition is significantly higher. This highlights the importance of considering location and regional factors when evaluating the cost of education."),
                          titlePanel("#2: Tuition and Career Outcomes"),
                          h4("The analysis of tuition rates and income data indicates a positive correlation between higher tuition rates and potential career outcomes. We observed that colleges with higher tuition rates generally have higher mean salaries, particularly for out-of-state students. This suggests that investing in higher-cost education may provide students with access to better job opportunities and higher starting salaries. However, it is important to note that this trend is not absolute. Some students who pay lower tuition still achieve high starting salaries, suggesting the presence of other influential factors such as program quality, networking opportunities, and individual skills and abilities. Therefore, it is crucial to consider multiple factors when making educational and career decisions."),
                          titlePanel("#3: Education Investment and Future Financial Situation"),
                          h4("Our aim was to compare tuition costs and income at different career stages between private and public universities. This analysis sheds light on the value and potential returns of educational investments. By understanding the relationship between tuition costs and income, we can provide valuable insights for students, families, and policymakers seeking to improve higher education opportunities and promote social equity. Making informed decisions about education investments can have a significant impact on an individual's future financial situation."),
                          
)
# Define UI for application that draws a histogram
ui <- navbarPage("Group 65",
                 intro_page,
                 aryan_pg,
                 sammys_pg,
                 zhikai_pg,
                 takeaways_pg
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_data <- reactive({
    df %>%
      filter(Region == input$region, type %in% input$type)
  })
  
  output$tuition_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = state_code, y = in_state_total)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "State Code", y = "In-State Total Tuition",
           title = "In-State Total Tuition by State") +
      theme_minimal()
  })
  
  output$salary_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(state, -Starting.Median.Salary), y = Starting.Median.Salary), height = 1500 , width = 7) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "College", y = "Starting Median Salary",
           title = "Starting Median Salary by College") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
  })
  
  output$salary_scatter <- renderPlotly({
    print(input$cost)
    
    filtered_df <- filter(grouped_df, out_of_state_tuition <= input$cost)
    print(filtered_df$out_of_state_tuition)
    
    scatter <- ggplot(filtered_df, aes(x = out_of_state_tuition, y = Starting.Median.Salary)) +
      geom_point(aes(col = name), show.legend = FALSE) +
     # geom_text(aes(label = name), vjust = -0.5, hjust = 0.5, color = "black", size = 3) +
      ggtitle("Out of state tuition vs Avg starting salary per school") +
      labs(y = " Starting Median Salary(USD $)", x = "Out of State Tuition(USD $)")
    p <- ggplotly(scatter)
    p <- layout(p, height = 650)
  })
  
  output$salary_scatter2 <- renderPlotly({
    print(input$cost)
    
    filtered_df <- filter(grouped_df, in_state_tuition <= input$cost)
    print(filtered_df$in_state_tuition)
    
    scatter2 <- ggplot(filtered_df, aes(x = in_state_tuition, y = Starting.Median.Salary)) +
      geom_point(aes(col = name), show.legend = FALSE) +
      # geom_text(aes(label = name), vjust = -0.5, hjust = 0.5, color = "black", size = 3) +
      ggtitle("In state tuition vs Avg starting salary per school") +
      labs(y = " Starting Median Salary(USD $)", x = "In State Tuition(USD $")
    p <- ggplotly(scatter2)
    p <- layout(p, height = 650)
  })
  output$overview <- renderText({
    paste("This data story utilizes two primary datasets: 'salaries-by-region.csv' and 'tuition_cost.csv'. Our data is categorized by public and private institutions, enabling us to highlight the cost, receive, and return on investment between these two types of colleges. Notably, while private colleges tend to have a standard tuition rate for all students, public colleges often differ in their in-state and out-of-state rates. However, this part is not the primary focus of our narrative here. Instead, we are interested in the contrast directly between private and public colleges for individuals, including the tuition cost, and starting and mid-career median salaries across these different institution types. We believe this contrast provides significant insights, as the impact of an institution's type on salary tends to diminish as one's career progresses.")
  })
  
  output$comp_plot <- renderPlot({
    ggplot(data_tuition, aes(x=College_Type, y=Avg_Tuition, fill=Tuition_Type)) +
      geom_bar(stat="identity", position="dodge") +
      theme_minimal() +
      labs(title="Comparison of Average Tuition Costs",
           x="Type of College",
           y="Average Tuition Cost",
           fill="Tuition Type")
  })
  
  output$career_plot <- renderPlot({
    ggplot(data_salary_progression, aes(x=Career_Stage, y=Avg_Salary, fill=College_Type)) +
      geom_bar(stat="identity", position="dodge") +
      theme_minimal() +
      labs(title="Comparison of Average Salaries at Different Career Stages",
           x="Career Stage",
           y="Average Salary",
           fill="Type of College")
  })
  
  output$takeaways <- renderText({
    
    paste("Education investment is an important decision that affects an individual's future financial situation. From our comparison of tuition costs and income at different career stages after graduating from private and public universities, we can see that the tuition fees of private universities are generally several times higher than those of public universities. At the same time, the starting salary of graduates after entering the workforce has not widened so much. Therefore, the value and potential returns of investments in these private universities are not optimistic. We believe that this analysis can provide valuable insights for future students, families, and policy makers seeking to improve higher education opportunities and social equity.")
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)


