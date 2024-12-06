ui4 <- navbarPage(
  title = "Guide on Sexual & Reproductive Education and Life Skills",
  
  # Add Custom CSS for Tab Colors and Title Sizes
  tags$head(
    tags$style(HTML("
      /* Navbar Style */
      .navbar { background-color: #4CAF50; }          /* Navbar Background */
      .navbar-default .navbar-nav > .active > a { background-color: #8BC34A !important; } /* Active Tab */
      
      /* Title Styles */
      .navbar-header .navbar-brand { font-size: 32px; font-weight: bold; color: white; } /* Main Title Size */
      .tab-panel h3, .tab-panel h2 { font-size: 26px; font-weight: bold; } /* Sub Tab Titles */
      
      /* Tab Content Styling */
      .tab-content { padding: 20px; }                /* Content Padding */
      
      /* Tab Background Colors */
      .tab-pane.introduction { background-color: #f9fbe7; } /* Light Green */
      .tab-pane.ovulatory-cycle { background-color: #e8f5e9; } /* Light Blue */
      .tab-pane.helpful-links { background-color: #fce4ec; } /* Light Pink */
      .tab-pane.one-stop-center { background-color: #e3f2fd; } /* Light Cyan */
      .tab-pane.personalized-advice { background-color: #fff3e0; } /* Light Orange */
    "))
  ),
  
  # First Tab: Introduction
  tabPanel("Introduction",
           value = "introduction",
           fluidPage(
             h2("Welcome to the Guidance App"),
             p("This app provides educational resources on topics like pregnancy prevention, ovulatory cycles, abstinence, and more. Explore the tabs to learn!")
           )),
  
  # Second Tab: Ovulatory Cycle
  tabPanel("Ovulatory Cycle",
           value = "ovulatory-cycle",
           fluidPage(
             h3("Understanding Your Ovulatory Cycle"),
             p("Track your menstrual cycle to understand your fertile days."),
             plotOutput("cycleChart"),
             p("Regular cycles help predict ovulation. This can assist in planning or preventing pregnancy."),
             
             tags$ul(
               tags$li(a("Learn more about Ovulatory-cycle",
                         href ="https://www.yourfertility.org.au/everyone/timing", target = "_blank"))
             )
           )),
  
  # Third Tab: Helpful Links
  tabPanel("Helpful Links",
           value = "helpful-links",
           fluidPage(
             h3("Helpful Videos for Teenage Girls"),
             p("Below are some online videos that provide valuable guidance on preventing unwanted pregnancy:"),
             
             tags$ul(
               tags$li(a("Video 1: Teenage Pregnancy Prevention", 
                         href = "https://youtu.be/xD0a8Tm5VT4", target = "_blank")),
               tags$li(a("Video 2: Guidance for Teenage Girls", 
                         href = "https://youtu.be/hKolGTaO08A", target = "_blank"))
             ),
             
             p("Click on the links to watch the videos in a new tab.")
           )),
  
  # Fourth Tab: One Stop Center
  tabPanel("Isange One Stop Center",
           value = "one-stop-center",
           fluidPage(
             h3("Call Isange One Stop Center"),
             p("If you or someone you know needs help, you can call the Isange One Stop Center at:"),
             tags$h4("📞 3029 (Toll-Free)"),
             p("Trained professionals are available to provide assistance and guidance."),
             p("Your call will be treated with the utmost confidentiality.")
           )),
  
  # Fifth Tab: Personalized Advice
  tabPanel("Personalized Advice",
           value = "personalized-advice",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h3("Personal Information"),
                 textInput("name", "Your Name:", ""),
                 numericInput("age", "Your Age:", value = 16, min = 10, max = 20),
                 selectInput("education", "Current Education Level:",
                             choices = c("Primary School", "Secondary School", "Higher Education", "Not in School")),
                 radioButtons("working", "Are you currently working?",
                              choices = c("Yes", "No")),
                 actionButton("submit", "Get Advice")
               ),
               mainPanel(
                 h3("Personalized Advice"),
                 textOutput("message"),
                 hr(),
                 h3("Educational Resources"),
                 tags$ul(
                   tags$li(a("Health Development Initiative Rwanda", href = "https://hdirwanda.org/", target = "_blank")),
                   tags$li(a("Importance of Staying in School", href = "https://www.unicef.org/education", target = "_blank")),
                   tags$li(a("How to Avoid Early Pregnancy", href = "https://www.who.int/health-topics/adolescent-health", target = "_blank"))
                 )
               )
             )
           ))
)

# Define Server Logic
server4 <- function(input, output) {
  
  # Ovulatory Cycle Plot
  output$cycleChart <- renderPlot({
    # Ovulatory cycle example
    days <- 1:28
    hormone_levels <- c(rep(1, 5), seq(1, 10, length.out = 5), seq(10, 1, length.out = 5), rep(1, 13))
    
    plot(days, hormone_levels, type = "l", col = "blue", lwd = 2,
         xlab = "Day of Cycle", ylab = "Hormone Levels",
         main = "Ovulatory Cycle Visualization")
  })
  
  # Personalized Advice Logic
  observeEvent(input$submit, {
    advice <- ""
    
    if (input$education == "Primary School") {
      advice <- "Focus on building strong learning habits and always ask for help when needed."
    } else if (input$education == "Secondary School") {
      advice <- "Stay motivated in your studies and explore your interests for a potential career."
    } else if (input$education == "Higher Education") {
      advice <- "Take advantage of opportunities to network and prepare for your professional life."
    } else if (input$education == "Not in School") {
      advice <- "Consider returning to school or finding alternative learning opportunities."
    }
    
    if (input$working == "Yes") {
      advice <- paste(advice, "While working is admirable, focus on education while you're young.")
    }
    
    output$message <- renderText({
      paste("Hi", input$name, ",", advice)
    })
  })
}
shinyApp(ui = ui4, server = server4)
