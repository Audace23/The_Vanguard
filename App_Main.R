#Assessing the risk of getting pregnant.
#packages
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(leaflet)
library(bs4Dash)
library(sf)
library(bslib)
library(leafdown)
library(shinyjqui)
library(sortable)
library(readxl)
library(fresh)
library(highcharter)
library(rsconnect)
library(mailtoR)
library(shinyjs)
library(forecast)
library(timeSeries)
library(emayili)
# Manipulated Data
Percentage_of_teenage<-read.csv("./www/Percentage_of_teenage.csv")
LatestData_EAC<- read.csv("./www/LatestData_EAC.csv")
#Selecting colors
colors<- c("#034742","#2ca02c","#092CA0", "#431d85", "#570631","#9e086f")
#convert to tree_map oriented data
LatestData_EAC_TreeMap<-LatestData_EAC%>%
  mutate(name = Country,
         value = Mean_BirthRate,
         color = colors) %>%
  list_parse()

Pregnancy_per_District<-st_read("./www/Pregnancy_per_District.shp")
Top5_Districts<- read.csv("./www/Top5_Districts.csv")
Religion<- read.csv("./www/Religion.csv")
TypeOfResidence<-read.csv("./www/TypeOfResidence.csv")
WealthIndex<-read.csv("./www/WealthIndex.csv")
Women_under20_2020<-read.csv("./www/Women_under20_2020.csv")
RatePerRegion<- read.csv("./www/RatePerRegion.csv")
RatePerReligion<- read.csv("./www/RatePerReligion.csv")
RatePerWealthIndex<- read.csv("./www/RatePerWealthIndex.csv")
#Quick manipulations
RatePerRegion$Pregnancy_Rate<- round(RatePerRegion$Pregnancy_Rate,2)
RatePerReligion$Pregnancy_Rate<- round(RatePerReligion$Pregnancy_Rate,2)
RatePerWealthIndex$Pregnancy_Rate<- round(RatePerWealthIndex$Pregnancy_Rate,2)
colnames(RatePerWealthIndex)[colnames(RatePerWealthIndex)=="Wealth.Index"]<- "Wealth_Index"
RatePerReligion<- RatePerReligion%>%
  dplyr::group_by(Religion)%>%
  dplyr::summarise(Pregnancy_Rate = mean(Pregnancy_Rate))
#other data sets
Teenager_preg <- read.csv("./www/EverPregnant_DataFrame.csv")
newdata2<- read.csv("./www/newdata2.csv")
#those are Percentages from manipulation script
EverPregnant2005<- "6.56%"
EverPregnant2015<- "7.46%"
EverPregnant2020<- "5.14%"
EverPregnant2010<- "6.04%"
#Data set
EverPregnant_DataFrame<- data.frame(DHS = c(2005,2010,2015,2020),
                                    Pregnancy_Rate = c(6.56,6.04,7.46,5.14))
Forecast_Data<- read.csv("./www/Forecast.csv")
Forecast_Data$ds<- ifelse(Forecast_Data$ds==2005.8,2025,2030)
colnames(Forecast_Data)[colnames(Forecast_Data)=="ds"]<- "DHS"
colnames(Forecast_Data)[colnames(Forecast_Data)=="yhat"]<- "Pregnancy_Rate"
Forecast_Data$Nature<- "Forecast"
EverPregnant_DataFrame_101<- EverPregnant_DataFrame
EverPregnant_DataFrame_101$Nature<- "Historical"
Forecast_Data<- Forecast_Data%>%
  full_join(EverPregnant_DataFrame_101)%>%
  arrange(DHS)%>%
  select(2,6,3,4,5)
Forecast_Data$DHS<- as.integer(Forecast_Data$DHS)
#Paths to the image used
path_description<- "./www/VariableeeDescription.png"
path_summary<- "./www/Logit_m2_Image.png"
path_NISR_logo <- "./www/NISR_log.png"

#Preparing data for Risk assessment tab and logistic model development
Women_under20_2020
#Bringing variable of district to our dataset.
glimpse(Women_under20_2020)
#Creating new logic variables
Women_under20_2020$v217<-ifelse(Women_under20_2020$v217<=6,1,0)#here 1 represent the one who have knowledge about to her ovulatory cycle
Women_under20_2020
Women_under20_2020$v301<-ifelse(Women_under20_2020$v301>=3,1,0) #The same with previous 
Women_under20_2020
Women_under20_2020 <- Women_under20_2020 %>%
  dplyr::mutate(v152 = case_when(
      between(v152,0,29) ~ 1,
      between(v152,30,39) ~ 2,
      between(v152,40,49) ~ 3,
      between(v152,50,59) ~4,
      v152 >= 60 ~ 5 ))#Grouping the age of the head of household
Women_under20_2020$v136<-ifelse(Women_under20_2020$v136<=4,1,ifelse(Women_under20_2020$v136>=5&Women_under20_2020$v136<8,2,3)) #Grouping the size of household

logit_m<-glm(EverPregnant~v025+v106+v119+v130+v136+v151+v152+v170+v171a+v190+v217+v301+v481+v714,
             data = Women_under20_2020,family = binomial(link = "logit"))
logit_m1<-summary(logit_m)
logit_m1
####then we are going to find new model which consist those significance variables
new_logit<-glm(EverPregnant~v106+v136+v152+v190+v714,data =Women_under20_2020,binomial(link = "logit"))
Logistic_mod<-summary(new_logit)
Logistic_mod
new_data<-data.frame(v106=0,
                     v136=1,
                     v152=1,
                     v190=1,
                     v714=1)
prediction<-predict(new_logit,new_data,type="response")
prediction  


#Shiny App

ui11 <- bs4DashPage(
  help = NULL,
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$div(
        class = "custom",
        "The Vanguard Team"
      )
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("bar-chart")),
      menuItem("Risk Assessment", tabName = "assessment", icon = icon("calculator")),
      menuItem("Forecasting Next DHS", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Insights & Recommendations", tabName = "recommendations", icon = icon("lightbulb"))
    )
  ),
  footer = dashboardFooter(
    right = "The Vanguard Team",
    left = "NISR Big Data Hackathon 2024"
  ),
  body = dashboardBody(
    tabItems(
      # Home tab content
      tabItem(
        tabName = "home",
        useShinyjs(),
        bs4Dash::box(
          title = "TACKLING THE CHALLENGE OF TEENAGE PREGNANCY USING DATA SCIENCE!",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          collapsible = FALSE,
          tagList(
            HTML("
  <div style='background-color: #092CA0; color: #F8F9FC; padding: 20px; border-radius: 8px;'>
    <p style='font-size: 1.2em; line-height: 1.5;'>
      Teenage pregnancy is the main problem facing young people in Rwanda, 
      with <strong>33,423 </strong> girls getting pregnant in 2022 according to the data from United Nations Population Fund (UNFPA). 
      Teenage pregnancy affects not only teen mothers but also their children, families, 
      and the country as a whole.
    </p>
    <a href='https://www.aljazeera.com/features/2023/8/25/in-rwanda-teenage-pregnancies-are-rising-the-cost-is-heavy-analysts-say' 
       target='_blank' 
       style='display: inline-block; margin: 10px 0; padding: 10px 15px; background-color: #007bff; color: white; text-decoration: none; border-radius: 5px; font-weight: bold;'>
      Read the story
    </a>
    <p style='font-size: 1em; margin-top: 15px;'>
      This is a collection of stories from affected teen mothers in Rwanda.
    </p>
  </div>
")
            
          )
        ),
        shiny::fluidRow(
          bs4Dash::box(
            title = tags$div(
              tags$div("The Vanguard Team", style = "font-size: 26px; font-weight: bold; text-align: center;"),
              tags$div(icon("users"), style = "text-align: center; font-size: 24px; margin-top: 5px;")
            ),
            width = 4,
            solidHeader = TRUE,
            collapsible = FALSE,
            status = NULL,
            headerBorder = FALSE,
            HTML("<div style='text-align: center; font-size: 18px'>
          <p>Bringing data science solutions to the world's most challenging problems!</p>
        </div>"),
            style = "border: 1px solid #570631; border-radius: 8px; background-color: #f8fafc;", 
            headerTag = tags$div(
              style = "background-color: #6a1b9a; color: white; padding: 10px; text-align: center; border-radius: 8px 8px 0 0;",
              tags$div("The Vanguard Team", style = "font-size: 26px; font-weight: bold;"),
              tags$div(icon("users"), style = "font-size: 24px; margin-top: 5px;")
            )
          )
          ,
          bs4Dash::box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            HTML("
  <blockquote style='font-size: 16px; color: black; font-family: Arial, sans-serif; line-height: 1.5; margin-left: 20px; border-left: 14px solid #007bff; padding-left: 10px; background-color: #f8f9fc;'>
    Data science has helped to solve the most challenging problems. <b>Our objective</b> is to use data science techniques to analyze the teenage pregnancy challenge in Rwanda and identify girls at high risk.
  </blockquote>
")
          )
        ),
        shiny::fluidRow(
          bs4Dash::box(
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "purple",
            title = tags$div(
              "About Our Team",
              tags$i(style = "font-size: 18px; margin-left: 10px;"),
              class = "box-title"
            ),
            shiny::fluidRow(
              bs4Dash::box(
                title = "TUYIZERE Audace",
                width = 4,
                status = "primary",
                solidHeader = TRUE,
                HTML("I am a final year student in Economic-Statistics at the University of Rwanda, I am passionate about Data Science, banking, and leadership.<br>
                  <a href='http://www.linkedin.com/in/tuyizere-audace-b59509251' target='_blank' style='color: #0e76a8; font-weight: bold;'>Connect on LinkedIn</a>")
              ),
              bs4Dash::box(
                title = "AKIMANA Sand",
                width = 4,
                status = "success",
                solidHeader = TRUE,
                HTML("I am a final year student in Economic-Statistics at the University of Rwanda, I am passionate about public health, and youth advocacy.<br>
                  <a href='https://www.linkedin.com/in/akimana-sand-458504289' target='_blank' style='color: #0e76a8; font-weight: bold;'>Connect on LinkedIn</a>")
              ),
              bs4Dash::box(
                title = "NIYOGISUBIZO Fabrice",
                width = 4,
                status = "primary",
                solidHeader = TRUE,
                HTML("I am a final year student in Economic-Statistics at the University of Rwanda, I am passionate about financial analysis and Data science.<br>
                  <a href='https://www.linkedin.com/in/niyogisubizo-fabrice-66a826284' target='_blank' style='color: #0e76a8; font-weight: bold;'>Connect on LinkedIn</a>")
              )
            )
          )
        )
      ),
      
      # Dashboard tab content
      tabItem(
        tabName = "dashboard",
        shiny::fluidRow(
          bs4Dash::box(
            title = "TRENDS IN TEENAGE PREGNANCY RATE IN RWANDA OVER THE YEARS DHS CARRIED OUT",
            width = 12,
            status = "olive",
            solideHeader = TRUE,
            collapsible = FALSE,
            highchartOutput("V0")
          )
        ),
        
        shiny::fluidRow(
          sortable(
            width = 4,
            jqui_resizable(
              bs4Dash::box(
                title = "How many Teenage Girls interviewed in DHS?",
                width = 12,
                status = "olive",
                collapsible = TRUE,
                highchartOutput("V1")
              )
            ),
            bs4Dash::box(
              title = "How is teenage pregnancy in neighbouring countries?",
              width = 12,
              status = "olive",
              collapsible = TRUE,
              collapsed = TRUE,
              highchartOutput("V22")
            )
          ),
          
          sortable(
            width = 8,
            bs4Dash::box(
              title = "How is teenage pregnancy across districts in Rwanda?",
              width = 12,
              status = "olive",
              collapsible = FALSE,
              maximizable = TRUE,
              selectInput(
                inputId = "dhs_year",
                label = "Select DHS Year:",
                choices = c(2010, 2020),
                selected = 2020
              ),
              plotlyOutput("V3")
            ),
            bs4Dash::box(width = 12,
                title = "What are the top 5 Districts with high Teenage pregnancy rate?",
                status = "olive",
                collapsible = TRUE,
                collapsed = TRUE,
                selectInput(
                  inputId = "dhs_year_table",
                  label = "Most of districts in top five are in Eastern province.",
                  choices = c(2010, 2020),
                  selected = 2020
                ),
                tableOutput("Top5_Districts")
            )
          )
        ),
        
        shiny::fluidRow(
          bs4Dash::box(
            title = NULL,
            width = 12,
            solidHeader = TRUE,
            status = "olive",
            collapsible = FALSE,
            style = "border: none; box-shadow: none;",
            HTML("<p style='font-size:18px; font-color: #112A46; text-align:center;'>Rwanda has made progress in fighting against teenage pregnancy compared to other countries in East Africa, but the journey is still long. <b>No Child should give Birth.</b> But who are the girls likely to be pregnant? And where should more effort be allocated?</p>")
          )
        ),
        
        shiny::fluidRow(
          bs4Dash::box(
            title = "Which region has a high Teenage pregnancy? (Urban vs Rural)", width = 4,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "purple",
            selectInput(
              inputId = "dhs_year_region",
              label = "Select DHS Year:",
              choices = c(2020, 2010),
              selected = 2020
            ),
            highchartOutput("region_pie_chart")
          ),
          bs4Dash::box(
            title = "How does teenage pregnancy vary by the Wealth-index?", width = 4,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "purple",
            selectInput(
              inputId = "dhs_year_wealth",
              label = "Selectt DHS Year",
              choices = c(2020, 2010),
              selected = 2020),
            highchartOutput("wealth_bar_chart")
          ),
          bs4Dash::box(
            title = "How does teenage pregnancy vary by Religion?", width = 4,
            collapsible = TRUE,
            collapsed = TRUE,
            status = "purple",
            selectInput(
              inputId = "dhs_year_religion",
              label = "Select DHS Year:",
              choices = c(2020, 2010),
              selected = 2020
            ),
            highchartOutput("religion_bar_chart")
          )
        )
      ),
      # Assessment tab content
      tabItem(
        tabName = "assessment",
        title = tags$h2("ASSESSING THE RISK OF GETTING PREGNANT BASING ON SOCIAL-ECONOMIC FACTORS", style = "text-align: center; color: royal blue;"),
        sidebarLayout(
          sidebarPanel(
            numericInput("v106", "v106 (Highest Education Level)", value = 0, min = 0, max = 3),
            numericInput("v136", "v136 (Size of Household)", value = 1, min = 1, max = 3),
            numericInput("v152", "v152 (Head of Household's Age)", value = 1, min = 1, max = 5),
            numericInput("v190", "v190 (Wealth Index)", value = 1, min = 1, max = 5),
            numericInput("v714", "v714 (Currently Working)", value = 0, min = 0, max = 1),
            actionButton("predict_btn", "Predict Probability of Pregnancy", 
                         style = "color: white; background-color: #4169e1; border-color: purple;")
          ),
          
          mainPanel(
            tags$h3("Assessment Result", style = "color: purple; font-weight: bold;"),
            textOutput("prediction_result"),
            bs4Dash::box(title = "Description of Variables/Factors", width = 12, status = "primary", collapsible = TRUE, collapsed = FALSE,
                imageOutput("variable_description_image")),
           bs4Dash::box(title = "SUMMARY OF LOGISTIC MODEL USED", width = 12, status = "purple", collapsible = TRUE, collapsed = TRUE,
                imageOutput("model_summary_image"))
            
          )
        ), #MINI Dashboard
        bs4Dash::box(
          title = "MINI DASHBOARD SHOWS EFFECT OF EACH VARIABLE IN OUR MODEL",
          width = 12,
          status = "primary",
          collapsible = TRUE,
          collapsed = FALSE,
          solidHeader = FALSE,
          # Nested boxes
          shiny::fluidRow(
            box(
              title = "Education Level Effect on Teenage Pregnancy Probability",
              width = 6,
              plotOutput("Education")
            ),
            box(
              title = "Effect of Household Size on Teenage Pregnancy Probability",
              width = 6,
              plotOutput("House_member")
            )),
          shiny::fluidRow(
            box(
              title = "Effect of Head of Household's Age on Teenage Pregnancy Probability",
              width = 6,
              plotOutput("household_age")
            ),
            box(
              title = "Effect of Family's Wealth-index on Teenage Pregnancy Probability",
              width = 6,
              plotOutput("wealth")
            ))
        )
      ),
      # Recommendations tab content
      tabItem(
        tabName = "recommendations",
        bs4Dash::box(
          width = 12,
          collapsible = FALSE,
          title = tags$h3("KEY INSIGHTS"),
          status = "purple",
          solidHeader = FALSE,
          HTML("
  <div style='color: #020303; background-color: #00aeef; font-size: 18px; padding: 10px; border-radius: 5px;'>
    <p>From the analysis, we see that teenage pregnancy is a challenge facing Rwandan youth and is associated with various social-economic factors. Basing on the analysis we carried out, here are the takeaways:</p>
    <ul>
      <li>2015 recorded the highest teenage pregnancy rates.</li>
      <li>In 2020, numbers have reduced but according to the UNFPA, teenage pregnancy has again increased as the effect of COVID-19.</li>
      <li>Rwanda has the least number of teenage pregnancy rates in neibouring countries east African countries but there is still a way to go.</li>
      <li>Teenage pregnancy is more prevalent in rural areas than urban areas.</li>
      <li>Girls from households where head of households are less than 30 years old, are at high risk and this suggest that mostly head of household are not parents of those teens.</li>
      <li>Girls from middle households in terms of wealth are at a highest risk, even more than those from poor households.</li>
      <li>Education level plays a significant role in reducing the risk of teenage pregnancy.</li>
    </ul>
    <p>These findings highlight the need for targeted interventions to address this issue effectively.</p>
  </div>
")
        ),
        shiny::fluidRow(
          bs4Dash::box(
            title = NULL,
            width = 12,
            solidHeader = TRUE,
            status = NULL,
            collapsible = FALSE,
            style = "border: none; box-shadow: none;",
            HTML("<p style='font-size:18px; color: #112A46; text-align:center;'>
          Girls need advice and trustable information about their sexual health. 
          We have created a guide on sexual and reproductive health and life skills to inform girls with customized assistance. 
          <a href='https://niyogisubizo.shinyapps.io/OUMEDBRG/' style='color: #0e76a8; font-weight: bold;'>Our Guide on Sexual Education</a>
        </p>")
          )
        ),
        bs4Dash::box(
          width = 12,
          collapsible = FALSE,
          title = tags$h3("OUR RECOMMENDATIONS"),
          status = "primary",
          solidHeader = FALSE,
          HTML("
  <div style='color: #F8F9FC; background-color: #092CA0; font-size: 18px; padding: 10px; border-radius: 5px;'>
    <p>After analyzing the challenge of teenage pregnancy and creating a risk assessement tool which shows how significant factors affect teenage pregnancy, We recommend the following:</p>
    <ul>
      <li>Women empowerment should starts from empowering young girls in their teenage.</li>
      <li>In Eastern provence as the where most of top 5 districts having high teenage pregnancy rate should put more efforts in fighting this challenge.</li>
      <li>They should be special effort put in rural areas as girls there are likely to get pregnant than in urban areas.</li>
      <li>Faith based organizations like Churches has to help government on this fight as communities have number of girls getting pregnant.</li>
      <li>Droupout of school has significant effect on teenage pregnancy. Therefore, zero dropout should be the goal especially for girls.</li>
      <li>We recommand more effort in preventive solutions to this challenge because it long term effects on lives of affected girls.</li>
      <li>They should be more effort in ensuring that all girls have access to sexual and reproductive healt services.</li>
      <li>we recommend collaboration between institutions in getting data, for example Rwanda Biomedical center(RBC) can give data to NISR to build a rich data base of monthly or at least annualy data which can be used in forecasting.</li>
    </ul>
    <p>During this project we had a challenge of finding the dataset specifically for teenage pregnancy, we highly recommend a across-country research on this challenge which will have bigger sample size than DHS.</p>
  </div>
") 
        ),
        shiny::fluidRow(
          bs4Dash::box(
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = tags$h3("DATA SOURCE & PROJECT'S DOCUMENT"),
            status = "purple",
            shiny::fluidRow(
              bs4Dash::box(
                width = 6,
                title = tags$h3("Source of data we used"),
                status = "success",
                collapsible = FALSE,
                HTML("
<ul>
  <li>
    We have used different dataset of Demographic Health Survey (DHS) of 2005 to 2020, you can get them from a website of the National Institute of Statistics of Rwanda (NISR) on Microdata using this link.<br>
    <a href='https://microdata.statistics.gov.rw/index.php/catalog' target='_blank' style='color: #0e76a8; font-weight: bold;'>Get the dataset from NISR</a>
  </li>
  <li>
    To deepen our study, we have also used data from United Nations Population Fund (UNFPA) to compare countries with Rwanda, you can use this link to get them from UNFPA's website.<br>
    <a href='https://www.unfpa.org/data' target='_blank' style='color: #0e76a8; font-weight: bold;'>Get the dataset from UNFPA</a>
  </li>
  <li>
  You can get the code for this app through this GitHub link
  <a href='https://github.com/Audace23/The_Vanguard' target='_bank' style='color: #0e76a8; font-weight: bold;'>    Go to our repository</a>
</ul>")
                
              ),
              bs4Dash::box(
                width = 6,
                title = tags$h3("Downloadable pdf report of our project"),
                status = "success",
                collapsible = FALSE,
                downloadButton("downloadReport","Download the PDF report")
              ))
          ))
      ), #UI part for Forecast tab
      tabItem(
        tabName = "forecast",
        fluidPage(
          tags$div(
            style = "background-color: #f3f3f3; padding: 10px; border-radius: 5px;",
            tags$h1(
              "FORECASTING THE RATE OF TEENAGE PREGNANCY FOR NEXT DHS",
              style = "color: #062A73; text-align: center;"
            )
          ),
          sidebarLayout(
            sidebarPanel(
              actionButton("forecast_btn", "Generate Forecast for 2025 & 2030")
            ),
            mainPanel(
              plotOutput("forecast_plot")
            )
          ),
          shiny::fluidRow(
            box(
              title = "RATE FROM TIME SERIES MODEL",
              status = "primary",
              width = 12,
              tableOutput("Forecast_Data")
            )
          )
        )
      )
    )
  )
)


server11 <- function(input, output, session) {
  output$V1 <- renderHighchart({
    highchart(type = "chart") %>%
      highcharter::hc_add_series(Percentage_of_teenage$Perentage, type = "column", name = "Percentage", color= "#062A73") %>%
      highcharter::hc_add_series(Percentage_of_teenage$Perentage, type = "line", name = "Percentage", color= "#740629") %>%
      highcharter::hc_xAxis(categories = Percentage_of_teenage$DHS) %>%
      highcharter::hc_yAxis(min = 0, max = 50, title = list(text = "Percentage of teenage girls")) %>%
      highcharter::hc_subtitle(text = "Percentage of girls of age 15-19 over DHS years")
  })
  output$V0 <- renderHighchart({
    highchart() %>%
      hc_chart(type = "line") %>% 
      hc_add_series(
        data = EverPregnant_DataFrame,
        type = "line",
        hcaes(x = DHS, y = Pregnancy_Rate)
      ) %>%
      hc_xAxis(title = list(text = "DHS Years")) %>%
      hc_yAxis(min = 0, max = 15,title = list(text = "Rate of Teenage Pregnancy"))
  })
  
  output$V22 <- renderHighchart({
    highchart() %>%
      hc_chart(type = "treemap") %>%
      hc_add_series(
        data = LatestData_EAC_TreeMap,
        layoutAlgorithm = "sliceAndDice",
        name = "Category Treemap"
      )
  })
  
  output$V3 <- renderPlotly({
    filtered_data <- Pregnancy_per_District %>%
      dplyr::filter(DHS == input$dhs_year)
    
    map_plot <- ggplot(data = filtered_data) +
      geom_sf(aes(geometry = geometry, fill = Percentage, text = paste("District:", District, "<br>Percentage:", Percentage))) +
      scale_fill_viridis_c() +
      theme_minimal()
    
    ggplotly(map_plot, tooltip = "text")
  })
  
  output$Top5_Districts <- renderTable({
    selected_year <- input$dhs_year_table
    Top5_Districts %>% dplyr::filter(DHS == selected_year)
  })
  
  output$religion_bar_chart <- renderHighchart({
    filtered_data_religion <- Religion %>%
      dplyr::filter(DHS == input$dhs_year_religion)
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = paste("Religion Distribution - DHS", input$dhs_year_religion)) %>%
      hc_xAxis(categories = filtered_data_religion$Religion) %>%
      hc_yAxis(title = list(text = "Percentage")) %>%
      hc_series(list(name = "Percentage", data = filtered_data_religion$Percentage, color = "#092CA0")) %>%
      hc_tooltip(pointFormat = '<b>{point.y:.1f}%</b>')
  })
  
  output$region_pie_chart <- renderHighchart({
    filtered_data_region <- TypeOfResidence %>%
      dplyr::filter(DHS == input$dhs_year_region)
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = paste("Region Distribution - DHS", input$dhs_year_region)) %>%
      hc_series(list(
        name = "Percentage",
        colorByPoint = TRUE,
        data = list_parse(data.frame(name = filtered_data_region$Region, y = filtered_data_region$Percentage)),
        colors = c("#092CA0", "#570631")
      )) %>%
      hc_tooltip(pointFormat = '<b>{point.y:.1f}%</b>')
  })
  
  output$wealth_bar_chart <- renderHighchart({
    filtered_data_wealth <- WealthIndex %>%
      dplyr::filter(DHS == input$dhs_year_wealth)
    
   highcharter::highchart() %>%
     highcharter::hc_chart(type = "bar") %>%
     highcharter::hc_title(text = paste("Wealth Index Distribution - DHS", input$dhs_year_wealth)) %>%
     highcharter::hc_xAxis(categories = filtered_data_wealth$Wealth_Index) %>%
     highcharter::hc_yAxis(title = list(text = "Percentage")) %>%
     highcharter::hc_series(list(name = "Percentage", data = filtered_data_wealth$Percentage, color = "#570631")) %>%
     highcharter::hc_tooltip(pointFormat = '<b>{point.y:.1f}%</b>')
  })
  #MINI Dashboard in Risk tab
  output$Education<- renderPlot({
    ggplot(newdata2,aes(x = v106,y = Probability,fill = as.factor(v106)))+
      geom_bar(stat = "identity",position = "dodge")+
      labs(
        x="Education Level",y="Probability of Pregnancy(%)",
        fill="Education Level"
      )+
      scale_fill_manual(
        values = c("#092ca0","#1f77b4","#509e2f","#f3ea00"),
        labels=c("No Education(0)","Primary(1)","Secondary(2)","University(3)")
      )+
      theme_minimal()
  })
  output$House_member<- renderPlot({
    newdata2$v136<- as.factor(newdata2$v136)
    ggplot(newdata2,aes(x=v136,y=Probability,fill=v136))+
      geom_bar(stat = "identity",position = "dodge")+
      labs(x="Member Level",y="Probability of Pregnancy(%)",fill="Member Level")+
      scale_fill_manual(
        values = c("#092ca0","#509E2F","#00aeef"),
        labels=c("1(0-4)","2(5-8)","3(9+)")
      )+
      theme_minimal()
  })
  output$household_age<-renderPlot({
    # Convert v152 to factor if not already
    newdata2$v152 <- factor(newdata2$v152, levels = 1:5)
    
    ggplot(newdata2, aes(x = v152, y = Probability, fill = v152)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = "Age of Household head Categories",
        y = "Probability of Getting Pregnancy (%)"
      ) +
      scale_fill_manual(
        values = c("#092ca0", "#2ca02c", "#1f77b4", "#f3ea00", "#800635"),
        labels = c("1(0-29)", "2(30-39)", "3(40-49)", "4(50-59)", "5(60+)")
      ) +
      theme_minimal()
  })
  output$wealth<- renderPlot({
    newdata2$v190<- as.factor(newdata2$v190)
    ggplot(newdata2,aes(x=v190,y=Probability,fill=v190))+
      geom_bar(stat = "Identity",position = "dodge")+
      labs(
        x="Wealth Index",y="Probability of Pregnancy(%)"
      )+
      scale_fill_manual(
        values=c("#092ca0","#2ca02c","#00aeef","#f3ea00","#800635"),
        labels=c("Poorest(1)","Poorer(2)","Middle(3)","Richer(4)","Richest(5)")
      )+
      theme_minimal()
  })
  # Server part for forecast tab
  # Reactive function to generate the forecast
  forecast_data <- reactive({
    # Load the data
    Teenager_preg <- Teenager_preg
    
    # Create time series
    ts_data <- ts(Teenager_preg$Pregnancy_Rate, start = min(Teenager_preg$DHS), frequency = 5)
    
    # ETS model and forecasting
    ets_model <- ets(ts_data)
    forecast_ets <- forecast(ets_model, h = 2)
    
    # Forecast data frame
    forecast_df <- data.frame(
      ds = as.Date(c("2025-01-01", "2030-01-01")),
      yhat = as.numeric(forecast_ets$mean),
      yhat_lower = as.numeric(forecast_ets$lower[, 2]),
      yhat_upper = as.numeric(forecast_ets$upper[, 2])
    )
    
    # Original data frame
    original_df <- data.frame(
      ds = as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01")),
      y = as.numeric(ts_data)
    )
    
    # Combine historical and forecast data
    combined_df <- bind_rows(
      original_df %>% mutate(type = "Historical"),
      forecast_df %>% mutate(type = "Forecast", y = NA)
    ) %>% mutate(y_combined = coalesce(y, yhat))
    
    list(combined_df = combined_df, forecast_df = forecast_df)
  })
   # Render plot
  output$forecast_plot <- renderPlot({
    req(forecast_data())
    
    data <- forecast_data()$combined_df
    forecast_df <- forecast_data()$forecast_df
    
    ggplot(data, aes(x = ds, y = y_combined, color = type)) +
      geom_line(linewidth = 1.2) +
      geom_ribbon(data = forecast_df, aes(ymin = yhat_lower, ymax = yhat_upper, x = ds),
                  fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
      labs(
        title = "Forecasting Teenage Pregnancy Rate (2025 & 2030)",
        x = "Year", 
        y = "Pregnancy Rate"
      ) +
      scale_color_manual(values = c("Historical" = "blue", "Forecast" = "cyan3")) +
      scale_x_date(breaks = as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2025-01-01", "2030-01-01")),
                   date_labels = "%Y") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),       # Increase title size
        axis.title.x = element_text(size = 14),                    # Increase x-axis label size
        axis.title.y = element_text(size = 14),                    # Increase y-axis label size
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Increase x-axis text size & angle
        axis.text.y = element_text(size = 12),                     # Increase y-axis text size
        legend.position = "right"
      )
    })
  output$Forecast_Data<- renderTable({
    Forecast_Data
  })
  
  # Server part for assessment risk tab
  observeEvent(input$predict_btn, {
    new_data <- data.frame(v106 = input$v106, v136 = input$v136, v152 = input$v152, v190 = input$v190, v714 = input$v714)
    probability <- predict(new_logit, new_data, type = "response")
    
    output$prediction_result <- renderText({
      paste("Risk of Getting Pregnant:", round(probability, 3) * 100, "%")
    })
    
    output$model_summary_image <- renderImage({
      list(src = path_summary, contentType = "image/png", width = 500, height = 300)
    }, deleteFile = FALSE)
  })
  
  output$variable_description_image <- renderImage({
    list(src = path_description, contentType = "image/png", width = 500, height = 300)
  }, deleteFile = FALSE)
}
# Run the application 
shinyApp(ui= ui11, server= server11)

