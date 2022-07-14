library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(Rmisc)
require(scales)
require(stringr)

house_types=c("Single-family Detached" = "1Fam",
  "Two-family Coversion" = "2fmCon", 
  "Duplex" = "Duplex", 
  "Townhouse Inside Unit" = "Twnhs", 
  "Townhouse End Unit" = "TwnhsE")

ui <- dashboardPage(
  dashboardHeader(title = "House Price in Ames"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("map-o")),
    menuItem("Year", tabName = "year", icon = icon("line-chart")),
    menuItem("HouseType", tabName = "type",icon = icon("bar-chart")),
    menuItem("Correlation", tabName = "corr", icon = icon("area-chart")),
    menuItem("Data", tabName = "table", icon = icon("book"))
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "overview",
      h1("Introduction"),
      p("Homeowners who always want to maximize the value of their homes, but they usually overspend on remodeling and don't get a return on their investment when they sell. People hope to buy a property, and to get the greatest house available within their budget. This project's purpose is to address some of these issues, specifically:"),
      p("● What is the predicted sale price of a house given a set of features?"),
      p("● What features homeowners can add to increase the value of their home?"),
      p("● What is the expected sale price of a house in a set of features ?"),
      p("● What kind of house would one be able to afford on a budget?"),
      br(),
      h3("This video..."),
      HTML('<iframe width="50%" height="300" 
                  src="https://www.youtube.com/embed/1wb63eN8jqs" 
                  frameborder="0" allowfullscreen></iframe>'),
      br(),
      h3("Group Members"),
      fluidRow(column(3,
                      p("Yexuan Chu")
              ),
               column(3,
                      p("Zijia Meng")
                ),
               column(3,
                      p("yi Zhu")
                      ),
               column(3,
                      p("Zhitong Zhou")
                      )
      )
    ),
    tabItem(tabName = "year",
            sliderInput("year", "Year:", min = 1879, max = 2010, value = 1, 
                        step = 5, animate = animationOptions(interval = 2000, loop = FALSE)),
            plotOutput("plotyear")
    ),
    tabItem(
      tabName = "type",
      h2 = "Sales Price For Different Building Type",
      fluidRow(
        box(title = "Histogram",
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plottype", width = 800, height = 500)),
        
        box(title = "Type",
            width = 3,
            checkboxGroupInput("Type", label = "House Type",choices = house_types,
                               selected = c("1Fam",  "2fmCon",  "Duplex", "Twnhs", "TwnhsE"))),
        tabBox(
          width = 10,
          tabPanel("1Fam", 
                   "A single detached dwelling contains only one dwelling unit 
                     and is completely separated by open space on all sides from 
                     any other structure, except its own garage or shed.",br(),br(),
                   img(src="SHD.jpg", height=180, width=220
                   )
                   ),
          tabPanel("2Conv", 
                   "Converted Dwelling means a building originally 
                     constructed as a Single Detached Dwelling in which the number
                     of Dwelling Units has been or may be lawfully increased to 
                     a maximum of two Dwelling Units, provided one of the Dwelling
                     Units is located wholly or partly above the other or located
                     wholly behind the other, but does not include a Semi-Detached
                     Dwelling or a Duplex;",br(),br(),
                   img(src="2Fam.jpg", height=180, width=220
                   )),
          tabPanel("Duplex",
                   "A duplex is a house which has been divided into 
                     two separate units for two different families or groups of people.",
                   br(),br(),
                   img(src="Duplex.jpg", height=180, width=220
                   )),
          tabPanel("Twnhs", 
                   "Townhome Unit means an individual residential dwelling unit
                     that (i) shares one or more common walls with another residential 
                     dwelling unit, (ii) is physically attached to the land underneath the unit,
                     and (iii) the fee simple land underneath the unit is or will 
                     be conveyed with each such unit. Inside Units means the ones are not at the end",
                   br(),br(),
                   img(src="TH.jpg", height=180, width=220
                   )),
          tabPanel("TwnhsE",
                   "Townhome Unit means an individual residential dwelling unit
                     that (i) shares one or more common walls with another residential 
                     dwelling unit, (ii) is physically attached to the land underneath the unit,
                     and (iii) the fee simple land underneath the unit is or will 
                     be conveyed with each such unit. End Units means the ones that are at the end",
                   br(),br(),
                   img(src="ThE.jpg", height=180, width=220
                   ))
        ))
    ),
    tabItem(
      tabName = "corr",
      h2("Corrs"),
      fluidRow(column(3,
                      selectInput("selectf", "Select a feature",
                                  c("YearBuilt", "YearRemodAdd", "FullBath", "GrLivArea", "TotRmsAbvGrd", "TotalBsmtSF",
                                    "X1stFlrSF",  "GarageCars", "GarageArea", "OverallQual"
                                  ))
               ),
               column(5,plotOutput("plotchoice")),
               column(4,plotOutput("plotcorr"))
      )
    ),
    tabItem(tabName = "table", dataTableOutput("myTable"))
  ))
)

server <- function(input, output, session) {
  train = read.csv('train.csv')
  test = read.csv('test.csv')
  test$Id = NULL
  train$Id = NULL
  test$SalePrice = NA
  all = rbind(train, test)
  Abb = unique(all$BldgType)
  Words = c("Single-family Detached", "Two-family Coversion", "Duplex",
            "Townhouse End Unit", "Townhouse Inside Unit")
  all$words = all$BldgType
  for (i in 1:length(Abb)) {
    all$words = str_replace(all$words, Abb[i], words[i])
  }
  
  df1 = all %>%
    select(BldgType, SalePrice, YearBuilt) %>%
    na.omit()
  
  #plots
  output$plotyear = renderPlot({ 
    
    all %>%
      filter(!is.na(SalePrice))%>%
      filter(YearBuilt==input$year)%>%
      ggplot(aes(x=SalePrice)) +
      geom_histogram(fill="orange", binwidth = 10000) +
      scale_x_continuous(breaks= seq(0, 800000, by=100000))
    
  })
  
  output$plottype = renderPlot({
    df1 %>%
      filter(BldgType %in% input$Type) %>%
      ggplot(aes(SalePrice, fill = BldgType))+
      geom_histogram(position = position_stack(reverse = TRUE),
                     binwidth = 15000) +
      ggtitle("Histogram of SalePrice") +
      ylab("Count") +
      xlab("Housing Price") +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8),
        legend.background = element_rect(
          size = 0.5,
          linetype = "solid",
          colour ="black"
        )
      )
  })
  
  output$plotcorr = renderPlot({
    num_features = which(sapply(all, is.numeric))
    num_names = names(num_features)
    
    all_num_features = all[, num_features]
    
    corr = cor(all_num_features, use="pairwise.complete.obs")
    
    corr_sorted = as.matrix(sort(corr[,'SalePrice'], decreasing = TRUE))
    Corr_high = names(which(apply(corr_sorted, 1, function(x) abs(x)>0.5)))
    corr = corr[Corr_high, Corr_high]
    corrplot.mixed(corr, tl.pos = "lt",lower = 'shade', upper = 'pie', order = 'hclust') +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.8,hjust = 0.5)) +
      theme(legend.position = "NULL")
  })
  
  output$plotchoice = renderPlot({
    all %>%
      ggplot(mapping = aes_string(x=input$selectf, y="SalePrice")) +
      geom_point()
    
  })
  
  output$myTable = renderDataTable({
    return(datatable(all, rownames = FALSE,options = list(scrollX = TRUE)))
  })
  
}


shinyApp(ui = ui, server = server)
