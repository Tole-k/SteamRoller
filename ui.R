library(shiny)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(fresh)
library(shinyWidgets)
shinyUI(dashboardPage(skin = "green",
    dashboardHeader(title = "SteamRoller",tags$li(class = "dropdown",
        tags$img(src = "images/steam.png", height = "50px", width = "150px")
        )),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data Table", tabName = "DataTable", icon = icon("table")),
            menuItem("Value", tabName = "Value", icon = icon("dollar")),
            menuItem("History", tabName = "History", icon = icon("history")),
            menuItem("Hall of Fame", tabName = "HallOfFame", icon = icon("trophy")),
            menuItem("About", tabName = "About", icon = icon("info"))
        )
    ),
    dashboardBody(
        includeCSS("www/custom.css"),
        tabItems(
            tabItem(tabName = "DataTable",
                    h2("Data Table"),
                    fluidRow(
                        box(width = 12,pickerInput(inputId="columns",label="Choose columns for the data table",choices=colnames(data),multiple=TRUE,selected = c("name","release_date","developer","price"),  options = pickerOptions(
                            actionsBox = TRUE, 
                            size = 10,
                            selectedTextFormat = "count > 4"
                        ), )),
                    ),
                    fluidRow(
                        box(width = 12,title = "Table",
                            DT::dataTableOutput("table"))
                    ),
                    fluidRow(box(width = 4, title = "Game Info",htmlOutput("game_info")),
                        box(width = 8, title="Reviews",plotlyOutput("reviews"))
                    )
            ),
            tabItem(tabName = "Value",
                    h2("Recommended Games in terms of value"),
                    fluidRow(
                        box(width = 4, title = "Genre",selectInput(inputId="genre",label="Choose a genre",choices=c(unique(dataD$genres)),selected = "Racing",selectize = T)),
                        box(width = 4, title = "Platform",selectInput(inputId="platform",label="Select your platform",choices=unique(dataD$platforms)),selected = "Windows"),
                        box(width = 4, title = "Unpopular Games",materialSwitch(inputId="filter",label="filter unpopular games",value = TRUE,status = "success"))
                    ),
                    fluidRow(
                        box(title = "Recommended games in terms of dollars per hour",plotlyOutput("Dvalue")),
                        box(title = "Recommended games in terms of reviews",plotlyOutput("Pvalue"))
                    )
            ),
            tabItem(tabName = "History",
                    h2("History of Games"),
                    fluidRow(
                        box(width=6, title = "Game reviews throughout the years", plotOutput("history",click = "history_click")),
                        box(width=6, title = "Best games of the year", DT::dataTableOutput("history_table"))
                    ),
                    fluidRow(
                        box(width=2, title = "Customize graph", prettyCheckboxGroup(inputId="custom",label="Choose one or both:",choices=c("add line","add area"),selected = c("add line","add area"),status = "success")),
                        box(width=10, title = "History of prices", plotlyOutput("prices"))
                    )
            ),
            tabItem(tabName = "HallOfFame",
                    h2("Hall of Fame"),
                    fluidRow(
                        box(width = 6, title = "Time Period", sliderInput(inputId="time",label="Choose time period",min = min(data$release_date),max = max(data$release_date),value = c(min(data$release_date),max(data$release_date)))),
                        box(width = 6, title = "Genre",selectInput(inputId="genrePD",label="Choose genre",choices=c(unique(dataD$genres)),selected = "Racing"))
                    ),
                    fluidRow(
                        box(width=6, title = "Developers", plotlyOutput("hall_of_fame1")),
                        box(width=6, title = "Developers", plotlyOutput("hall_of_fame3"))
                        
                    ),
                    fluidRow(
                        box(width=6, title = "Publishers", plotlyOutput("hall_of_fame2")),
                        box(width=6, title = "Publishers", plotlyOutput("hall_of_fame4"))
                    )
                ),
            tabItem(tabName = "About",
                    h2("About"),
                    p("This is a dashboard for Steam games data. It allows you to explore the data about games released on Steam platform up to 2019."),
                    h3("Data Table"),
                    p("This tab allows you to view the data in a table format. You can select the columns you want to view to explore chosen aspects of the dataset. Additionally select a game from the table to view more information about it."),
                    h3("Value"),
                    p("This tab was designed to help you choose the next game to play. Since it doesn't know your taste it focuses on objective measures such as reviews, price and playtime. You can select the genre and platform and also filter out unpopular games."),
                    h3("History"),
                    p("This tab allows you to view the history of games. You can view the reviews of games throughout the years and also view the best games of the selected year."),
                    h3("Hall of Fame"),
                    p("This tabs purpose is to credit the best developers and publishers in the industry. You can select the time period and genre to see the best of the best. 
                      You can also see all developers and publishers plotted on a scatter plot, on which size of the point represents the number of games they have released."),
                    h4("Created by:"),
                    p("Anatol Kaczmarek and Dawid Siera"),
                    h4("Data Source:"),
                    p("https://www.kaggle.com/nikdavis/steam-store-games")
            )
        )
    )
))
