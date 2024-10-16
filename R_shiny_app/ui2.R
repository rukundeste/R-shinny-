library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(ggplot2)
library(hrbrthemes)
library(shinythemes)
library(dplyr)

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)

# Load your dataset
data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")

ui <- dashboardPage(
  skin = "green",
  
  # SECTION 1: Header
  dashboardHeader(title = "E-commerce Customer Behavior Analysis", titleWidth = 450),
  
  # SECTION 2: Sidebar
  dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
    menuItem("Dataset Overview", tabName = "overview", icon = icon("database")),
    menuItem("Satisfaction Analysis", tabName = "satisfaction", icon = icon("bar-chart")),
    menuItem("Customer Segmentation", tabName = "Segmentation", icon = icon("users")), 
    menuItem("Discount Strategies", tabName = "Discount_applied_analysis", icon = icon("tags")),
    menuItem("City-based Insights", tabName = "City_insights", icon = icon("city")),
    menuItem("About", tabName = "About", icon = icon("user"))
  )),
  
  # SECTION 3: Body
  dashboardBody(tabItems(
    tabItem(tabName = "intro",
            h2('Introduction', style = 'text-align:center'),
            p("E-commerce has revolutionized the way consumers shop, 
              providing unprecedented access to products and services at any time and from anywhere.
              Understanding customer behavior in this digital landscape is crucial 
              for businesses aiming to enhance their online strategies and improve customer satisfaction."),
            img(src = "./Img3D.webp", width = "640")
    ),
    tabItem(tabName = "overview",
            h2("Dataset Description", style = 'text-align:center'),
            p("This dataset provides a comprehensive view of customer behavior within an e-commerce platform.
              Each entry in the dataset corresponds to a unique customer,
              offering a detailed breakdown of their interactions and transactions. 
              The information is crafted to facilitate a nuanced analysis of customer preferences,
              engagement patterns, and satisfaction levels, 
              aiding businesses in making data-driven decisions to enhance the customer experience."),
            img(src = "./Img3D.webp", width = "640"),
            dataTableOutput('userTable')
    ),
    tabItem(tabName = "satisfaction",
            selectInput(inputId = "Gender", 
                        label = "Gender:",
                        choices = unique(data$Gender),
                        selected = unique(data$Gender)[1]  # Use the first unique value as default
            ),
            selectInput(inputId = "Membership", 
                        label = "Membership Type:",
                        choices = unique(data$Membership.Type),
                        selected = unique(data$Membership.Type)[1]  # Use the first unique value as default
            ),
            plotOutput("Gender_plot"),
            plotOutput("Membership_plot")
    ),
    tabItem(tabName = "Segmentation",
            selectInput(inputId = "Age", 
                        label = "Age:",
                        choices = unique(data$Age),
                        selected = unique(data$Age)[1]  # Use the first unique value as default
            ),
            selectInput(inputId = "Last_purchased",  # Changed ID to be unique
                        label = "Last_Purchased:",
                        choices = unique(data$Days.Since.Last.Purchase  ),
                        selected = unique(data$Days.Since.Last.Purchase )[1]  # Use the first unique value as default
            ),
            plotOutput("Age_plot"),
            plotOutput("Last_plot")
    ),
    tabItem(tabName = "Discount_applied_analysis",
            selectInput(inputId = "Discount", 
                        label = "Items Purchased:",
                        choices = unique(data$Items.Purchased),
                        selected = unique(data$Items.Purchased)[1]  # Use the first unique value as default
            ),
            selectInput(inputId = "item_discount", 
                        label = "Gender:",
                        choices = unique(data$Gender),
                        selected = unique(data$Gender)[1]  # Use the first unique value as default
            ),
            plotOutput("Discount_plot"),
            plotOutput("Discount_plot2")
    ),
    tabItem(tabName = "City_insights",
            selectInput(inputId = "Items.Purchased",
                        label = "City:",
                        choices = unique(data$City ),
                        selected = unique(data$City)[1]  # Use the first unique value as default
            ),
            selectInput(inputId = "City",  # Changed ID to be unique
                        label = "Memeship:",
                        choices = unique(data$Membership.Type ),
                        selected = unique(data$Membership.Type)[1]  # Use the first unique value as default
            ),
            plotOutput("city_plot"),
            plotOutput("city_plot2")
    ),
    
    tabItem(tabName = "About",
            h2("About Me"),
            p("Hello everyone, I am Oreste RUKUNDO and  currently a student at New York City Data Science Academy(NYCDSA).
            This is my R shiny app for E-Commerce Customer Behavior Analysis. It is a user-friendly interface, interactive data visualization and rapid prototyping. 
Thank You!!!
"),
            img(src = "https://hbcuconnect.com/photos/alumni/2258045.gif",width="200")
    )
  ))
)

