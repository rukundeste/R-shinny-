library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(ggplot2)
library(hrbrthemes)
library(shinythemes)

# Load your dataset
data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")
# Define UI
ui <- dashboardPage(
  dashboardHeader(title ="E-commerce Customer Behavior Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Dataset Overview", tabName = "overview", icon = icon("th")),
      menuItem("Graphs", tabName = "Graphs", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
                              h2('Introduction',style='text-align:center'),
                              p("E-commerce has revolutionized the way consumers shop, providing unprecedented access
                                to products and services at any time and from anywhere. Understanding customer behavior
                                in this digital landscape is crucial for businesses aiming to enhance their online strategies
                                and improve customer satisfaction.")
),
      # Second tab content
tabItem(tabName = "overview",
        h2("Dataset Overview"),
        p("This dataset provides a comprehensive view of customer behavior within
                                   an e-commerce platform. Each entry in the dataset corresponds to a unique customer,
                                   offering a detailed breakdown of their interactions and transactions.
                                   The information is crafted to facilitate a nuanced analysis of customer preferences,
                                   engagement patterns, and satisfaction levels, aiding businesses in making
                                   data-driven decisions to enhance the customer experience."),
img(src="./Img3D.webp",width="500")
),
tabItem(tabName = "Graphs",
selectInput("graph_type", "Select Graph Type:",
                              choices = c("Age Distribution",
                                          "Gender Distribution by City",
                                          "Satisfaction Level by Age",
                                          "Items Purchased by City",
                                          "Membership.Type by Average Rating",
                                          "Distribution of Item purchased",
                                          "Total Spend by Gender",
                                          "Distribution of customers by gender",
                                          "Total Spend by Gender",
                                          "Gender Total Spend by City",
                                          "Total Spend Over Time",
                                          "Age vs. Total Spend",
                                          "Average Rating by Item Purchased",
                                          "Days Since last Purchased by Gender",
                                          "Item purchased by Membership",
                                          "Sales volume by Discount",
                                          "Discounts Taken by Membership",
                                          "Satisfaction Level by Gender and Age",
                                          "Total Spend by Membership",
                                          "Customer Segmentation",
                                          "Correlation Matrix",
                                          "Total Spend by Age",
                                          "Density of Total Spend by Membership"

                                        )),

plotOutput("plot")

     )
  )
) )
