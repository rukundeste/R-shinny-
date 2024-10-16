# Define server logic
data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")

# # #######################################
server <- function(input, output) {
  
  # Load the dataset
  data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")
  
  # Render user table
  output$userTable <- renderDataTable({
    datatable(data)
  })
  
  # Gender satisfaction plot
  output$Gender_plot <- renderPlot({
    filtered_data <- data[data$Gender == input$Gender, ]
    
    # Count satisfaction levels
    satisfaction_counts <- filtered_data %>%
      group_by(Gender, Satisfaction.Level) %>%
      summarize(Count = n(), .groups = 'drop')
    
    ggplot(satisfaction_counts, aes(x = Gender, y = Count, fill = Satisfaction.Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Satisfaction Level by Gender",
           x = "Gender",
           y = "Count of Customers") +
      scale_fill_manual(values = c("Unsatisfied" = "red", "Neutral" = "yellow", "Satisfied" = "green")) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  
  # Membership satisfaction plot
  output$Membership_plot <- renderPlot({
    filtered_data <- data[data$Membership.Type == input$Membership, ]
    
    # Count satisfaction levels by membership type
    satisfaction_counts <- filtered_data %>%
      group_by(Membership.Type, Satisfaction.Level) %>%
      summarize(Count = n(), .groups = 'drop')
    
    ggplot(satisfaction_counts, aes(x = Membership.Type, y = Count, fill = Satisfaction.Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Satisfaction Level by Membership Type",
           x = "Membership Type",
           y = "Count of Customers") +
      scale_fill_manual(values = c("Unsatisfied" = "red", "Neutral" = "yellow", "Satisfied" = "green")) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  # Customer segmentation
  output$Age_plot <- renderPlot({
    filtered_data <- data[data$Age == input$Age, ] 
  customer_summary <- filtered_data %>%
    group_by(Customer.ID) %>%
    summarise(Total.Spend, Age = mean(Age), Gender = first(Gender))
  
  # K-means clustering
  set.seed(42)
  kmeans_result <- kmeans(customer_summary[, c("Total.Spend", "Age")], centers = 3)
  
  customer_summary$Cluster <- as.factor(kmeans_result$cluster)
  
  # Plot customer segments
  ggplot(customer_summary, aes(x = Total.Spend, y = Age, color = Cluster,  )) +
    geom_boxplot(alpha = 0.5) +
    labs(title = "Customer Segmentation", x = "Total Spend", y = "Age") +
    theme_minimal()
  })
  
  # Last purchase vs Total Spend plot
  output$Last_plot <- renderPlot({
    filtered_data <- data[data$Days.Since.Last.Purchase == input$Last_purchased , ] 
    ggplot(filtered_data, aes(x = Days.Since.Last.Purchase, y = Total.Spend)) +
      geom_point() +
      labs(title = "Total Spend Over Days Since Last Purchase",
           x = "Days Since Last Purchase",
           y = "Total Spend") +
      theme_minimal()
  })
  
  # Discount plot
  output$Discount_plot <- renderPlot({
    filtered_data <- data[data$Gender == input$Discount , ]
    customer_summary <- filtered_data %>%
      group_by(Customer.ID) %>%
      summarise(Total.Spend = sum(Total.Spend), Age = mean(Age), Gender = first(Gender), .groups = 'drop')
    
    # K-means clustering
    set.seed(42)
    kmeans_result <- kmeans(customer_summary[, c("Total.Spend", "Age")], centers = 3)
    customer_summary$Cluster <- as.factor(kmeans_result$cluster)
    
    ggplot(customer_summary, aes(x = Total.Spend, y = Age, color = Cluster)) +
      geom_point(size = 3) +
      labs(title = "Customer Segmentation by Total Spend and Age",
           x = "Total Spend",
           y = "Age") +
      theme_minimal()
  })
  
  # Average spending by discount applied
  output$Discount_plot2 <- renderPlot({
    filtered_data <- data[data$Total.Spend == input$item_discount , ]
    summary_data <- filtered_data %>%
      group_by(Discount.Applied) %>%
      summarize(Total.Spend = mean(Total.Spend), .groups = 'drop')
    
    ggplot(summary_data, aes(x = factor(Discount.Applied), y = Total.Spend, fill = factor(Discount.Applied))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Average Amount Spent by Discount Applied",
           x = "Discount Applied",
           y = "Average Amount Spent")
  })
  
  # Density plot for items purchased by city
  output$city_plot <- renderPlot({
    filtered_data <- data[data$City == input$Items.Purchased , ]
    ggplot(filtered_data, aes(x = Items.Purchased, fill = City)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density of Items Purchased by City",
           x = "Items Purchased",
           y = "Density") +
      theme_minimal()
  })
  
  # Average rating by city and membership type
  output$city_plot2 <- renderPlot({
    filtered_data <- data[data$City == input$City  , ]
    ggplot(data, aes(x = City, y = Membership.Type, fill = Average.Rating)) +
      geom_tile() +
      scale_fill_gradient(low = "red", high = "green") +
      labs(title = "Average Rating by City and Membership Type",
           x = "City",
           y = "Membership Type") +
      theme_minimal()
  })
}







# Run the app
# shinyApp(ui = ui, server = server)
# shinyServer(server)