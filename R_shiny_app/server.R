# Define server logic
data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")

server <- function(input, output) {

  # Load the dataset
  data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")

  # Render user table
  output$userTable <- renderDataTable({
    datatable(data)
  })
  output$membership_plot1 <- renderPlot({
    # Filter the data based on user input and convert Membership.Type to a factor
    filtered_data <- data %>%
      filter(Membership.Type %in% input$Membership) %>%
      mutate(Membership.Type = factor(Membership.Type, levels = c("Bronze", "Silver", "Gold")))
    
    # Create the density plot
    ggplot(filtered_data, aes(x = Total.Spend, fill = Membership.Type)) + 
      geom_density(alpha = 0.5) + 
      labs(title = "Total Spend by Membership Status") +
      scale_fill_manual(values = c("Bronze" = "#cd7f32", 
                                   "Silver" = "#c0c0c0", 
                                   "Gold" = "#ffd700")) +
      theme_minimal()  # Optional: add a minimal theme for better aesthetics
  })
  

  # Membership satisfaction plot
  output$Membership_plot <- renderPlot({
    # Filter the data based on selected gender(s)
    filtered_data <- data[data$Gender %in% input$Gender, ] 
    
    # Summarize counts by membership type and satisfaction level
    satisfaction_counts <- filtered_data %>%
      group_by(Membership.Type, Satisfaction.Level) %>%
      summarize(Count = n(), .groups = 'drop')
    
    # Convert Membership.Type to a factor with specified order
    satisfaction_counts$Membership.Type <- factor(satisfaction_counts$Membership.Type, 
                                                  levels = c("Bronze", "Silver", "Gold"))
    
    # Create the bar plot
    ggplot(satisfaction_counts, aes(x = Membership.Type, y = Count, fill = Satisfaction.Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Satisfaction Level by Membership Type",
           x = "Membership Type",
           y = "Count of Customers") +
      scale_fill_manual(values = c("Unsatisfied" = "#F7C6C7", 
                                   "Neutral" = "#FCEBAE", 
                                   "Satisfied" = "#B2E0D7")) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })

  # Gender Total Spend by city plot
  output$spend_plot <- renderPlot({
    # Filter data based on selected gender(s)
    filtered_data <- data %>% filter(Gender %in% input$Total_spend)
    
    # Create the bar plot
    ggplot(filtered_data, aes(x = City, y = Total.Spend, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +  # Place bars side by side
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Gender Total Spend by City",
           x = "City",
           y = "Total Spend") +
      scale_fill_brewer(palette = "blue")  # Choose a color palette
  })

  # Last purchase vs Total Spend plot
  output$Items_plot <- renderPlot({
    # Filter the data based on selected cities
    filtered_data <- data %>% filter(City %in% input$Items_purchased)
    
    # Create the box plot
    ggplot(filtered_data, aes(x = City, y = Items.Purchased, fill = City)) +
      geom_boxplot(show.legend = FALSE) +  # Hide the legend
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Items Purchased by City", 
           x = "City", 
           y = "Items Purchased") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Discount plot
  output$Discount_plot <- renderPlot({
    # Filter data based on selected discounts
    filtered_data <- data %>% filter(Discount.Applied %in% input$Discount)
    
    # Check if filtered data is not empty
    if(nrow(filtered_data) == 0) {
      return(NULL)  # or a message that no data is available
    }
    
    # Summarize data
    summary_data <- filtered_data %>%
      group_by(Membership.Type, Discount.Applied) %>%
      summarise(Total.Spend = sum(Total.Spend, na.rm = TRUE), .groups = 'drop')
    
    summary_data$Membership.Type <- factor(summary_data$Membership.Type, 
                                           levels = c("Bronze", "Silver", "Gold"))
    
    # Create the bar plot
    ggplot(summary_data, aes(x = Membership.Type, y = Total.Spend, fill = Discount.Applied)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Total Spend by Membership and Discount Applied",
           x = "Membership",
           y = "Total Spend") +
      scale_fill_brewer(palette = "Set3") +  # Adjust color palette as needed
      theme_minimal()  # Consider customizing the theme further
  })

  # Average spending by discount applied
  output$Discount_plot2 <- renderPlot({
    summary_data <- data %>%
      filter(Gender%in% input$item_discount ) %>%
      group_by(Discount.Applied) %>%
      summarize(Total.Spend = mean(Total.Spend), .groups = 'drop')
    
    # Plotting
    ggplot(summary_data, aes(x = factor(Discount.Applied), y = Total.Spend, fill = factor(Discount.Applied))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        title = "Average Amount Spent by Discount Applied",
        x = "Discount Applied",
        y = "Average Amount Spent"
      ) +
      scale_fill_brewer(palette = "Pastel1") # Optional: Use a soft color palette
  })

  # Density plot for items purchased by city
  output$city_plot <- renderPlot({
    data %>% filter(City %in% input$Items.Purchased) %>%
    ggplot(aes(x = Items.Purchased, fill = City)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density of Items Purchased by City",
           x = "Items Purchased",
           y = "Density") +
      theme_minimal()
  })

  # Average rating by city and membership type
  output$city_plot2 <- renderPlot({
    # Filter and mutate the data
    filtered_data <- data %>% 
      filter(Membership.Type %in% input$City) %>%
      mutate(Membership.Type = factor(Membership.Type, levels = c("Bronze", "Silver", "Gold")))
    
    # Create the tile plot
    ggplot(filtered_data, aes(x = City, y = Membership.Type, fill = Average.Rating)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("#cd7f32", "#c0c0c0", "#ffd700"), 
                           values = scales::rescale(c(1, 0.5, 0)), 
                           guide = "colorbar") +  # Create a gradient from Bronze to Gold
      labs(title = "Average Rating by City and Membership Type",
           x = "City",
           y = "Membership Type") +
      theme_minimal()
  })
}



# Run the app
# shinyApp(ui = ui, server = server)
# shinyServer(server)