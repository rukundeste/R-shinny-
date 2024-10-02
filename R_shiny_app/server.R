# Define server logic
data <- read.csv("E-commerce Customer Behavior - Sheet1.csv")
server <- function(input, output) {
  
  output$plot <- renderPlot({
    req(input$graph_type)  # Ensure the input is available
    print(input$graph_type)
    if (input$graph_type == "Age Distribution") {
      data %>%
        ggplot( aes(x=Age)) +
        geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
        ggtitle("Bin size = 5") +
        theme_ipsum() + labs(title = "Age Distribution", x = "Age", y = "Count")
      
    } else if (input$graph_type == "Gender Distribution by City") {
      ggplot(data, aes(x = City, fill = Gender)) +
        geom_bar(position = "dodge") +theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title = "Gender Distribution by City", x = "City", y = "Count")
      
    } else if (input$graph_type == "Satisfaction Level by Age") {
    
      data %>%
        ggplot( aes(x=Satisfaction.Level, y=Age, fill=Satisfaction.Level)) + 
        geom_violin() +
        xlab("class") +
        theme(legend.position="none") +
        xlab("")
      
    } else if (input$graph_type == "Items Purchased by City") {
      ggplot(data, aes(x = City, y = Items.Purchased, fill = City)) +
        geom_boxplot(show.legend = FALSE) +
        scale_fill_brewer(palette = "Dark2") +
        labs(title = "Items Purchased by City", x = "City", y = "Items Purchased") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$graph_type == "Membership.Type by Average Rating") {
      ggplot(data, aes(x = Membership.Type, y = Average.Rating, fill = Membership.Type)) +
        geom_violin(
          alpha = 0.5,  # Set transparency for the fill
          scale = "area",
          color = "black",  # Outline color of the violins
          size = 1         # Size of the outline
        ) +
        labs(
          title = "Membership Type by Average Rating",
          x = "Membership Type",
          y = "Average Rating"
        ) +
        theme_minimal() +  # Clean theme for better aesthetics
        theme(legend.position = "none")  # Optionally hide the legend
      
    } else if (input$graph_type == "Distribution of Item purchased") {
      ggplot(data, aes(x = Items.Purchased)) +
        geom_freqpoly(
          aes(color = ..count..),  # Color by the count of items purchased
          size = 1,                # Thickness of the lines
          alpha = 0.7              # Transparency (not usually used with lines)
        ) +
        labs(
          title = "Distribution of Item purchased",
          x = "Items Purchased",
          y = "Count"
        ) +
        theme_minimal()
      
    } else if (input$graph_type == "Total Spend by Gender") {
      ggplot(data, aes(x = Average.Rating, y = Total.Spend)) +
        geom_jitter(
          height = 2,                # Adjust the vertical jitter
          width = 2,                 # Adjust the horizontal jitter
          aes(alpha = 0.7, color = Average.Rating, shape = Gender),  # Set transparency, color, and shape based on a variable
          size = 3                   # Size of the points
        ) +
        labs(
          title = "Total Spend by Gender",
          x = "Average Rating",
          y = "Total Spend"
        ) +
        theme_minimal() +          # Clean theme for better aesthetics
        scale_color_gradient(low = "red", high = "blue")  # Optional: Adjust color gradient
      
      
    } else if (input$graph_type == "Distribution of customers by gender") {
      ggplot(data, aes(x = Gender,fill=Gender)) + 
        geom_bar() + 
        labs(title = "Customer Distribution by Gender")
      
    } else if (input$graph_type == "Total Spend by Gender") {
      data %>%
        ggplot( aes(x=Gender, y=Total.Spend, fill=Gender)) +
        geom_boxplot() +
        scale_fill_viridis(discrete = TRUE, alpha=0.6) +
        geom_jitter(color="black", size=0.4, alpha=0.9) +
        theme_ipsum() +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Total Spend by Gender") +
        xlab("")
      
    } else if (input$graph_type == "Gender Total Spend by City") {
      ggplot(data, aes(x = City, y = Total.Spend, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +  # Dodge to place bars side by side
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        labs(title = "Gender Total spend by City",
             x = "City",
             y = "Total spend") +
        scale_fill_brewer(palette = "Set1")
      
    } else if (input$graph_type == "Total Spend Over Time") {
      ggplot(data, aes(x = Days.Since.Last.Purchase, y = Total.Spend, fill= Days.Since.Last.Purchase)) + 
        geom_line() + 
        labs(title = "Total Spend Over Time")
      
    } else if (input$graph_type == "Age vs. Total Spend") {
      ggplot(data, aes(x = Age, y = Total.Spend)) + 
        geom_point() + 
        geom_smooth(method = "lm") + 
        labs(title = "Age vs. Total Spend")
      
    } else if (input$graph_type == "Average Ratings by Item Purchased") {
      ggplot(data, aes(x = Items.Purchased, y =Average.Rating)) + 
        geom_bar(stat = "identity") + 
        labs(title = "Average Ratings by Item Purchased")
      
    } else if (input$graph_type == "Days Since Last Purchase by Gender") {
      ggplot(data, aes(x = Gender, y = Days.Since.Last.Purchase, fill = Gender)) + 
        geom_boxplot() + 
        labs(title = "Days Since Last Purchase by Gender")
    }
      else if (input$graph_type == "Items purchased by membership type") {
        items_by_membership <- data %>%
          group_by(Membership.Type) %>%
          summarise(Total.Items.Purchased = sum(Items.Purchased, na.rm = TRUE))
        
        # Create the bar plot
        ggplot(items_by_membership, aes(x = Membership.Type, y = Total.Items.Purchased, fill = Membership.Type)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Items Purchased by Membership Type", x = "Membership Type", y = "Total Items Purchased")
    }
      else if (input$graph_type == "sales volume by Discount") {
          summary_data <- data %>%
            group_by(Discount.Applied) %>%
            summarize(Total.Spend = mean(Total.Spend))
          ggplot(summary_data, aes(x = factor(Discount.Applied), y = Total.Spend, fill = factor(Discount.Applied))) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = "Average amount spent  by Discount Applied",
                 x = "Discount Applied",
                 y = "Average amount spent")
    }
      else if (input$graph_type == "Discounts Taken by Membership") {
            ggplot(data, aes(x = Membership.Type, fill = Discount.Applied)) + 
              geom_bar(position = "fill") + 
              labs(title = "Discounts Taken by Membership Status")
    }
      else if (input$graph_type == "Satisfaction Level by Gender and Age") {
              ggplot(data, aes(x = Age, y = Satisfaction.Level)) + 
                geom_point() + 
                facet_grid(~ Gender) + 
                labs(title = "Satisfaction Level by Age and Gender")
              
    } else if (input$graph_type == "Total Spend by Membership") {
              ggplot(data, aes(x =Total.Spend , fill = Membership.Type)) + 
                geom_density(alpha = 0.5) + 
                labs(title = "Density of Total Spend by Membership Status")
              
    } 
      else if (input$graph_type == "Customer segmentation") {
         customer_summary <- data %>%
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
    } 
      else if (input$graph_type == "correlation matrix") {
      correlation_matrix <- cor(data[, c("Age", "Total.Spend", "Items.Purchased", "Average.Rating", "Days.Since.Last.Purchase")])
      
      # Create the correlation plot
      corrplot(correlation_matrix, method = "color", 
               title = "Correlation Matrix of Numeric Variables", 
               tl.col = "black", tl.srt = 45, 
               addCoef.col = "grey")
      
      
    } else if (input$graph_type == "Total spent by Age") {
      ggplot(data_summary, aes(x = Age, y = Total_Spend)) +
        geom_line(
          aes(color = Age),          # Color the line based on Age (if desired)
          size = 1.5,                # Size of the line
          alpha = 0.8,               # Transparency of the line
          linetype = "solid"         # Type of line (solid, dashed, etc.)
        ) +
        labs(
          title = "Total Spend by Age",
          x = "Age",
          y = "Total Spend"
        ) +
        theme_minimal() +          # Clean theme for better aesthetics
        scale_color_gradient(low = "red", high = "blue")  # Optional color gradient
      
      
    } else if (input$graph_type == "Density plot of item purchased") {
      ggplot(data, aes(x = Items.Purchased)) +
        geom_density(
          aes(alpha = 0.5, fill = City),  # Fill based on density counts
          color = "black",                      # Outline color
          linetype = "solid",                   # Line type
          size = 1,                             # Size of the line
          adjust = 1,                           # Adjust bandwidth (higher values = smoother)
          kernel = "gaussian"                   # Gaussian kernel for density estimation
        ) +
        labs(
          title = "Density of Item purchased",
          x = "Item purchased",
          y = "Density"
        ) +
        theme_minimal()    
    }
  })
}

# Run the app
# shinyApp(ui = ui, server = server)
# shinyServer(server)