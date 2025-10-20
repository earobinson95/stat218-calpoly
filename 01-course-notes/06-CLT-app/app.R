library(shiny)
library(tidyverse)
library(gganimate)
library(transformr)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Exploring the Distribution of the Sampling Mean"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num_samples", "Number of Samples:", value = 1),
      actionButton("sample_button", "Draw Samples")
    ),
    
    mainPanel(
      tabsetPanel(
        # Plots Tab
        tabPanel("Plots",
                 plotOutput("population_plot", height = "300px"),
                 conditionalPanel(
                   condition = "input.num_samples == 1",
                   plotOutput("sample_plot", height = "150px")
                 ),
                 plotOutput("sample_mean_plot", height = "200px")
        ),
        
        # Scenario Tab
        tabPanel("Scenario",
                 h3("Example 6.1: Time Perception Impaired by Nicotine Withdrawal"),
                 p("A study conducted by researchers at Pennsylvania State University investigated whether time perception, a simple indication of a person's ability to concentrate, is impaired during nicotine withdrawal."),
                 p("The study results were presented in the paper ", 
                   em("Smoking Abstinence Impairs Time Estimation Accuracy in Cigarette Smokers"), 
                   "(Klein, Corwin, and Stine 2003)."),
                 p("After a 24-hour smoking abstinence, 20 daily smokers were asked to estimate how much time had passed during a 45-second period."),
                 p("The resulting data on perceived elapsed time (in seconds) were summarized and visualized as shown below. These results are artificial but are similar to the actual findings.")
        ),
        
        # Activity Tab
        tabPanel("Activity",
                 h3("Exploring the Distribution of the Sampling Mean"),
                 
                 # Step 1
                 h4("Step 1: Population Parameters"),
                 p("In this simulation study, what are the following parameters for your population?"),
                 textInput("mu", "Population Mean (μ):", value = "45"),
                 numericInput("sigma", "Population Standard Deviation (σ):", value = 7.3),
                 textInput("step1_answer", "Where does this value of 45 seconds come from?", ""),
                 
                 # Step 2
                 h4("Step 2: Repeated Samples"),
                 p("In practice, we usually don’t know the true population mean (μ). Researchers often take random samples to estimate this mean. In our case, we’ll simulate multiple random samples of 20 subjects."),
                 p("Sample 1: Click 'Draw Samples'. Record the sample mean and sample standard deviation:"),
                 textOutput("sample1_mean"),
                 textOutput("sample1_sd"),
                 
                 p("Sample 2: Click 'Draw Samples' again. Record the new values:"),
                 textOutput("sample2_mean"),
                 textOutput("sample2_sd"),
                 
                 p("Sample 3: Click 'Draw Samples' once more. Record:"),
                 textOutput("sample3_mean"),
                 textOutput("sample3_sd"),
                 
                 textAreaInput("comparison", "How do the means and standard deviations from these three samples differ?", ""),
                 
                 # Step 3
                 h4("Step 3: Distribution of Sample Means"),
                 p("Each of these sample means is a single point from the distribution of sample means. Visualize the distribution of sample means."),
                 p("Add a dot on the plot for each sample mean from the three samples above."),
                 numericInput("distribution_mean", "Distribution Mean", value = NA),
                 numericInput("distribution_sd", "Distribution SD", value = NA),
                 
                 # Step 4
                 h4("Step 4: Hypothesis Testing"),
                 p("The researchers hypothesized that the mean perceived elapsed time for smokers during nicotine withdrawal was greater than 45 seconds."),
                 p("Null Hypothesis (H0): The mean perceived elapsed time for all smokers is equal to 45 seconds."),
                 p("Alternative Hypothesis (HA): The mean perceived elapsed time for all smokers is greater than 45 seconds."),
                 textInput("p_value", "Estimate the p-value based on the simulation", ""),
                 
                 # Step 6
                 h4("Step 6: Understanding the Central Limit Theorem (CLT)"),
                 p("The Central Limit Theorem (CLT) allows us to approximate the distribution of sample means as normal, provided the sample size is large enough."),
                 numericInput("se_calc", "Calculate the Standard Error (SE)", value = NA),
                 p("Check the shape: Does the 'Distribution of Sample Means' look approximately normal?"),
                 textAreaInput("clt_shape", "Answer:", "")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    sample_means = tibble(mean = numeric())
                       )
  
  # Create reactive values to store the population and sample means
  population_data <- reactive({
    
    tibble(x = rnorm(1000, 
                     mean = 45, 
                     sd = 7.3
                     )
           )
    
  })
  
  
  # Plot population distribution
  output$population_plot <- renderPlot({
    
    population_data <- population_data()
    
    ggplot(data = population_data,
           mapping = aes(x = x)) +
      geom_dotplot(method = "histodot",
                   dotsize = 0.4,
                   binwidth = 1
                   
                   ) +
      # geom_vline(xintercept = input$pop_mean, 
      #            color = "steelblue", 
      #            size = 1
      #            ) +
      geom_text(aes(label = paste0("Mean: ", round(mean(population_data$x), 0), 
                               "\nsd: ", round(sd(population_data$x), 2)
                               )
                   ),
               x = 55,
               y = Inf,
                hjust = -0.5,
                vjust = 4,
                color = "steelblue",
                size = 6
      ) +
      labs(title = "Population: Distribution of 'all' smokers suffering from nicotine withdrawal",
           x = "Perceived Elapsed Time (seconds)",
           y = "") +
      scale_x_continuous(limits = c(0,80), breaks = seq(0,80,5)) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    
  })
  
  observeEvent(input$sample_button, {
    
    population_data <- population_data()
    
    sample_data <- tibble(sample_rep = 1:input$num_samples) |>
          mutate(sample_x = map(sample_rep, ~ sample(population_data$x, 
                                                     size = 20
                                                     )
                                )
                 ) |> 
          unnest(sample_x)

    
    output$sample_plot <- renderPlot({
    
      ggplot(data = sample_data,
           mapping = aes(x = sample_x)) +
      geom_dotplot(method = "histodot",
                   dotsize = 0.4,
                   binwidth = 1
                   ) +
      geom_vline(aes(xintercept = mean(sample_x)), 
                 color = "steelblue",
                 size = 1
                 ) +
      geom_text(data = sample_data |> 
                  group_by(sample_rep) |> 
                  summarize(mean_value = mean(sample_x),
                            sd_value = sd(sample_x)
                            ),
                aes(label = paste0("Mean: ", round(mean_value, 1),
                              "\nsd: ", round(sd_value, 1)
                              ),
                    x = 60, 
                    y = 0.75
                    ),
                hjust = 0,
                vjust = 1,
                color = "steelblue",
                size = 4
      ) +
      # facet_wrap(~sample_rep, ncol = 1) +
      labs(title = paste0("Sample: Distribution of n = ", 20, " Smokers"),
           x = "Perceived Elapsed Time (seconds)",
           y = "") +
      scale_x_continuous(limits = c(0,80), breaks = seq(0,80,5)) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
      
    })

  new_means <- sample_data |> 
      group_by(sample_rep) |> 
      summarize(mean = mean(sample_x)
                )
    
  rv$sample_means <- rv$sample_means |> 
      bind_rows(new_means)
  
  output$sample_mean_plot <- renderPlot({
    
    ggplot(data = rv$sample_means,
           mapping = aes(x = mean)) +
      geom_dotplot(method = "histodot",
                   dotsize = 0.4,
                   binwidth = 1
      ) +
      geom_vline(aes(xintercept = mean(mean)), 
                 color = "steelblue",
                 size = 1
      ) +
      annotate("text",
               label = paste0("Mean: ", round(mean(rv$sample_means$mean), 0),
                              "\nsd: ", round(sd(rv$sample_means$mean), 1)
               ),
               x = 60, 
               y = Inf,
               hjust = 0,
               vjust = 3,
               color = "steelblue",
               size = 6
      ) +
      labs(title = "Distribution of Sample Means",
           x = "Mean Perceived Elapsed Time (seconds)",
           y = "") +
      scale_x_continuous(limits = c(0,80), breaks = seq(0,80,5)) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
      )
    
  })
  
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)






# Create animation of samples falling into the distribution of sample means
# anim_plot <- ggplot(sample_data, aes(x = sample_means, y = sample_id)) +
#   geom_point(size = 5, color = "orange") +
#   labs(title = "Animating Sampling Process", x = "Sample Mean", y = "") +
#   transition_time(sample_id) +
#   ease_aes('linear') +
#   shadow_wake(wake_length = 0.1, alpha = FALSE) +
#   enter_fade() +
#   exit_fade()

# Save the animation
# anim <- animate(anim_plot, duration = input$speed, fps = 10, width = 800, height = 400)

# Output the animation
# output$animated_sample_plot <- renderImage({
#   anim_file <- tempfile(fileext = ".gif")
#   anim_save(anim_file, animation = anim)
#   list(src = anim_file, contentType = 'image/gif')
# }, deleteFile = TRUE)
