# Session to provide installation instructions? Check nowcastLSTM procedure on clean computer
# Define UI for application that shows nowcast dashboard
# User selects in sidebar: i, target, scale target, train end date (selectInput, dateInput)
# User clicks run nowcast in sidebar (actionButton)
# Window 1 (visualizations): user selects feature
# Window 2 (predictions): visualize predictions (plot), variable importance (table), download button

# if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
# if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
# if("reticulate" %in% rownames(installed.packages()) == FALSE) {install.packages("reticulate")}
if(T){
 librarian::shelf(here, tidyverse, reticulate, dhopp1/nowcastLSTM, caret, shiny, shinydashboard)
 source(here("afdb_nowcast_load_data.R"))
 source(here("helper_functions.R"))
 nowcastLSTM::initialize_session(paste(miniconda_path(),"/python.exe", sep = "")) # No need to worry about warnings
}

ui <- dashboardPage(
  dashboardHeader(title = "Nowcast Africa dashboard"),
  # Sidebar options
  dashboardSidebar(
    sidebarMenu(
      selectInput("i", "Select country", selected = "ETH", choices = afr_list),
      selectInput("j", "Select benchmark", selected = "AGO", choices = afr_list),
      uiOutput("select_target"),
      actionButton("createBtn", "Generate nowcast"),
      menuItem("Visualizations", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Nowcast", tabName = "nowcast", icon = icon("th"))
    )
  ),
  # Dashboard body
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                fluidRow(
                  box(
                    uiOutput("select_feature"),
                    plotOutput("plot1", height = 300)
                  ),
                  box(
                    sliderInput("cutoff", label = NULL, value = c(as.Date("2015-1-1"), as.Date("2022-1-1")), min = min(data$date), max = max(data$date)),
                    plotOutput("map1", height = 300)
                  )
                ),
                fluidRow(
                  box(
                    tableOutput("table1")
                  ),
                  box(
                    plotOutput("plot2", height = 300)
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "nowcast",
              h2("Nowcast outputs")
      )
    )
  )
)

# Define server logic
# Arguments are created when user selects i or target
# Render plot
# Run nowcast and predictions when user presses button (observeEvent)

server <- function(input, output) {
  # Initialize data based on user input
  
  arguments <- reactive({
    initialize_data(data,
                    country = input$i,
                    target = input$target,
                    train_size = 0.7, 
                    test_end_date = "2019-12-1", 
                    scale_target_tf = T, 
                    explanatory_variables = if(exists("selected_variables")) selected_variables else NA,
                    parameters = fast)
  })
  
  filtered_data <- reactive({
    req(input$i, input$j, input$target, input$cutoff)
    
    i <- input$i
    j <- input$j
    target <- input$target
    date_min <- input$cutoff[1]
    date_max <- input$cutoff[2]
    
    data %>% 
      filter(between(date, date_min, date_max)) %>% 
      select(date, contains(substring(target, 5))) %>% 
      select(!contains("global"))
  })
  output$select_target <- renderUI({
    selectInput('target', 'Select target', selected = "ETH_food_price_yoy", str_subset(names(data), input$i))
  })
  
  output$select_feature <- renderUI({
    req(arguments())
    selectInput('feature', 'Select second variable', selected = "ETH_cpi_yoy", as.list(names(arguments()$test)))
  })
  
  # Country plot for selected i, target and second variable
  output$plot1 <- renderPlot({
    req(input$target, input$feature)
    country_plot(data, target = input$target, feature = input$feature)
  })
  # Density plot for selected target, i, j, and Africa
  output$plot2 <- renderPlot({
    req(filtered_data(), input$i, input$j, input$target)
    i <- input$i
    j <- input$j
    target <- input$target
    
    plot_data <- filtered_data() %>% 
      pivot_longer(-date) %>% 
      mutate(name = str_remove(name,paste0("_",substring(target, 5)))) %>% 
      mutate(group = case_when(
        name == i ~ i,
        name == j ~ j,
        TRUE ~ "Other")) 
      
      plot_data %>% 
        ggplot(aes(value, fill = group)) + 
        geom_density(alpha = 0.6) + 
        scale_x_log10() + 
        theme_void() +
        theme(legend.title=element_blank(), legend.position = c(0.2,0.8)) +
        labs(fill=target)
  })
  # Regional map for selected target and interactive date range
  output$map1 <- renderPlot({
    req(input$target, input$cutoff)
    region_plot(data, target = substring(input$target, 5), cutoff = input$cutoff)
  })
  # Summary table for selected i, j, and Africa
  output$table1 <- renderTable({
    req(input$i, input$j, input$target, input$cutoff, filtered_data())

    i <- input$i
    j <- input$j
    target <- input$target
    date_min <- input$cutoff[1]
    date_max <- input$cutoff[2]
    filtered_data <- filtered_data()
      
    summary_data_i <- summary_stats(filtered_data, target, i)
    summary_data_j <- summary_stats(filtered_data, target, j)
    
    last_obs_afr <- filtered_data %>%
      pivot_longer(-date) %>%
      filter(!is.na(value)) %>%
      summarize(max(date)) %>%
      pull()
    
    first_obs_afr <- filtered_data %>%
      pivot_longer(-date) %>%
      filter(!is.na(value)) %>%
      summarize(min(date)) %>%
      pull()
    
    summary_data_afr <- filtered_data %>% 
      pivot_longer(-date) %>%
      filter(!is.na(value)) %>% 
      summarise(
        first = 0,
        last = 0,
        N = n(),
        mean = mean(value),
        sd = sd(value),
        min = min(value),
        median = median(value),
        max = max(value)
      ) %>% 
      pivot_longer(everything(), values_to = "Africa", names_to = "Stats") %>% 
      mutate(Africa = case_when(
                Stats == "N" ~ as.character(as.integer(Africa)),
                Stats == "last" ~ as.character(last_obs_afr),
                Stats == "first" ~ as.character(first_obs_afr),
                TRUE ~ as.character(round(Africa,1))
              )
      )
    
    summary_data_i %>% 
      bind_cols(summary_data_j[,2]) %>% 
      bind_cols(summary_data_afr[,2])
  })
}

shinyApp(ui, server)
