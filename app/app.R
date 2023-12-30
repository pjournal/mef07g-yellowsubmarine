#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
pti <- c("readxl","dplyr","tidyverse", "ggplot2", "lubridate", "tidyr", "stringi", "hrbrthemes", "viridis", "scales", "knitr","shinyWidgets","DT","RColorBrewer","gridExtra","ggrepel")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
  install.packages(pti)
}


library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringi)
library(hrbrthemes)
library(viridis)
library(scales)
library(knitr)
library(shiny)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(ggridges)
library(knitr)
library(kableExtra)
library(gridExtra) 
library(ggrepel)



loaded_datasets <- readRDS("wits_data.rds")
read_wits_turkey_data_only <- loaded_datasets$wits_turkey_data_only
read_wits_turkey_data_with_partners <- loaded_datasets$wits_turkey_data_with_partners
sapply(read_wits_turkey_data_only, function(x) sum(is.na(x)))
sapply(read_wits_turkey_data_with_partners, function(x) sum(is.na(x)))





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("A Turkey Analysis: Trade Balance and Partners(WITS)"),
  
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # First section with sub-tabs
      tabsetPanel(
        
        # Input for selecting the year using a slider
        sliderInput("selected_year_slider", "Select Year:",
                    min = min(read_wits_turkey_data_only$year),
                    max = max(read_wits_turkey_data_only$year),
                    c(2002, 2020), step = 1, sep = ""),
        
        
        selectInput("selected_country", "Select Country:",
                    choices = sort(unique(read_wits_turkey_data_with_partners$partner_name)),
                    selected = "Germany")
        
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Exports and Imports Rating", plotOutput("exports_imports_plot")),
        tabPanel("Top 10 Exported and Imported Products", plotOutput("top_imported_exported_products")),
        tabPanel("Product Comparison Analysis",
                 selectInput("selected_partners", "Select Partners:",
                             choices = unique(read_wits_turkey_data_with_partners$partner_name),
                             selected = c("Germany","Spain","Iraq"),
                             multiple = TRUE),
                 plotOutput("scatterPlot")
        ),        tabPanel("Exported Products Ranking", dataTableOutput("top_export_products_table")),
        tabPanel("Imported Products Ranking", dataTableOutput("top_import_products_table"))
        
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  #Data selected
  
  selected_country_data <- reactive({
    read_wits_turkey_data_with_partners %>%
      filter(partner_name == input$selected_country & year >= input$selected_year_slider[1] & year <= input$selected_year_slider[2])
    
  })
  
  selected_country_data_2 <- reactive({
    read_wits_turkey_data_with_partners %>%
      filter(
        partner_name %in% input$selected_partners,
        year >= input$selected_year_slider[1],
        year <= input$selected_year_slider[2]
      )
  })
  
  
  #1
  
  country_exports <- reactive({
    selected_country_data() %>%
      group_by(year) %>%
      summarize(country_exports = sum(trade_value_usd_exp))
  })
  
  country_imports <- reactive({
    selected_country_data() %>%
      group_by(year) %>%
      summarize(country_imports = sum(trade_value_usd_imp))
  })
  
  # Plot for exports and imports with the selected country
  output$exports_imports_plot <- renderPlot({
    ggplot() +
      geom_line(data = country_exports(), aes(x = year, y = country_exports, color = "Exports"), size = 1.5) +
      geom_line(data = country_imports(), aes(x = year, y = country_imports, color = "Imports"), size = 1.5) +
      labs(title = paste("Trade Analysis with", input$selected_country), x = "Year", y = "Trade Value (USD)") +
      theme(legend.text = element_text(size = 12),       # Adjust legend text size
            axis.text.x = element_text(size = 12),        # Adjust x-axis text size
            axis.text.y = element_text(size = 12),        # Adjust y-axis text size
            axis.title.x = element_text(size = 14),       # Adjust x-axis title size
            axis.title.y = element_text(size = 14),       # Adjust y-axis title size
            plot.title = element_text(size = 16, face = "bold"))  # Adjust plot title size
  })
  
  
  # Top Products 
  top_exported_products_data <- reactive({
    data <- selected_country_data() %>%
      group_by(section_name) %>%
      summarize(total_export = sum(trade_value_usd_exp)) %>%
      top_n(10)
    
    # Truncate section names to the first 20 characters
    data$truncated_name <- str_trunc(data$section_name, 100)
    
    data
  })
  
  top_imported_products_data <- reactive({
    data <- selected_country_data() %>%
      group_by(section_name) %>%
      summarize(total_import = sum(trade_value_usd_imp)) %>%
      top_n(10)
    
    # Truncate section names to the first 70 characters
    data$truncated_name <- str_trunc(data$section_name, 100)
    
    data
  })
  
  output$top_imported_exported_products <- renderPlot({
    data_export <- top_exported_products_data()
    data_import <- top_imported_products_data()
    
    # Get distinct truncated names for colors
    color_names_export <- unique(data_export$truncated_name)
    color_names_import <- unique(data_import$truncated_name)
    
    # Generate a limited number of distinct colors
    colors_export <- brewer.pal(length(color_names_export), "Set3")
    colors_import <- brewer.pal(length(color_names_import), "Set3")
    
    plot_export <- ggplot(data = data_export, aes(x = reorder(section_name, -total_export), y = total_export, fill = truncated_name)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10 Diversity of Exported Products (", input$selected_country, ", ", paste(input$selected_year_slider, collapse = " - "), ")"), x = "Products", y = "Total Export Value (USD)") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +  # Add horizontal line at y = 0
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
            legend.position = "bottom", legend.text = element_text(size = 10),
            axis.text = element_text(size = 12)) +
      guides(fill = guide_legend(nrow = 10)) +
      scale_fill_manual(values = setNames(colors_export, color_names_export), name = "")
    
    plot_import <- ggplot(data = data_import, aes(x = reorder(section_name, -total_import), y = total_import, fill = truncated_name)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top 10 Diversity of Imported Products (", input$selected_country, ", ", paste(input$selected_year_slider, collapse = " - "), ")"), x = "Products", y = "Total Import Value (USD)") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +  # Add horizontal line at y = 0
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
            legend.position = "bottom", legend.text = element_text(size = 10),
            axis.text = element_text(size = 12)) +
      guides(fill = guide_legend(nrow = 10)) +
      scale_fill_manual(values = setNames(colors_import, color_names_import), name = "")
    
    
    grid.arrange(plot_export, plot_import, ncol = 2)
  })
    
    
    # Imported products
    output$top_import_products_table <- renderDataTable({
    data <- selected_country_data()  # Use selected_country_data() as a function
    
    top_import_products <- data %>%
      group_by(section_name) %>%
      summarize(total_import = sum(trade_value_usd_imp)) %>%
      na.omit()
    
    top_import_products <- top_import_products %>%
      arrange(desc(total_import))
    
    top_import_products$total_import <- scales::number_format(suffix = "M")(top_import_products$total_import / 1e6)
    
    datatable(top_import_products, colnames = c('Section Name', 'Total Import(USD)'), options = list(pageLength = 9))
  })
  
    # Exported products
    output$top_export_products_table <- renderDataTable({
      data <- selected_country_data()  # Use selected_country_data() as a function
      
      top_export_products <- data %>%
        group_by(section_name) %>%
        summarize(total_export = sum(trade_value_usd_exp)) %>%
        na.omit()
      
      top_export_products <- top_export_products %>%
        arrange(desc(total_export))
      
      top_export_products$total_export <- scales::number_format(suffix = "M")(top_export_products$total_export / 1e6)
      
      datatable(top_export_products, colnames = c('Section Name', 'Total Export(USD)'), options = list(pageLength = 9))
    })
  
  
    # Reactive expression for ggplot
    output$scatterPlot <- renderPlot({
      
      # Add a new column for total trade
      trade_data_selected_partners <- selected_country_data_2() %>%
        mutate(total_trade_exp = (trade_value_usd_exp)/19000000,
               total_trade_imp = (trade_value_usd_imp)/19000000)
      
      # Group by section_name and calculate total trade values for each section_name
      total_trade_values_19_year_exp <- trade_data_selected_partners %>%
        group_by(section_name, partner_name) %>%
        summarise(total_exp_value = sum(total_trade_exp))
      
      total_trade_values_19_year_imp <- trade_data_selected_partners %>%
        group_by(section_name, partner_name) %>%
        summarise(total_imp_value = sum(total_trade_imp))
      
      # Filter top 5 section names for exports
      filtered_commodity_data_exp <- total_trade_values_19_year_exp %>%
        group_by(partner_name) %>%
        arrange(desc(total_exp_value)) %>%
        slice_head(n = 6)
      
      # Filter top 5 section names for imports
      filtered_commodity_data_imp <- total_trade_values_19_year_imp %>%
        group_by(partner_name) %>%
        arrange(desc(total_imp_value)) %>%
        slice_head(n = 6)
      
      # Truncate section names to the first 20 characters
      filtered_commodity_data_exp$truncated_name <- str_trunc(filtered_commodity_data_exp$section_name, 60)
      filtered_commodity_data_imp$truncated_name <- str_trunc(filtered_commodity_data_imp$section_name, 60)
      
      # Set up the color palette for exports
      colors_exp <- brewer.pal(length(unique(filtered_commodity_data_exp$truncated_name)), "Set3")
      unique_names_exp <- unique(filtered_commodity_data_exp$truncated_name)
      color_mapping_exp <- setNames(colors_exp[1:length(unique_names_exp)], unique_names_exp)
      
      # Set up the color palette for imports
      colors_imp <- brewer.pal(length(unique(filtered_commodity_data_imp$truncated_name)), "Set3")
      unique_names_imp <- unique(filtered_commodity_data_imp$truncated_name)
      color_mapping_imp <- setNames(colors_imp[1:length(unique_names_imp)], unique_names_imp)
      
      # Use the color_mapping in ggplot for exports
      pro_export <- ggplot(filtered_commodity_data_exp, aes(x = partner_name, y = total_exp_value, fill = truncated_name)) +
        geom_col() +
        scale_fill_manual(values = color_mapping_exp[unique(filtered_commodity_data_exp$truncated_name)]) +  # Set manual color scale
        labs(title = paste("Average Annual Export Breakdown by Product Category and Partner (", "Country", ", ", paste(input$selected_year_slider, collapse = " - "), ")"),
             x = "Partner Name", y = "Total Export mn USD",
             fill = NULL) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +  # Add horizontal line at y = 0
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(nrow = 10))
      
      # Use the color_mapping in ggplot for imports
      pro_import <- ggplot(filtered_commodity_data_imp, aes(x = partner_name, y = total_imp_value, fill = truncated_name)) +
        geom_col() +
        scale_fill_manual(values = color_mapping_imp[unique(filtered_commodity_data_imp$truncated_name)]) +  # Set manual color scale
        labs(title = paste("Average Annual Import Breakdown by Product Category and Partner (", "Country", ", ", paste(input$selected_year_slider, collapse = " - "), ")"),
             x = "Partner Name", y = "Total Import mn USD",
             fill = NULL) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +  # Add horizontal line at y = 0
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(nrow = 10))
      
      # Arrange plots side by side
      grid.arrange(pro_export, pro_import, ncol = 2)
    })
    
    
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)