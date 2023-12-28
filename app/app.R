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
        tabPanel("Revenue Growth Analysis", plotOutput("scatterPlot")),
        tabPanel("Exported Products Ranking", dataTableOutput("top_export_products_table")),
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
      # Replace the ggplot code here with modifications for Shiny
      top_sections_export <- selected_country_data() %>%
        group_by(section_name) %>%
        summarize(total_export = sum(trade_value_usd_exp)) %>%
        arrange(desc(total_export)) %>%
        head(10)
      
      selected_data_top10_export <- selected_country_data() %>%
        filter(section_name %in% top_sections_export$section_name) %>%
        group_by(section_name) %>%
        mutate(
          partner_count = n_distinct(partner_name),
          cagr = ifelse(
            n_distinct(year) > 1, 
            ((trade_value_usd_exp[which(year == max(year))] / trade_value_usd_exp[which(year == min(year))])^(1/(max(year) - min(year) + 1)) - 1) * 100, 
            NA
          ),
          total_trade_value_exp_2020 = sum(trade_value_usd_exp) / 1000000
        ) %>%
        ungroup()
      
      selected_data_top10_export$truncated_name <- str_trunc(selected_data_top10_export$section_name, 35)
      
      gg_export <- ggplot(selected_data_top10_export, aes(x = section_name, y = total_trade_value_exp_2020, size = cagr, color = truncated_name)) +
        geom_point(alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_size_continuous(range = c(3, 15)) +
        labs(title = paste("Export Partner and Revenue Growth Analysis for", input$selected_country),
             y = "Total Export Revenue (mn USD )",
             size = "CAGR",
             color = "Section Name") +
        theme(
          axis.text.x = element_blank(), axis.title.x = element_blank(),  # Hide x-axis labels
          legend.position = "right",
          legend.box = "horizontal",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
        )+
        scale_x_discrete(expand = c(0.5, 0.5)) +  # Adjust the expand values as needed
        scale_y_continuous(expand = c(0.1, 0.1))   
      
      # Repeat the process for import
      top_sections_import <- selected_country_data() %>%
        group_by(section_name) %>%
        summarize(total_import = sum(trade_value_usd_imp)) %>%
        arrange(desc(total_import)) %>%
        head(10)
      
      selected_data_top10_import <- selected_country_data() %>%
        filter(section_name %in% top_sections_import$section_name) %>%
        group_by(section_name) %>%
        mutate(
          partner_count = n_distinct(partner_name),
          cagr = ifelse(
            n_distinct(year) > 1, 
            ((trade_value_usd_imp[which(year == max(year))] / trade_value_usd_imp[which(year == min(year))])^(1/(max(year) - min(year) + 1)) - 1) * 100, 
            NA
          ),
          total_trade_value_imp_2020 = sum(trade_value_usd_imp) / 1000000
        ) %>%
        ungroup()
      
      selected_data_top10_import$truncated_name <- str_trunc(selected_data_top10_import$section_name, 35)
      
      gg_import <- ggplot(selected_data_top10_import, aes(x = section_name, y = total_trade_value_imp_2020, size = cagr, color = truncated_name)) +
        geom_point(alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_size_continuous(range = c(3, 15)) +
        labs(title = paste("Import Partner and Revenue Growth Analysis for", input$selected_country),
             y = "Total Import Revenue (mn USD )",
             size = "CAGR",
             color = "Section Name") +
        theme(
          axis.text.x = element_blank(), axis.title.x = element_blank(),  # Hide x-axis labels
          legend.position = "right",
          legend.box = "horizontal",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
        )+
        scale_x_discrete(expand = c(0.5, 0.5)) +  # Adjust the expand values as needed
        scale_y_continuous(expand = c(0.1, 0.1))   
      
      # Arrange plots vertically with export on top
      grid.arrange(gg_export, gg_import, ncol = 2, heights = c(0.7, 0.3))
    })
    
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)