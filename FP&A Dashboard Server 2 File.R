# Server functions for compensation tab
server <- function(input, output) {

custom_df <- reactive({
  process_file_result <- processFile()
  data <- process_file_result$data
  data %>% 
    clean_names()
})

observe({
  compensation_table_df <- custom_df() %>% 
    filter(category == "Compensation Accounts") %>%
    filter(!(grepl("Indirect Cost", description, ignore.case = TRUE))) %>% # remove indirect cost lines from data
    mutate(grant = if_else(is.na(grant), "operating", grant))
  
  compensation_table_analysis <- compensation_table_df %>%
    group_by(account, grant, department_name) %>% 
    summarise(amount = sum(debit_credit)) %>% 
    mutate(id = if_else(account %in% wage_accounts, "pay", ""))
  
  summarize_compensation_accounts <- function(data, wage_accounts, tax_accounts, benefit_accounts) {
    data %>%
      filter(account %in% c(wage_accounts, tax_accounts, benefit_accounts)) %>%
      group_by(department_name, account, grant) %>%
      summarise(amount = sum(amount))
  }
  
  compensation_summary <- summarize_compensation_accounts(compensation_table_analysis, wage_accounts, tax_accounts, benefit_accounts)
  
  # Store compensation result in a reactive value
  output$compensation_result_table <- renderDT({
    
    compensation_result <- compensation_summary %>%
      pivot_wider(names_from = account, values_from = amount, values_fill = 0) %>% 
      datatable(class = 'cell-border stripe')
    
  })
  
  # Add download handler for compensation data
  output$download_compensation <- downloadHandler(
    filename = function() {
      paste("compensation_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      result <- processFile()
      
      compensation_result <- compensation_summary %>%
        pivot_wider(names_from = account, values_from = amount, values_fill = 0)
      
      writexl::write_xlsx(compensation_result, path = file)
    }
  )
  
  # Store compensation result for plotting in a reactive value
  output$compensation_result_plot <- renderPlotly({
    
    # Define a custom order for months starting from July
    custom_month_order <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June")
    
    compensation_plot_df <- custom_df() %>% 
      filter(category == "Compensation Accounts") %>%
      filter(!(grepl("Indirect Cost", description, ignore.case = TRUE))) %>% # remove indirect cost lines from data
      mutate(grant = if_else(is.na(grant), "operating", grant))
    
    compensation_plot_result <- compensation_plot_df %>%
      mutate(entry_month = month(date)) %>% 
      mutate(entry_month = month.abb[entry_month]) %>% 
      mutate(entry_month = factor(entry_month, levels = custom_month_order)) %>% 
      mutate(account_category = if_else(account %in% tax_accounts , "Total Taxes", 
                                        if_else(account %in% benefit_accounts, "Total Benefits", account))) %>% 
      group_by(account_category, entry_month) %>% 
      summarise(account_sum = sum(debit_credit)) %>% 
      ungroup()
    
    plot_ly(data = compensation_plot_result, 
            x = ~entry_month, 
            y = ~account_sum, 
            color = ~account_category,
            type = 'scatter',  # Specify the chart type
            mode = 'lines+markers',  # Show both lines and markers
            text = ~paste("Account: ", account_category, "<br>Month: ", entry_month, "<br>Value: ", scales::dollar(account_sum))) %>%
      layout(title = "Monthly Compensation Summary",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Total Compensation"),
             hovermode = "closest")
    
  })
  
})

}