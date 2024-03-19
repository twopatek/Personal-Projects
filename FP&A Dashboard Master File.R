# Load necessary libraries
pacman::p_load(shiny, tidyverse, lubridate, purrr, janitor, openxlsx, shinythemes, writexl, plotly, DT, readxl)

# setwd("C:/Users/MatthewAdams/Eduserve Solutions/FPATeam - Documents/Matthew Adams/CSUSA Projects/General Ledger Demo/FPA Dashboard Files")

# Create Vector String to remove rows with "Total"
rows_to_remove <- c("Total")

# Create Vector to Categorize accounts
building_repairs_maintenance_accounts <- c("63120","63140","63160","63180","63200","63220",
                                           "63240","63260","63280","63281","63300","63320",
                                           "63340","63360","63380","63400","63420","63440",
                                           "63460","63470","63480","63520","36540","63560",
                                           "63580","63720")

compensation_accounts <- c("60020","60040","60060","60080","60100","60120",
                           "60140","60420","60440","60480","60520","60640",
                           "60660","60680","60690")
management_fee_accounts <- c("61520","61530","61540","61560","61580","61620")

instructional_accounts <- c("65030","65010","65020","65240","65000","65041",
                            "65060","65100","65040")
utility_accounts <- c("64720","64740","64760","64780","64910","64920",
                      "64930","64940","64990","64980")

wage_accounts <- c("60020 - Regular Wages-Exempt", "60040 - Regular Wages-Non-Exempt",
                   "60060 - Overtime", "60080 - Bonus", "60100 - 10 Month Accrual", "60120 - Stipend")

tax_accounts <- c("60640 - FICA", "60660 - FUTA", "60680 - SUTA", "60690 - 10 Month Accrual Payroll Taxes")

benefit_accounts <- c("60140 - Sick Day Buy Out","60420 - General Benefits", "60440 - 401(k) Match", 
                      "60480 - Employee insurance", "60520 - Workers Compensation")

theme = shinytheme("cerulean")

# Load payroll date
raw_payroll_data <- read_excel("03.15.2024 HRIS_Employee Breakdown - Mar.xlsx") %>% suppressWarnings()

# Process payroll
payroll_condensed <- raw_payroll_data %>% 
  clean_names() %>% 
  select(location_name, job_title, legal_name, grant_indicator_description, annual_rate, cost_code, cost_code_description) %>% 
  filter(!(cost_code %in% c("104N00", "105N00", "106N00")))

# Create list of all schools
school_list <- unique(payroll_condensed$location_name)

# Define UI for application
ui <- fillPage(
  padding = 80,
  theme = theme,
  titlePanel("FP&A Dashboard"),
  tabsetPanel(
    tabPanel("General Ledger Automation",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 fileInput("upload", "Upload General Ledger File"),
                 downloadButton("download_ledger", "Download Data"),
                 br(),
                 p("Select the desired general ledger from Netsuite and click 'Expand All Rows'. Download and save as Microsoft Excel Workbook file type."),
                 p("Using the 'Browse' button, select your saved ledger, and wait for the data to appear on the right. Click download to finish.")
               ),
               mainPanel(
                 fluidRow(
                   column(
                     width = 12,
                     style = "max-height: 700px; max-width: 2000px; overflow-x: auto;",
                     DTOutput("ledger_table")
                   ),
                   verbatimTextOutput("error_message")
                 )
               )
             )
    ),
    tabPanel("Payroll Automation",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("payroll_school", "Select School:", choices = school_list, selected = first(school_list)),
                 downloadButton("download_filtered_payroll", "Download Filtered Payroll"),
                 p("This tab automates the payroll process by filtering the data by school, condensing it, and then creating a pivot."),
                 p("This dashboard is using the latest payroll file, dated - 03.15.2024")
               ),
               mainPanel(
                 fluidRow(
                   column(
                     width = 12,
                     style = "max-height: 700px; max-width: 2000px; overflow-x: auto;",
                     DTOutput("filtered_payroll_table")
                   )
                 )
               )
             )
             
    ),
    tabPanel("Compensation Summary",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 downloadButton("download_compensation", "Download Data"),
                 br(),
                 p("The table on the right displays YTD compensation by department and account.")
               ),
               mainPanel(
                 fluidRow(
                   column(
                     width = 12,
                     style = "max-height: 700px; max-width: 2000px; overflow-x: auto;",
                     DTOutput("compensation_result_table")
                   )
                 )
               )
             )
    ),
    tabPanel("Monthly Compensation",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 br(),
                 p("The plot on the right displays the values booked in each compensation account for each month. You can add/remove lines from the plot by selecting them in the legend.")
               ),
               mainPanel(
                 plotlyOutput("compensation_result_plot",  height = "750px", width = "1500px")
               )
             )
    )
  )
)
    



# Define server logic required to process the uploaded file
server <- function(input, output) {
  
  # Set Maximum File Upload Size, Set to 100 MB
  options(shiny.maxRequestSize=100*1024^2)
  
  # Function to read the uploaded file and store it as an object
  processFile <- reactive({
    req(input$upload)
    
    withProgress(
      message = 'Processing data...',
      detail = 'This may take a moment...',
      value = 0.2,
      {
        inFile <- input$upload
        if (!grepl("\\.xlsx$", inFile$name, ignore.case = TRUE)) {
          output$error_message <- renderText("Unsupported file type. Please upload an Excel (.xlsx) file.")
          return(NULL)
        } else {
          output$error_message <- renderText(NULL)
        }
        
        data <- openxlsx::read.xlsx(inFile$datapath)
        
        csv <- data %>% slice(-(1:3))
        
        colnames(csv) <- unlist(csv[1, ])
        
        csv <- csv[-1, ]
        
        df <- csv %>% 
          clean_names() %>% 
          select(-balance) %>%
          filter(!grepl(paste(rows_to_remove, collapse = "|"), account)) %>% 
          fill(account) %>% 
          mutate(test = if_else(is.na(type),"0","1")) %>% 
          relocate(test, .after = type) %>% 
          filter(test != "0") %>% 
          select(-test) %>% 
          mutate(account_number = str_sub(account,1,5)) %>% 
          relocate(account_number, .before = account) %>% 
          filter(account_number > 40000) %>% 
          mutate(indicator = str_sub(account_number,1,1)) %>% 
          relocate(indicator, .before = account_number) %>% 
          mutate(credit = as.numeric(credit),
                 credit2 = if_else(indicator == "4" & is.na(credit),-as.numeric(debit), credit)) %>% 
          mutate(debit = as.numeric(debit),
                 debit2 = if_else(indicator != "4" & is.na(debit),-as.numeric(credit), debit)) %>%
          mutate(debit_credit = round(if_else(indicator == "4",credit2,
                                              if_else(indicator != "4",debit2,credit2)), 2)) %>%
          select(-debit2, -credit2) %>% 
          relocate(debit_credit, .before = debit) %>% 
          mutate(debit = round(debit, 2),
                 credit = round(credit, 2)) %>%
          mutate(account_title = substring(account, 8)) %>% 
          relocate(account_title, .before = account) %>% 
          mutate(date = as.numeric(date), 
                 date = as.Date(date, origin = "1899-12-30")) %>% 
          mutate(category = if_else(account_number %in% building_repairs_maintenance_accounts,"Building R&M",
                                    if_else(account_number %in% compensation_accounts, "Compensation Accounts",
                                            ifelse(account_number %in% management_fee_accounts, "Management Fees",
                                                   if_else(account_number %in% instructional_accounts, "Instructional Accounts",
                                                           if_else(account_number %in% utility_accounts, "Utility Accounts",
                                                                   if_else(indicator == "4","Revenue Accounts",""))))))) %>% 
          relocate(category, .before = account_number) %>% 
          select(-1,-3,-4, -11, -12, -17:-20) %>% 
          select(-contains("date_created")) %>% 
          rename(vendor = name)
        
        label <- data %>% select(1) %>% slice(1:3)
        
        schoolname <- label %>% slice(1)
        schoolname <- sub(".*:", "", schoolname)
        schoolname <- trimws(schoolname) %>% tibble()
        
        period_range <- label %>% slice(3)
        
        source <- "Netsuite"
        source <- tibble(source)
        
        merge1 <- merge(schoolname, period_range)
        
        ledger_label <- merge(merge1, source) %>% 
          rename("school" = ".") %>% 
          rename("period range" = "Eduserve,.Inc")
        
        sim_time <- round(nrow(df) / 7000)
        
        Sys.sleep(sim_time)
        
        setProgress(1)
        
        result <- list(
          data = df,
          label = ledger_label
        )
        
        return(result)
        
      }
    )
  })
  
  # Render the table of the uploaded data
  output$ledger_table <- renderDT({
    
    df <- processFile()$data
    
    # Get the school name and period range
    schoolname <- processFile()$label$school
    period_range <- processFile()$label$`period range`
    
    # Combine school name and period range for the caption
    caption_text <- paste("School: ", schoolname, ", Period Range: ", period_range)
    df %>% datatable(
      caption = caption_text,
      class = 'cell-border stripe',
      options = list(paging = FALSE),
      colnames = c(
        "",
        "Category",
        "Account",
        "Type",
        "Date",
        "Document Number",
        "Vendor",
        "Debit/Credit",
        "Description",
        "Grant",
        "Department Name",
        "Fund Name"
      )
    ) %>% 
      formatCurrency(columns = "debit_credit")
  })
  
  # Add download handler
  output$download_ledger <- downloadHandler(
    filename = function() {
      paste("processed_GL_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      result <- processFile()
      writexl::write_xlsx(list(Data = result$data, Info = result$label), path = file)
    }
  )
  
  # Payroll Automation Server Functions
  
  # Declare filtered_payroll and filtered_payroll_report globally
  filtered_payroll <- NULL
  filtered_payroll_report <- NULL
  
  output$filtered_payroll_table <- renderDT({
    
    filtered_payroll <<- payroll_condensed %>% 
      filter(location_name == input$payroll_school)
    
    filtered_payroll_report <<- filtered_payroll %>% 
      group_by(cost_code) %>% 
      summarise(headcount = n_distinct(legal_name), annual_rate = sum(annual_rate))
    
    # Combine school name and period range for the caption
    caption_text2 <- paste("School: ", input$payroll_school, ", Payroll File Period: ", "03.15.2024")
    
    filtered_payroll %>% 
      datatable(
        caption = caption_text2,
        class = "cell-border stripe",
        options = list(paging = FALSE),
        colnames = c(
          "School",
          "Job Title",
          "Name",
          "Funding Source",
          "Annual Rate",
          "Cost Code",
          "Description"
        )
      ) %>% 
      formatCurrency(columns = "annual_rate")
    
  })
  
  # Add download handler
  output$download_filtered_payroll <- downloadHandler(
    filename = function() {
      paste("filtered_payroll_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(list('Payroll Detail' = filtered_payroll, "Payroll Pivot" = filtered_payroll_report), path = file)
    }
  )
  
  # Taxes and Benefits Server Functions
  
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
        group_by(department_name, account) %>%
        summarise(amount = sum(amount))
    }
    
    compensation_summary <- summarize_compensation_accounts(compensation_table_analysis, wage_accounts, tax_accounts, benefit_accounts)
    
    # Store compensation result in a reactive value
    output$compensation_result_table <- renderDT({
      
      compensation_result <- compensation_summary %>%
        pivot_wider(names_from = account, values_from = amount, values_fill = 0)
      
      compensation_table <- compensation_result %>% 
        datatable(
          class = 'cell-border stripe', 
          options = list(paging = FALSE),
          colnames = c('ID' = 1, 'Department' = 2)
        ) %>% 
        formatRound(2:ncol(compensation_result), 0) %>% 
        formatStyle(
          columns = 2:ncol(compensation_result),
          Color = styleInterval(
            cuts = -.01,
            values = c("white", "")
          ),
          backgroundColor = styleInterval(
            cuts = -.01,
            values = c("red", "")
          )
        )
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
              text = ~paste("Account: ", account_category, "<br>Month: ", entry_month, "<br>Value: ", scales::dollar(account_sum)),
              marker = list(size = 8),
              line = list(width = 4)) %>%
        layout(title = list(text = "Booked Compensation Amount By Month", font = list(size = 25)),
               xaxis = list(title = "Month"),
               yaxis = list(title = "", tickformat = "$,.0f"),
               hovermode = "closest",
               showlegend = TRUE,
               margin = list(t = 60)
        )
      
    })
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
