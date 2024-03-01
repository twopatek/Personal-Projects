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
        source <- data_frame(source)
        
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
  output$ledger_table <- renderTable({
    df <- processFile()$data
    df 
  }, height = "1000px")
  
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
  
}