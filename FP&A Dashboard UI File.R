# Define UI for application
ui <- fillPage(
  padding = 80,
  theme = theme,
  titlePanel("Dashboard Beta"),
  tabsetPanel(
    tabPanel("Upload/Download",
             sidebarLayout(
               sidebarPanel(
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
                     style = "max-height: 500px; overflow-y: auto;",
                     tableOutput("ledger_table")
                   ),
                   verbatimTextOutput("error_message")
                 )
               )
             )
    ), 
    tabPanel("Taxes & Benefits",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 downloadButton("download_compensation", "Download Data")
               ),
               mainPanel(
                 card(
                   height = 650,
                   full_screen = TRUE,
                   card_header("Compensation Analysis"),
                   card_body(
                     min_height = 200,
                     layout_column_wrap(
                       width = 1/2, 
                       height = 800,
                       card(full_screen = TRUE, 
                            card_header("Compensation Table"), 
                            DTOutput("compensation_result_table"),
                       ),
                       card(full_screen = TRUE, 
                            card_header("Compensation Plot"), 
                            plotlyOutput("compensation_result_plot"))
                     )
                   )
                 )
               )
             )
    )
  )
)