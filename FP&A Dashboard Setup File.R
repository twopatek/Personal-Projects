# Install and load necessary libraries
pacman::p_load(shiny, bslib, htmltools, tidyverse, lubridate, purrr, janitor, openxlsx, shinythemes, writexl, plotly, DT)


# Identify word pattern to remove totaling rows from general ledger
rows_to_remove <- c("Total")

# Create vector of accounts numbers to categorize general ledger accounts
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


# Set theme for app 
theme = shinytheme("cerulean")

# Run the application
shinyApp(ui = ui, server = server)



