pacman::p_load(tidyverse, pdftools, janitor, writexl, stringr)

setwd("C:/Users/MatthewAdams/Eduserve Solutions/FPATeam - Documents/Matthew Adams/CSUSA Projects/Convert PDF to Excel")

# Specify the folder containing the PDF files
folder_path <- "C:/Users/MatthewAdams/Eduserve Solutions/FPATeam - Documents/Matthew Adams/CSUSA Projects/Convert PDF to Excel"

# Get a list of PDF files in the folder
pdf_file <- list.files(path = folder_path, pattern = "\\.pdf$", full.names = TRUE)

pdf_texts <- pdf_text(pdf_file)

all_lines <- unlist(strsplit(pdf_texts, "\n", fixed = TRUE)) %>%
  tibble() %>%
  rename(scraped_text = '.') %>%
  mutate(scraped_text = str_remove_all(scraped_text, "\\$")) %>% 
  mutate(page = str_extract(scraped_text, "Page (\\d+) of \\d+"),
         page_numeric = as.numeric(str_extract(page, "\\d+")))

filled_pages <- all_lines %>% 
  fill(page_numeric, .direction = "up") %>% 
  select(-page)

column_names <- c("general_fund", "special_revenue_fund", "debt_service_fund", "capital_projects_fund", "total_all_funds")

summary_by_fund_and_function <- filled_pages %>%
  filter(page_numeric == 11) %>%
  select(scraped_text) %>%
  slice(-8:-10) %>% 
  mutate(scraped_text = str_trim(scraped_text, side = "left")) %>% 
  separate(scraped_text, into = c("account", "values"), sep = "\\s{2,}", extra = "merge", fill = "right") %>% 
  separate(values, into = column_names, sep = "\\s{2,}") %>%
  pivot_longer(cols = 2:6, names_to = "names", values_to = "values") %>%
  mutate(test = if_else(account == "", 1, 0)) %>% 
  filter(test != 1) %>% 
  select(-test) %>% 
  mutate(values = if_else(values == "-", "0", values)) %>% 
  mutate(values = if_else(str_detect(values, "\\("), str_replace(values, "\\(", "-"), values)) %>% 
  mutate(values = if_else(str_detect(values, "\\)"), str_replace(values, "\\)", ""), values)) %>% 
  mutate(values = if_else(str_detect(values, "[[:alpha:]]"), NA, values)) %>% 
  mutate(values = as.numeric(gsub(",", "", values))) %>%
  mutate(values = if_else(is.na(values), -9999, values)) %>% 
  mutate(account = factor(account, levels = unique(account))) %>%
  mutate(names = factor(names, levels = column_names)) %>%
  group_by(account, names) %>% 
  summarise(amount = sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = account, names_from = "names", values_from = "amount")

column_names2 <- c("actual_2021_22", "budget_2022_23", "budget_2023_24", "percent_of_change")

other_pages_pdf <- filled_pages %>% 
  filter(page_numeric != 11) %>%  
  group_by(page_numeric) %>% 
  # mutate(cumsum = row_number(page_numeric)) %>% 
  # mutate(test = if_else(cumsum %in% c(8:11), 1, 0)) %>% 
  # filter(test != 1) %>% 
  select(page_numeric, everything()) %>%
  # select(-test, -cumsum) %>% 
  mutate(scraped_text = str_trim(scraped_text, side = "left")) %>% 
  separate(scraped_text, into = c("account", "values"), sep = "\\s{2,}", extra = "merge", fill = "right") %>% 
  separate(values, into = column_names2, sep = "\\s{2,}") %>% 
  ungroup() %>% 
  pivot_longer(cols = 3:6, names_to = "names", values_to = "values") %>% 
  mutate(test = if_else(account == "", 1, 0)) %>% 
  filter(test != 1) %>% 
  select(-test) %>% 
  group_by(page_numeric) %>% 
  mutate(values = if_else(values == "-", "0", values)) %>%
  mutate(values = if_else(str_detect(values, "\\("), str_replace(values, "\\(", "-"), values)) %>% 
  mutate(values = if_else(str_detect(values, "\\)"), str_replace(values, "\\)", ""), values)) %>% 
  mutate(values = if_else(str_detect(values, "%"), as.numeric(str_replace(values, "%", ""))/100, as.numeric(gsub(",", "", values)))) %>% 
  mutate(values = if_else(is.na(values), -9999, values)) %>%
  mutate(account = factor(account, levels = unique(account))) %>%
  mutate(names = factor(names, levels = column_names2)) %>%
  ungroup() %>% 
  group_by(page_numeric, account, names) %>% 
  summarise(amount = sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = 1:2, names_from = "names", values_from = "amount") 



# Write to Excel file
write_xlsx(summary_by_fund_and_function, "summary_by_fund_and_function.xlsx")

# Write to Excel file
write_xlsx(other_pages_pdf, "individual_fund_summary.xlsx")








  

