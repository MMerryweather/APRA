pacman::p_load(tidyverse, tidyxl, readxl, unpivotr)

table_1 = xlsx_cells("data/2018-AFLSS-201706.xlsx", sheets = "Table 1") %>% 
  filter(row > 4) %>% 
  filter(row != 6 & row != 7) %>% 
  select(row, col, data_type, character, numeric) %>%
  behead("N", header) %>% 
  mutate(header = str_replace_all(str_trim(header)," ","_")) %>% 
  select(-col) %>%
  spatter(header) %>% 
  select(-row)

clean = c("( +|\n|-)" = "_", "(\r|')" = "")
table_2 = read_xlsx("data/2018-AFLSS-201706.xlsx", sheet = "Table 2", skip = 4, na = "*") %>%
  set_names(str_replace_all(str_to_lower(names(.)), clean)) %>% 
  filter(!is.na(fund_name)) %>% 
  mutate_at(vars(total_assets:growth_in_number_of_member_accounts), as.numeric)

table_3 = read_xlsx("data/2018-AFLSS-201706.xlsx", sheet = "Table 3", skip = 4, na = "*") %>%
  set_names(str_replace_all(str_to_lower(names(.)), clean)) %>% 
  filter(!is.na(fund_name)) %>% 
  mutate_at(vars(net_assets_at_beginning_of_period:`one-year__rate_of_return`), as.numeric)


table_2 %>%
  ggplot(
    aes(
      x = ten_year_rate_of_return,
      y =  operating_expense_ratio,
      size = total_assets,
      fill = rse_regulatory_classification,
      colour = rse_regulatory_classification,
      label = fund_name
    )
  ) +
  geom_point(alpha=0.4)+
  scale_y_log10()+
  facet_wrap(~fund_type)

top_public = table_2 %>%
  filter(rse_regulatory_classification == "Public offer") %>% 
  ungroup() %>% 
  filter(ten_year_rate_of_return >= mean(ten_year_rate_of_return, na.rm = T)) %>% 
  filter(operating_expense_ratio < mean(operating_expense_ratio, na.rm = T)) %>%
filter(total_assets > mean(total_assets, na.rm = T)) %>% View()

top_pprivate = table_2 %>%
  filter(rse_regulatory_classification == "Non public offer") %>% 
  ungroup() %>% 
  filter(ten_year_rate_of_return >= mean(ten_year_rate_of_return, na.rm = T)) %>% 
  filter(operating_expense_ratio < mean(operating_expense_ratio, na.rm = T)) %>%
  filter(total_assets > mean(total_assets, na.rm = T)) %>% View()
