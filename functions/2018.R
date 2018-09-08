pacman::p_load(tidyverse, tidyxl, readxl, unpivotr, hrbrthemes, ggthemr)

table_1 = xlsx_cells("data/2018-AFLSS-201706.xlsx", sheets = "Table 1") %>% 
  filter(row > 4) %>% 
  filter(row != 6 & row != 7) %>% 
  select(row, col, data_type, character, numeric) %>%
  behead("N", header) %>% 
  mutate(header = str_replace_all(str_trim(header)," ","_")) %>% 
  select(-col) %>%
  spatter(header) %>% 
  select(-row)

clean = c("( +|\n|-)" = "_", "(\r|'|’|:)" = "")
table_2 = read_xlsx("data/2018-AFLSS-201706.xlsx", sheet = "Table 2", skip = 4, na = "*") %>%
  set_names(str_replace_all(str_to_lower(names(.)), clean)) %>% 
  filter(!is.na(fund_name)) %>% 
  mutate_at(vars(total_assets:growth_in_number_of_member_accounts), as.numeric)

table_3 = read_xlsx("data/2018-AFLSS-201706.xlsx", sheet = "Table 3", skip = 4, na = "*")
names(table_3)[60] = "investment_associated_with_service_provider_expenses"

table_3 = table_3 %>% set_names(str_replace_all(str_to_lower(names(.)), clean)) %>% 
  filter(!is.na(fund_name)) %>% 
  mutate_at(c(13, 56:79), as.numeric) %>% 
  mutate(investment_expenses_ratio = (investment_expenses_associated_with_investment_management_base_fee+
                                           investment_expenses_associated_with_investment_management_performance_based_fee+
                                           custodian+
                                           investment_consultant+
                                           investment_associated_with_service_provider_expenses+
                                           other_investment_expenses)/ cash_flow_adjusted_net_assets,
         operating_expense_ratio = (total_administration_and_operating_expenses + advice_expenses) / cash_flow_adjusted_net_assets)

df = table_3 %>% 
  inner_join(select(table_2, abn, five_year_rate_of_return,ten_year_rate_of_return)) %>% 
  mutate_at(vars(five_year_rate_of_return,ten_year_rate_of_return), as.numeric) %>% 
  filter(!is.na(five_year_rate_of_return))

df %>% 
  filter(rse_regulatory_classification == "Public offer") %>% 
  filter(five_year_rate_of_return >= median(five_year_rate_of_return)) %>%
  ggplot(aes(x = operating_expense_ratio,
             y = investment_expenses_ratio,
             colour = fund_type,
             size = net_assets_at_beginning_of_period/1e6,
             label = fund_name))+
  geom_point(alpha = 0.7)+
  scale_x_log10(limits = 10^c(-3,-1),
                breaks = 10^c(-3:-1),
                labels = scales::percent_format())+
  scale_y_log10(limits = 10^c(-5,-1.8),
                breaks = 10^c(-5:-1),
                labels = scales::percent_format())+
  scale_colour_viridis_d(direction = -1)+
  ggrepel::geom_text_repel(size= 3, alpha= 0.5, force = 10, show.legend = F)+
  labs(title = "Fees in Open Funds",
       subtitle = "Source: APRA - 2018 Annual Fund Level Summary Statistics",
       x = "Operating Expense Ratio (Includes Advice Fees)",
       y=  "Investment Expense Ratio",
       size = "Fund Size ($M)",
       colour = "Fund Type")+
  annotation_logticks()

ggsave(here::here("figures/fees.pdf"),
       width = 297,
       height = 210,
       units = "mm")

  
df %>% 
  filter(rse_regulatory_classification == "Public offer") %>% 
  filter(fund_type != "Corporate") %>%
  filter(operating_expense_ratio > 0.001) %>%
  ggplot(aes(x = operating_expense_ratio,
             y = five_year_rate_of_return,
             colour = fund_type,
             fill = fund_type,
             size = net_assets_at_beginning_of_period / 1e6,
             label = fund_name))+
  geom_point(alpha = 0.7)+
  scale_x_log10(limits = 10^c(-4,-1),
                breaks = 10^c(-4:-1),
                labels = scales::percent_format())+
  scale_y_percent()+
  scale_colour_viridis_d(direction = -1)+
  geom_smooth(method = "lm", se = F, show.legend = F)+
  stat_ellipse(show.legend = F) +
  labs(title = "Returns vs Fees in Open Funds",
       subtitle = "Source: APRA - 2018 Annual Fund Level Summary Statistics",
       x = "Operating Expense Ratio (Includes Advice Fees)",
       y=  "Five Year ROCE",
       size = "Fund Size ($M)",
       colour = "Fund Type",
       fill = "Fund Type")+
  annotation_logticks()

ggsave(here::here("figures/Returns vs fees.pdf"),
       width = 297,
       height = 210,
       units = "mm")


df %>% 
  # filter(rse_regulatory_classification == "Public offer") %>% 
  filter(five_year_rate_of_return >= median(five_year_rate_of_return, na.rm = T)) %>%
  filter(operating_expense_ratio >= 0.001) %>%
  ggplot(aes(x = operating_expense_ratio,
             y = five_year_rate_of_return,
             colour = fund_type,
             size = net_assets_at_beginning_of_period / 1e6,
             label = fund_name,
             shape = rse_regulatory_classification))+
  geom_point(alpha = 0.7)+
  scale_x_log10(limits = 10^c(-3,-1.5),
                breaks = 10^c(-3:-1),
                labels = scales::percent_format())+
  scale_y_percent(limits = c(0.08, 0.115))+
  scale_colour_viridis_d(direction = -1)+
  ggrepel::geom_text_repel(size= 3, alpha= 0.5, force = 10, show.legend = F)+
  scale_shape_manual(values = c(1,19))+
  labs(title = "5Y Returns vs Fees",
       subtitle = "Source: APRA - 2018 Annual Fund Level Summary Statistics",
       x = "Operating Expense Ratio (Includes Advice Fees)",
       y = "Five Year ROCE",
       size = "Fund Size ($M)",
       shape = "Offer Type",
       colour = "Fund Type")+
  annotation_logticks(sides="b")

ggsave(here::here("figures/5Y Returns vs fees in all funds.pdf"),
       width = 297,
       height = 210,
       units = "mm")


df %>% 
  # filter(ten_year_rate_of_return >= median(ten_year_rate_of_return, na.rm = T)) %>%
  filter(operating_expense_ratio >= 0.001) %>%
  ggplot(aes(x = operating_expense_ratio,
             y = ten_year_rate_of_return,
             colour = fund_type,
             size = net_assets_at_beginning_of_period / 1e6,
             label = fund_name,
             shape = rse_regulatory_classification))+
  geom_point(alpha = 0.7)+
  scale_x_log10(limits = 10^c(-3,-1.5),
                breaks = 10^c(-3:-1),
                labels = scales::percent_format())+
  scale_y_percent(limits = c(0.03, 0.06))+
  scale_colour_viridis_d(direction = -1)+
  ggrepel::geom_text_repel(size= 3, alpha= 0.5, force = 10, show.legend = F)+
  scale_shape_manual(values = c(1,19))+
  labs(title = "10Y Returns vs Fees",
       subtitle = "Source: APRA - 2018 Annual Fund Level Summary Statistics",
       x = "Operating Expense Ratio (Includes Advice Fees)",
       y = "Ten Year ROCE",
       size = "Fund Size ($M)",
       shape = "Offer Type",
       colour = "Fund Type")+
  annotation_logticks(sides="b")

ggsave(here::here("figures/10Y Returns vs fees in all funds.pdf"),
       width = 297,
       height = 210,
       units = "mm")


table_2 %>%
  filter(operating_expense_ratio>0) %>% 
  ggplot(
    aes(
      x = ten_year_rate_of_return,
      y =  operating_expense_ratio,
      size = total_assets,
      fill = fund_type,
      colour = fund_type,
      label = fund_name
    )
  ) +
  geom_point(alpha=0.6, na.rm = T)+
  scale_x_percent()+
  scale_y_log10(limits = 10^c(-4,-1),
                breaks = 10^c(-4:-1),
                labels = scales::percent_format())+
  facet_wrap(~rse_regulatory_classification)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme_ipsum()+
  geom_vline(xintercept = 0, colour = "black")



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
