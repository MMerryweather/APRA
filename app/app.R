library(shiny)
pacman::p_load(tidyverse, stringr, ggthemr, readxl, hrbrthemes, magrittr, here)
#------------------------------------------------------------------------------
multiply_by_1000 = function(x) {
  as.numeric(x) * 1000
}
percent_to_numeric = function(x) {
  str_replace(x, "\\%", "") %>% as.numeric()
}
percent_label = scales::percent_format()

# Clean up titles
replace_list = c(
  "(’|'|:|\U0092)" = "",
  "(\\/)" = "-",
  "[\n\r]+" = " ",
  "( |-)" = "_"
)

# Perform transforms to both tables
data = list("Table 2!A5:AA226","Table 3!A5:CE226") %>% 
  map(~ read_xlsx(here("data/2017-AFLSS-201606.xlsx"), range = .)) %>%
  map(~ set_names(., str_replace_all(names(.), replace_list))) %>%
  map(~ set_names(., str_replace_all(names(.), "___", "_"))) %>%
  map(~ filter(., !is.na(ABN)))

AFLSS2 = data %>%
  pluck(1) %>%
  mutate_at(c(13, 15, 16, 24:26), as.numeric) %>%
  mutate_at(c(13, 25, 26), multiply_by_1000) %>%
  mutate_at(c(14, 17:23, 27), percent_to_numeric)

AFLSS3_numerics = c(13:39, 41:62, 64:72, 74:82)

AFLSS3 = data %>%
  pluck(2) %>%
  mutate_at(AFLSS3_numerics, as.numeric) %>%
  mutate_at(AFLSS3_numerics, multiply_by_1000) %>%
  mutate_at(seq(min(AFLSS3_numerics), max(AFLSS3_numerics) + 1)[!(seq(min(AFLSS3_numerics), max(AFLSS3_numerics) +
                                                                        1) %in% AFLSS3_numerics)], percent_to_numeric) %>%
  select(-(84:85)) %>%
  set_names(str_replace_all(names(.), "[^A-Z,^a-z,^0-9,^_]", "")) %>%
  set_names(str_replace_all(names(.), "(__|/)", "_"))

# figure out a way to do this in-line
names(AFLSS3)[70] = "Associated_with_service_provider_expenses_super"

# Re-calculate Operating Expense ratio to include Advice fees
# and restore precision (originals rounded to 0.1%)
AFLSS3 = AFLSS3 %>%
  mutate(
    Net_members_benefits_outflow_ratio = (Total_Members_benefit_flows_out +
                                            Outward_rollover) / (Total_Members_benefit_flows_in + Inward_rollovers),
    Investment_expenses_ratio = (Total_Investment_expenses / Cash_flow_adjusted_net_assets),
    Operating_expense_ratio = (
      Total_administration_and_operating_expenses + Advice_expenses
    ) / Cash_flow_adjusted_net_assets,
    One_year_rate_of_return = Net_earnings_after_tax / Cash_flow_adjusted_net_assets
  )

fund_list = AFLSS3 %>%
  filter(!is.na(Operating_expense_ratio) & Operating_expense_ratio > 0) %>% 
  pluck("Fund_name") %>%
  str_replace_all("([^A-Z,a-z,0-9, ,\\&,-])", "")

#------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- bootstrapPage(
   # Application title
   titlePanel("APRA Superannuation Data Explorer"),
   selectInput("fund", "Select a Fund:",choices=fund_list),
   splitLayout(
     plotOutput("costs",width = "100%", height = 580),
     plotOutput("returns",width = "100%", height = 580)
   )
)
#------------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$costs <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = 20 + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
     AFLSS3 %>% 
       ggplot(aes(x = Operating_expense_ratio,
                  fill = RSE_Regulatory_classification,
                  colour = RSE_Regulatory_classification))+
       geom_density(alpha = 0.4)+
       facet_grid(Fund_type~.)+
       scale_x_continuous(labels = scales::percent_format(), trans = "log10",
                          breaks = 10^(-4:0))+
       theme_ipsum()+
       no_y_gridlines()+
       legend_bottom()+
       no_legend_title()+
       geom_vline(data = filter(AFLSS3, Fund_name %in% input$fund),
                  aes(xintercept = Operating_expense_ratio,
                      colour = RSE_Regulatory_classification),
                  show.legend = F)+
       geom_text(data = filter(AFLSS3, Fund_name %in% input$fund),
                 aes(x = Operating_expense_ratio, y  = 2.5, label = Fund_name),
                 hjust = "right",
                 nudge_x = -0.01,
                 show.legend = F)+
       geom_text(data = filter(AFLSS3, Fund_name %in% input$fund),
                 aes(x = Operating_expense_ratio, y= 2.5, label = percent_label(Operating_expense_ratio)),
                 hjust = "left",
                 nudge_x = 0.01,
                 show.legend = F)+
       no_legend_title()+
       labs(title = "Operating Costs (%)",
            x = "Operating Expense Ratio (%)")+
       theme(strip.text.y = element_text(size = 10, angle = 0))+
       no_y_axis()+
       no_minor_gridlines()
     
   })

   output$returns <- renderPlot({
     # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2] 
     # bins <- seq(min(x), max(x), length.out = 20 + 1)
     # 
     # # draw the histogram with the specified number of bins
     # hist(x, breaks = bins, col = 'darkgray', border = 'white')
     AFLSS2 %>% 
       ggplot(aes(x = Ten_year_rate_of_return,
                  fill = RSE_Regulatory_classification,
                  colour = RSE_Regulatory_classification))+
       geom_density(alpha = 0.4)+
       facet_grid(Fund_type~.)+
       scale_x_continuous(labels = scales::percent_format(),
                          breaks = seq(-0.06, 0.12, 0.02))+
       theme_ipsum()+
       no_gridlines()+
       legend_bottom()+
       geom_vline(xintercept = 0)+
       geom_vline(data = filter(AFLSS2, Fund_name %in% input$fund),
                  aes(xintercept = Ten_year_rate_of_return,
                      colour = RSE_Regulatory_classification),
                  show.legend = F)+
       geom_text(data = filter(AFLSS2, Fund_name %in% input$fund),
                 aes(x = Ten_year_rate_of_return, y= 150, label = Fund_name),
                 hjust = "right", show.legend = F,
                 nudge_x = -0.0005)+
       geom_text(data = filter(AFLSS2, Fund_name %in% input$fund),
                 aes(x = Ten_year_rate_of_return, y= 150, label = percent_label(Ten_year_rate_of_return)),
                 hjust = "left", show.legend = F,
                 nudge_x = 0.0005)+
       no_legend_title()+
       labs(title = "Annualized Returns (10Y)",
            x = "Ten Year Rate of Return (%)")+
       theme(strip.text.y = element_text(size = 10, angle = 0),
             strip.placement = "inside")+
       no_y_axis()
   }) 
   
}




#------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)

 