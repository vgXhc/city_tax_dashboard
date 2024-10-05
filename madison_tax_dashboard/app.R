#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(gt)
library(arrow)
library(duckdb)
library(duckplyr)

library(arrow)
library(duckdb)

con <- dbConnect(duckdb())

dbExecute(con,
          "CREATE VIEW taxes_all AS
   SELECT * FROM PARQUET_SCAN('data/taxes_all.parquet');")

dbExecute(con,
          "CREATE VIEW taxes_long AS
   SELECT * FROM PARQUET_SCAN('data/taxes_long.parquet');")


# list of all addresses for prepopulation address input field
address_choices <- tbl(con, "taxes_all") |>
  select(location) |> 
  arrange(location) |> 
  collect() |> 
  pull(location) |> 
  unique()

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("How have my Madison property value and taxes changed?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("address", 
                           label = "Enter your street address", 
                           choices = NULL
                           ),
            checkboxGroupInput("taxType",
                               "Show:",
                               choices = c(
                                     "city taxes" = "city_net_tax",
                                     "county taxes" = "county_net_tax",
                                     "school taxes" = "school_net_tax",
                                     "total net taxes" = "total_net_tax",
                                     "property value" = "total_assessed_value"
                                   ),
                               selected = "city_net_tax"),
            h3("Data Sources"),
            p("Taxes and property values: ", a(href = "https://www.cityofmadison.com/finance/treasury/property-taxes/tax-roll-data", "City of Madison")),
            p("Inflation: ", a(href = "https://fred.stlouisfed.org/graph/?g=WAVo", "Federal Reserve Economic Data")),
            p("Consumer Price Index: ", a(href = "https://www.bls.gov/cpi/data.htm", "Bureau of Labor Statistics"))
        ),

        # Show a plot and table
        mainPanel(
          tabsetPanel(
            id = "viz",
            tabPanel(
              "Absolute values",
           plotOutput("cityPlot"),
           gt_output("cityTable")
            ),
           tabPanel("Indexed values",
                    plotOutput("cityPlotIndex"),
                    gt_output("cityTableIndex")
                    ),
           tabPanel(
             "More info/FAQ",
             h1("What is this app all about?"),
             p("The City of Madison is asking its citizen to vote on a referendum. If the referendum passes, it would allow the city to increase the overall amount of property tax it collects by $22 million dollars. This website is meant to help voters put the referendum in context and better understand the relationship between individual property values, different kinds of property taxes, and how this relates to the overall inflation rate."),
               p("On the \"Visualization\" tab, you can search for your address and see tax roll data from 2016 to 2023. It allows you to compare how your property values have changed (they have most likely climbed almost every year) and also how the different components of your property tax bill have changed over time. How much property tax went to the school district compared to the city? Have property taxes gone up as much as your property values?"),
             h1("Where can I read more about  property taxes and the referendum?"),
                p("This website doesn't dive into the question of why we need a referendum now. The short version: Our city is growing, which means expenses are going up. But the state legislature is putting hard constraints on property tax increases and other ways to generate revenue. And the amount of state aid per capita that the city receives is among the lowest across all Wisconsin municipalities. There's a lot more to this, and if you want to read more, take a look at these sources:"),
                
                p(a(href = "https://www.cityofmadison.com/mayor/documents/REFERENDUMINFOSHEETFINAL.pdf", "City of Madison Referendum Info Sheet")),
                p(a(href= "https://posts.unit1127.com/p/madisons-budget-challenges-a-four", "Madison’s Budget Challenges: A four part series")
                ),
                h1("Who created this website?"),
                p("That would be me, Harald Kliems. I live in Madison and care about it a lot. Property taxes, levy limits, mill rates, and all that are complicated, and I have encountered many smart people who were and are confused by it. I'm hoping this website can clear things up at least a little."),
                p("I received helpful feedback from members of StrongTowns Madison. All opinions on here are my own."),
                h1("How should I vote on the referendum?"),
                p("You should look at the facts and decide for yourself. Personally, I cannot vote because I'm not a citizen of the US. If I ", em("could"), "vote, I would enthusiastically vote in favor of the referendum. It's very unfortunate that the referendum is needed, but a failure of the referendum will have terrible consequences for our city and many of its residents. And that damage will be hard to undo."),
                h1("What about the school referenda?"),
                p("Information on the proportion of taxes that goes to the school district is included in the charts and tables."),
             h1("What about special assessements?"),
             p("Special assessments (e.g. if the road in front of your property was reconstructed) are not included here. They don't have anything to do with the referendum -- but in some years they may be a large part of the amount of money you owe the city!"),
                h1("I found an error or would like to request a feature. What should I do?"),
                p("Send me an email at kliems@gmail.com or ", a(href= "https://github.com/vgXhc/city_tax_dashboard/issues", "submit an issue via GitHub")),
                h1("How can I support this website?"),
                p("For now, I'm planning to take this website offline after the election date. Until then, I'm covering the hosting costs out of pocket. If you want to contribute to the cost, you can buy me a ", a(href = "https://ko-fi.com/haraldk", "ko-fi")),
                h1("Where's the source code?"),
                p(a(href="https://github.com/vgXhc/city_tax_dashboard", "https://github.com/vgXhc/city_tax_dashboard"))
           )
        )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'address', choices = address_choices, server = TRUE, options = list(placeholder = 'start typing your address'))
  
  taxes_plot <- reactive(
    tbl(con, "taxes_long") |> 
      filter(location == input$address)
  )
  taxes_table <- reactive(
    tbl(con, "taxes_all") |> 
      filter(location == input$address)
  )
  
  #create names of percentage change columns
  change_columns <- reactive(
    map_chr(input$taxType, ~paste0(.x, "_change"))
  )
  
    output$cityPlot <- renderPlot({

      taxes_plot() |> 
        filter(variable %in% input$taxType) |> 
        mutate(variable_label = case_match(variable,
                                           "city_net_tax" ~ "City Net Tax",
                                           "county_net_tax" ~ "County Net Tax",
                                           "school_net_tax" ~ "School Net Tax",
                                           "total_net_tax" ~ "Total Net Tax",
                                           "total_assessed_value" ~ "Total Assessed Value")) |> 
        ggplot(aes(tax_year, value, color = variable_label)) +
          geom_line() +
        geom_point() +
        geom_text(aes(label = scales::label_percent( 
                                              accuracy = 1,
                                              style_positive = "plus",
                                              style_negative = "minus")(change), color = variable_label), nudge_y = 300, nudge_x = -0.5) +
        xlab("Tax Year") +
        labs(title = paste0("Property Values and Taxes for ", input$address)) + 
        scale_y_continuous(labels = scales::label_dollar(), limits = c(0, NA),
                           name = "Amount") +
        hrbrthemes::scale_color_ipsum() +
        hrbrthemes::theme_ipsum() +
        theme(legend.title = element_blank())

    })
    output$cityTable <- render_gt({

      taxes_table() %>%
        ungroup() %>% 
        select(tax_year, any_of(input$taxType), inflation_rate, any_of(change_columns())) %>%
        select(order(colnames(.))) %>% 
        relocate(tax_year) |> 
        gt() |> 
        fmt_percent(columns = ends_with("_change"), decimals = 1, force_sign = TRUE) |> 
        fmt_percent(columns = inflation_rate, scale_values = FALSE, decimals = 1, force_sign = TRUE) |>
        fmt_currency(columns = any_of(input$taxType)) |> 
        cols_label_with(fn = ~ gsub("\\_", " ", .) |> str_to_title(.)) |> 
        cols_move_to_end(columns = "inflation_rate") |> 
        data_color(columns = ends_with(c("_change", "_rate")), 
                   #domain = c(0,1),
                   palette = "viridis") |> 
        tab_style(style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(columns = tax_year))
    })
    
    output$cityPlotIndex <- renderPlot({
      
      # all indexed variables
      vars_indexed <- paste0(input$taxType, "_indexed")
      
      taxes_plot() |> 
        filter(variable %in% vars_indexed) |> 
        mutate(variable_label = case_match(variable,
                                           "city_net_tax_indexed" ~ "City Net Tax",
                                           "county_net_tax_indexed" ~ "County Net Tax",
                                           "school_net_tax_indexed" ~ "School Net Tax",
                                           "total_net_tax_indexed" ~ "Total Net Tax",
                                           "total_assessed_value_indexed" ~ "Total Assessed Value")) |> 
        ggplot(aes(tax_year, value, color = variable_label)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = scales::label_percent( 
          accuracy = 1,
          style_positive = "plus",
          style_negative = "minus")(change), color = variable_label), nudge_y = 1, nudge_x = -0.5) +
        xlab("Tax Year") +
        labs(title = paste0("Property Values and Taxes for ", input$address),
             subtitle = "Indexed to 2016 = 100") + 
        scale_y_continuous(#labels = scales::label_dollar(), limits = c(0, NA),
                           name = "Index (2016)") +
        hrbrthemes::scale_color_ipsum() +
        hrbrthemes::theme_ipsum() +
        theme(legend.title = element_blank())
      
    })
    output$cityTableIndex <- render_gt({
      # all indexed variables
      vars_indexed <- paste0(input$taxType, "_indexed")
      
      taxes_table() %>%
        ungroup() %>% 
        select(tax_year, any_of(vars_indexed), cpi_indexed) %>%
        select(order(colnames(.))) %>% 
        relocate(tax_year) |> 
        gt() |> 
        fmt_number(columns = ends_with("indexed"), decimals = 1) |> 
        # fmt_percent(columns = ends_with("_change"), decimals = 1, force_sign = TRUE) |> 
        # fmt_percent(columns = inflation_rate, scale_values = FALSE, decimals = 1, force_sign = TRUE) |>
        # fmt_currency(columns = any_of(input$taxType)) |> 
        cols_label_with(fn = ~ gsub("\\_", " ", .) |> str_to_title(.))  |> 
        cols_label_with(fn = ~ str_remove(., "Indexed")) |> 
        cols_label(cpi_indexed = "Consumer Price Index") |> 
        cols_move_to_end(columns = "cpi_indexed") |> 
        # data_color(columns = ends_with(c("_change", "_rate")), 
        #            #domain = c(0,1),
        #            palette = "viridis") |> 
        tab_style(style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(columns = tax_year)) |> 
        tab_source_note("All values indexed to 2016. 2016 = 100")
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

