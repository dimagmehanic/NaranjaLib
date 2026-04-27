# Orange Book
# Load the necessary library
library(shiny)
library(utils)
library(tidyverse)
library(ggalluvial)
library(fmtr)

con <- DBI::dbConnect(RSQLite::SQLite(), file.path(getwd(), "data", "OrangeBook.db")) # nolint

# source("setup.R")

prod <- tbl(con, "prod") %>% collect()

ui <- fluidPage(
  # Application title
  titlePanel("Orange Book"),
  fluidRow(tags$a(href = "https://www.fda.gov/drugs/drug-approvals-and-databases/orange-book-data-files", "Orange Book Main Page", target = "_blank")), # nolint
  fluidRow(tags$a(href = "https://www.accessdata.fda.gov/scripts/cder/ob/results_patent.cfm", "Use codes Page", target = "_blank")), # nolint
  fluidRow(
    column(4, sliderInput("Yrange", "Year range", value = c(min(prod$year), max(prod$year)), # nolint
                          min = min(prod$year), max = max(prod$year))),
    column(4, checkboxGroupInput("type", "Category of approved drugs",
                                 choices = unique(prod$Type),
                                 selected = unique(prod$Type))),
    column(4, selectInput("appl", "The firm name",
                          unique(prod$Applicant) %>% sort(),
                          multiple = TRUE))
  ),
  plotOutput("flow"),
  fluidRow(
    column(12, dataTableOutput("table"))
  )
)

server <- function(input, output, session) {
  # Server logic
  data <- reactive({
    req(input$Yrange, input$appl, input$type)
    prod %>%
      filter(year >= input$Yrange[1] & year <= input$Yrange[2] &
            Type %in% input$type & Applicant %in% input$appl)
  })

  tab <- reactive({
    data() %>% 
      group_by(year, Type, Applicant, Trade_Name, Definition) %>%
      summarise(name = first(Definition), .groups = "drop" ) %>%
      select (year, Applicant, Trade_Name, Type, Definition)
  })

  pd <- reactive({
    groped <- data() %>% 
      group_by(year, Type, Applicant, Trade_Name) %>%
      summarise(name = first(Trade_Name), .groups = "drop")

    N <- groped %>% group_by(year, Type) %>%
      summarise(n = n_distinct(Trade_Name), .groups = "drop")

    groped %>%
      group_by(year, Type, Applicant) %>%
      summarise(frq = n_distinct(Trade_Name), .groups = "drop") %>%
      left_join(N, by = join_by(year, Type)) %>%
      mutate(pct = round(100*frq/n, digits = 1))
  })

  pl <- reactive({
    pd() %>%
      ggplot(aes(axis1 = year, axis2 = Applicant, y = pct))+
      geom_alluvium(aes(fill = Type)) + geom_stratum() +
      geom_text(stat = "stratum",
                aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c("year", "Type", "Applicant")) +
      theme_void()
  })

  output$flow <- renderPlot(pl(), res = 96)
  output$table <- renderDataTable(tab(), options = list(pageLength = 10))
  session$onSessionEnded(function() { stopApp() })
}

shinyApp(ui, server)