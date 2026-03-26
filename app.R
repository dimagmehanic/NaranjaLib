# Orange Book
# Load the necessary library
library(shiny)
library(utils)
library(tidyverse)
library(ggalluvial) 
library(fmtr)

# Specify the path to the Zip file
# zip_file <- file.path(getwd(), "data", "EOBZIP_2026_02.zip

# Extract files from the Zip archive
# unzip(zip_file, exdir = file.path(getwd(), "data"))

# read products data
products <- read.csv2(file.path(getwd(), "data", "products.txt"),
                       header = TRUE, sep = "~", fill = TRUE)
# read patent data
patent <- read.csv2(file.path(getwd(), "data", "patent.txt"),
                     header = TRUE, sep = "~", fill = TRUE) 
# read use codes
ucode <- read.csv2(file.path(getwd(), "data", "Ucode.csv"),
                   header = TRUE, sep = ",", fill = TRUE)

patent %<>% select(Appl_Type, Appl_No, Product_No, Patent_Use_Code) %>%
  filter(!is.na(Patent_Use_Code))

products %<>% filter(!is.na(Product_No) & !is.na(Appl_No)) %>%  
    mutate(
        date = mdy(ifelse(Approval_Date == "Approved Prior to Jan 1, 1982",
                          str_replace(Approval_Date,"Approved Prior to Jan 1, 1982","Dec 31, 1981"), # nolint
                          Approval_Date)),
        year = year(date)) %>% arrange(year, date)

fmt_Type <- function(var){
  if (var == "DISCN") "Discontinued product"
  else if (var == "OTC") "Over-the-counter drug"
  else if (var == "RX") "Prescription drug"
}

prod <- products %>% left_join(patent) %>% 
    left_join(ucode, by=join_by(Patent_Use_Code == Code)) %>% 
    filter(!is.na(Definition)) %>% 
  mutate( Type = map(Type, fmt_Type) %>% unlist()  )

rm(patent, ucode, products)  

ui <- fluidPage(
  # Application title
  titlePanel("Orange Book"),
  fluidRow(tags$a(href = "https://www.fda.gov/drugs/drug-approvals-and-databases/orange-book-data-files", "Orange Book Main Page", target = "_blank")), # nolint
  fluidRow(tags$a(href = "https://www.accessdata.fda.gov/scripts/cder/ob/results_patent.cfm", "Use codes Page", target = "_blank")), # nolint
  fluidRow(
    column(4, sliderInput("Yrange", "Year range", value = c(min(prod$year), max(prod$year)), 
                          min = min(prod$year), max = max(prod$year))),
    column(4, checkboxGroupInput("type", "Category of approved drugs",
                                 choices=unique(prod$Type), 
                                 selected=unique(prod$Type))),
    column(4, selectInput("appl", "The firm name",
                          unique(prod$Applicant) %>% sort(), 
                          multiple=TRUE))
  ),
  plotOutput("flow"),
  fluidRow(
    column(12, dataTableOutput("table"))
  )
)
  
server <- function(input, output, session) {
  # Server logic
  data <- reactive({
    prod %>% 
    filter( year >= input$Yrange[1] & year <=input$Yrange[2] & 
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
      summarise(name = first(Trade_Name), .groups = "drop" )
    
    N <- groped %>% group_by(year, Type) %>% 
      summarise(n = n_distinct(Trade_Name), .groups = "drop" ) 
    
    groped %>% 
      group_by(year, Type, Applicant) %>% 
      summarise(frq = n_distinct(Trade_Name), .groups = "drop" ) %>%  left_join(N, by = join_by(year, Type)) %>% 
      mutate(pct = round(100*frq/n, digits=1)  )
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
  output$table <- renderDataTable(tab(), options = list(pageLength=10))
}

shinyApp(ui, server)