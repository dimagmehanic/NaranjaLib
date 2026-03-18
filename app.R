# Orange Book
# Load the necessary library
library(shiny)
library(utils)

# Specify the path to the Zip file
zip_file <- file.path(getwd(), "data", "EOBZIP_2026_02.zip")

# Extract files from the Zip archive
unzip(zip_file, exdir = file.path(getwd(), "data"))

ui <- fluidPage(
  tags$a(href = "https://www.fda.gov/drugs/drug-approvals-and-databases/orange-book-data-files", "Orange Book Main Page", target = "_blank") # nolint
)

server <- function(input, output, session) {
  # Server logic

}

shinyApp(ui, server)