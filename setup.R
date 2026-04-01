
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

fmt_Type <- function(var) {
  if (var == "DISCN") "Discontinued product"
  else if (var == "OTC") "Over-the-counter drug"
  else if (var == "RX") "Prescription drug"
}

prod <- products %>% left_join(patent) %>% 
  left_join(ucode, by=join_by(Patent_Use_Code == Code)) %>% 
  filter(!is.na(Definition)) %>% 
  mutate( Type = map(Type, fmt_Type) %>% unlist()  )

rm(patent, ucode, products)