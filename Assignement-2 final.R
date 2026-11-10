# Amazon.in Laptop Data Extraction using R

# Step 1: Install and load required packages
packages <- c("rvest", "httr", "xml2", "dplyr", "stringr", "tibble")

install.packages(setdiff(packages, rownames(installed.packages())))

library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(stringr)
library(tibble)


# Step 2: Set Amazon laptop search URL
url <- "https://www.amazon.in/s?k=laptop"


# Step 3: Set output folder path
output_folder <- "D:/SWAPNIL FOLDER/ADYPU/FY Notes ( Sem 7)/DAR/Assignment -1/amazon_laptop_output"

dir.create(output_folder, showWarnings = FALSE)


# Step 4: Read Amazon webpage
response <- GET(
  url,
  add_headers(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36",
    "Accept-Language" = "en-IN,en;q=0.9",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  )
)

page <- read_html(content(response, "text", encoding = "UTF-8"))


# Step 5: Select laptop product blocks
products <- page %>%
  html_elements("div[data-component-type='s-search-result']")


# Step 6: Stop if no products found
if (length(products) == 0) {
  stop("No product data found. Amazon may be blocking scraping.")
}


# Step 7: Function to safely extract text
get_text <- function(product, selector) {
  value <- product %>%
    html_element(selector) %>%
    html_text2()
  
  ifelse(is.na(value), "Not Available", str_squish(value))
}


# Step 8: Function to safely extract product URL
get_link <- function(product, selector) {
  value <- product %>%
    html_element(selector) %>%
    html_attr("href")
  
  ifelse(
    is.na(value),
    "Not Available",
    paste0("https://www.amazon.in", value)
  )
}


# Step 9: Extract laptop details
amazon_laptop_data <- tibble(
  laptop_name = sapply(products, get_text, "h2 span"),
  price = sapply(products, get_text, ".a-price .a-offscreen"),
  rating = sapply(products, get_text, ".a-icon-alt"),
  reviews = sapply(products, get_text, "a[href*='customerReviews'] span"),
  delivery_date = sapply(products, get_text, "[data-cy='delivery-recipe']"),
  product_url = sapply(products, get_link, "h2 a")
)


# Step 10: Clean data
amazon_laptop_data <- amazon_laptop_data %>%
  filter(laptop_name != "Not Available") %>%
  mutate(
    price = ifelse(price == "", "Not Available", price),
    rating = ifelse(rating == "", "Not Available", rating),
    reviews = ifelse(reviews == "", "Not Available", reviews),
    delivery_date = ifelse(delivery_date == "", "Not Available", delivery_date),
    across(everything(), str_squish)
  )


# Step 11: Stop if extracted table is empty
if (nrow(amazon_laptop_data) == 0) {
  stop("No laptop data extracted. Amazon blocked the request or changed page structure.")
}


# Step 12: Save data into CSV file
csv_path <- file.path(output_folder, "amazon_laptop_data.csv")

write.csv(amazon_laptop_data, csv_path, row.names = FALSE)


# Step 13: Print and view final data
print(amazon_laptop_data)

View(amazon_laptop_data)

print(paste("CSV file saved at:", csv_path))

install.packages("httpuv")

viewer <- getOption("viewer")

viewer("https://www.amazon.in/s?k=laptop")

browseURL("https://www.amazon.in/s?k=laptop")