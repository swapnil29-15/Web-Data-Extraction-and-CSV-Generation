# Assignment II: Web Data Extraction from Books to Scrape using R

# Step 1: Install and load packages
packages <- c("rvest", "httr", "xml2", "dplyr", "stringr")

install.packages(setdiff(packages, rownames(installed.packages())))

library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(stringr)



# Step 2: Set website URL and output folder
base_url <- "https://books.toscrape.com/"

output_folder <- "D:/SWAPNIL FOLDER/ADYPU/FY Notes ( Sem 7)/DAR/Assignment -1/books_output"

dir.create(output_folder, showWarnings = FALSE)


# Step 3: Function to convert rating word into number
convert_rating <- function(rating_text) {
  rating_text <- str_to_lower(rating_text)
  
  if (rating_text == "one") return(1)
  if (rating_text == "two") return(2)
  if (rating_text == "three") return(3)
  if (rating_text == "four") return(4)
  if (rating_text == "five") return(5)
  
  return(NA)
}


# Step 4: Function to extract one book detail page
scrape_book_details <- function(book_url) {
  page <- read_html(book_url)
  
  title <- page %>%
    html_element("h1") %>%
    html_text2()
  
  price <- page %>%
    html_element(".price_color") %>%
    html_text2()
  
  availability <- page %>%
    html_element(".availability") %>%
    html_text2()
  
  rating_text <- page %>%
    html_element(".star-rating") %>%
    html_attr("class") %>%
    str_replace("star-rating ", "")
  
  category <- page %>%
    html_elements(".breadcrumb li a") %>%
    html_text2() %>%
    tail(1)
  
  description <- page %>%
    html_element("#product_description + p") %>%
    html_text2()
  
  if (length(description) == 0 || is.na(description)) {
    description <- "No description available"
  }
  
  table_values <- page %>%
    html_elements("table td") %>%
    html_text2()
  
  upc <- table_values[1]
  
  tibble(
    title = title,
    category = category,
    price = price,
    rating = convert_rating(rating_text),
    availability = str_squish(availability),
    description = description,
    upc = upc,
    url = book_url
  )
}


# Step 5: Read main page
main_page <- read_html(base_url)


# Step 6: Get book links from first page
book_links <- main_page %>%
  html_elements(".product_pod h3 a") %>%
  html_attr("href")

book_links <- paste0(base_url, book_links)


# Step 7: Extract data from all book links
books_data <- bind_rows(lapply(book_links, scrape_book_details))


# Step 8: Handle missing values
books_data[is.na(books_data)] <- "Not Available"


# Step 9: Save data into CSV
csv_path <- file.path(output_folder, "books_extracted_data.csv")

write.csv(books_data, csv_path, row.names = FALSE)


# Step 10: Print final data
print(books_data)

print(paste("CSV file saved at:", csv_path))

View(books_data)