library(rvest)
library(xml2)

# Function to find all anchor tags with href pointing to CSV files
find_csv_links <- function(url) {
  # Read the webpage
  page <- read_html(url)
  
  # Extract all anchor tags
  anchor_tags <- html_nodes(page, "a")
  
  # Filter anchor tags with href that end with '.csv'
  csv_links <- anchor_tags %>%
    html_attr("href") %>%
    na.omit() %>% # Remove missing hrefs
    grep("\\.csv$", ., value = TRUE) # Find links ending with .csv
  
  return(csv_links)
}

# Example Usage
## url <- "https://www.niid.go.jp/niid/en/survaillance-data-table-english.html" # Replace with the desired URL
## csv_files <- find_csv_links(url)
## print(csv_files)

for (i in 0:633){
  url <- sprintf("https://www.niid.go.jp/niid/en/survaillance-data-table-english.html?start=%d", i);
  csvs <- find_csv_links(url);
  for( csv in csvs){
    week_number <- csv %>%
      str_split("/", simplify = T) %>%
      `[`(length(.)-1);
    file_type <-  csv %>%
      str_split("/", simplify = T) %>%
      `[`(length(.)) %>%
      str_replace_all("[0-9]","");
    if (file_type == "teiten.csv") {
      cat(sprintf("Downloading %s", csv))
      suppressMessages(
        read_csv(paste0("https://www.niid.go.jp",csv),
                 progress=F)) %>% write_csv(sprintf("week_%s.csv", week_number));
      cat(sprintf("Wrote out %s", sprintf("week_%s.csv", week_number)));
      Sys.sleep(0.5);
    } else {
      cat(sprintf("Ignoring %s", csv));
    }
  }
}
