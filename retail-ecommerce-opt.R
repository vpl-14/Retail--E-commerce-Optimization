library(dplyr)
library(stringr)
library(readr)
library(lubridate)

df <- read_csv(
  "raw_indian_retail_commerce_3500.csv",
  show_col_types = FALSE
)

cat("Original rows:", nrow(df), "\n")

df <- df %>%
  mutate(
    across(
      where(is.character),
      ~ str_trim(.x)
    )
  )

date_text <- df$Date_Recorded

df$Date_Recorded <- as.Date(
  parse_date_time(
    date_text,
    orders = c(
      "ymd", "dmy", "mdy",
      "d-b-Y", "b-d-Y", "B d, Y"
    ),
    quiet = TRUE
  )
)

df$Region <- str_to_title(df$Region)

df$Sales_Channel <- str_to_lower(df$Sales_Channel)

df$Sales_Channel <- case_when(
  df$Sales_Channel == "instore" ~ "In-Store",
  df$Sales_Channel == "affiliate" ~ "Affiliate",
  df$Sales_Channel == "mobile_app" ~ "Online - Mobile App",
  df$Sales_Channel == "online - web" ~ "Online - Web",
  df$Sales_Channel == "online - mobile app" ~
    "Online - Mobile App",
  TRUE ~ str_to_title(df$Sales_Channel)
)

df$Product_Category <- str_to_title(
  str_to_lower(str_trim(df$Product_Category))
)

df$Product_Category <- case_when(
  df$Product_Category == "Electronics" ~ "Electronics",
  df$Product_Category == "Fashion" ~ "Fashion",
  df$Product_Category == "Home & Kitchen" ~ "Home & Kitchen",
  df$Product_Category == "Groceries" ~ "Groceries",
  df$Product_Category == "Beauty & Personal Care" ~
    "Beauty & Personal Care",
  TRUE ~ df$Product_Category
)


price_text <- df$Unit_Price

df$Unit_Price <- parse_number(
  price_text,
  na = c("", "NA", "N/A", "FREE")
)

df$Unit_Price[
  !is.na(price_text) &
    str_to_upper(str_trim(price_text)) == "FREE"
] <- NA_real_


df$Unit_Price[
  !is.na(df$Unit_Price) &
    df$Unit_Price <= 0
] <- NA_real_



df <- df %>%
  filter(
    !is.na(Quantity),
    Quantity > 0,
    Quantity != 999
  )



df$Customer_Rating[
  !is.na(df$Customer_Rating) &
    !(df$Customer_Rating %in% 1:5)
] <- NA_real_



df$Discount_Pct[
  !is.na(df$Discount_Pct) &
    (df$Discount_Pct < 0 |
       df$Discount_Pct > 1)
] <- NA_real_



df <- df %>%
  distinct(Transaction_ID, .keep_all = TRUE)


df <- df %>%
  filter(if_all(everything(), ~ !is.na(.x)))

remove_iqr_outliers <- function(data, column) {
  
  x <- data[[column]]
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- IQR(x, na.rm = TRUE)
  
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  
  data %>%
    filter(
      .data[[column]] >= lower,
      .data[[column]] <= upper
    )
}

df <- remove_iqr_outliers(df, "Quantity")
df <- remove_iqr_outliers(df, "Unit_Price")


df <- df %>%
  mutate(
    Gross_Sales_INR = Quantity * Unit_Price,
    Net_Sales_INR = Quantity * Unit_Price *
      (1 - Discount_Pct)
  )
cat("\n========== CLEANING REPORT ==========\n")
cat("Rows remaining:", nrow(df), "\n")
cat("Columns remaining:", ncol(df), "\n")
cat("Missing values:", sum(is.na(df)), "\n")
cat("Negative quantities:",
    sum(df$Quantity < 0), "\n")
cat("Invalid quantity 999:",
    sum(df$Quantity == 999), "\n")
cat("Duplicate transaction IDs:",
    sum(duplicated(df$Transaction_ID)), "\n")
write_csv(
  df,
  "cleaned_indian_retail_tableau.csv",
  na = ""
)

cat("\nCleaned dataset exported successfully!\n")

