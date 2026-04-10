library(tidyverse)
library(readr)

# Step 1: Loading the raw dataset
raw_path <- file.choose()
raw <- read_csv(raw_path)
cat(nrow(raw), "rows ×", ncol(raw), "cols\n")

# Step 2: Removing junk rows
valid_series <- c(
  "Gini index",
  "Inflation, consumer prices (annual %)",
  "GDP per capita (current US$)",
  "Unemployment, total (% of total labor force) (national estimate)"
)
raw <- raw %>%
  filter(`Series Name` %in% valid_series)

# Step 3: Rename messy year columns
raw <- raw %>%
  rename_with(
    ~ str_extract(.x, "^\\d{4}"),
    matches("^\\d{4} \\[YR\\d{4}\\]")
  )

# Step 4: Pivot Wide → Long
long <- raw %>%
  pivot_longer(
    cols      = matches("^\\d{4}$"),
    names_to  = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year  = as.integer(Year),
    Value = na_if(Value, ".."),
    Value = as.numeric(Value)
  )

# Step 5: Pivot indicators → columns
tidy <- long %>%
  select(`Country Name`, `Country Code`, `Series Name`, Year, Value) %>%
  pivot_wider(
    names_from  = `Series Name`,
    values_from = Value
  )

# Step 6: Renaming to clean short names
tidy <- tidy %>%
  rename(
    Country        = `Country Name`,
    Code           = `Country Code`,
    GDP_per_capita = `GDP per capita (current US$)`,
    Gini_index     = `Gini index`,
    Inflation      = `Inflation, consumer prices (annual %)`,
    Unemployment   = `Unemployment, total (% of total labor force) (national estimate)`
  )

# Step 7: Filter Asian countries only
asian_codes <- c(
  "AFG","ARM","AZE","BGD","BTN","BRN","KHM","CHN","GEO",
  "IND","IDN","IRN","IRQ","JPN","JOR","KAZ","KGZ","LAO",
  "LBN","MYS","MDV","MNG","MMR","NPL","PAK","PHL","LKA",
  "SYR","TJK","THA","TLS","TKM","UZB","VNM","YEM","KOR",
  "SGP","TWN","HKG","TUR","ISR","SAU","ARE","QAT",
  "KWT","OMN","BHR"
)
asia <- tidy %>%
  filter(Code %in% asian_codes)

# Step 8: Keeping only the necessary columns
asia <- asia %>%
  select(Country, Code, Year, Gini_index, Inflation, GDP_per_capita, Unemployment)

# Step 9: Removing NA Values from Gini index and replacing NA with Median from other columns
asia_cleaned <- asia %>%
  drop_na(Gini_index) %>%
  group_by(Country) %>%
  mutate(
    Inflation      = ifelse(is.na(Inflation),      median(Inflation,      na.rm = TRUE), Inflation),
    GDP_per_capita = ifelse(is.na(GDP_per_capita), median(GDP_per_capita, na.rm = TRUE), GDP_per_capita),
    Unemployment   = ifelse(is.na(Unemployment),   median(Unemployment,   na.rm = TRUE), Unemployment)
  ) %>%
  ungroup()

# Step 10: Checking missing values per column before cleaning
colSums(is.na(asia))

# Step 11: Visualizing missing values as a bar chart before cleaning
num_of_missing_value_per_column <- sapply(asia, function(x) sum(is.na(x)))
barplot(num_of_missing_value_per_column,
        main      = "Missing Values per Column (Before)",
        col       = "steelblue",
        las       = 1,
        cex.names = 0.8,
        ylab      = "Count")

# Step 12: Checking missing values again after cleaning
colSums(is.na(asia_cleaned))

# Step 13: Visualizing missing values after cleaning to confirm they are handled
num_missing_after <- sapply(asia_cleaned, function(x) sum(is.na(x)))
barplot(num_missing_after,
        main      = "Missing Values per Column (After Cleaning)",
        col       = "steelblue",
        las       = 1,
        cex.names = 0.8,
        ylab      = "Count")

# Step 14: Detecting logically impossible values like negative unemployment or Gini above 100
invalid_gini  <- sum(asia_cleaned$Gini_index    <   0 | asia_cleaned$Gini_index    > 100, na.rm = TRUE)
invalid_unemp <- sum(asia_cleaned$Unemployment   <   0 | asia_cleaned$Unemployment  > 100, na.rm = TRUE)
invalid_gdp   <- sum(asia_cleaned$GDP_per_capita <   0, na.rm = TRUE)
invalid_infl  <- sum(asia_cleaned$Inflation      < -100, na.rm = TRUE)
cat("Invalid Gini values:",        invalid_gini,  "\n")
cat("Invalid Unemployment values:", invalid_unemp, "\n")
cat("Invalid GDP values:",          invalid_gdp,   "\n")
cat("Invalid Inflation values:",    invalid_infl,  "\n")

# Step 15: Boxplots before outlier handling to see how extreme the outliers are
boxplot(asia_cleaned$GDP_per_capita, main = "GDP per Capita — Before Outlier Handling")
boxplot(asia_cleaned$Inflation,      main = "Inflation — Before Outlier Handling")

# Step 16: Counting exactly how many outliers exist using the IQR method
detect_outliers <- function(x) {
  Q1      <- quantile(x, 0.25, na.rm = TRUE)
  Q3      <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val), na.rm = TRUE)
}
sapply(asia_cleaned[, c("Gini_index", "Inflation", "GDP_per_capita", "Unemployment")], detect_outliers)

# Step 17: Capping extreme values at 5th and 95th percentile to handle outliers without losing rows
winsorize <- function(x, lower = 0.05, upper = 0.95) {
  q <- quantile(x, c(lower, upper), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}
asia_cleaned <- asia_cleaned %>%
  mutate(
    Inflation      = winsorize(Inflation),
    GDP_per_capita = winsorize(GDP_per_capita),
    Unemployment   = winsorize(Unemployment)
  )
sapply(asia_cleaned[, c("Inflation", "GDP_per_capita", "Unemployment")], detect_outliers)

# Step 18: Boxplots after outlier handling to confirm the distributions look cleaner
boxplot(asia_cleaned$GDP_per_capita, main = "GDP per Capita — After Outlier Handling")
boxplot(asia_cleaned$Inflation,      main = "Inflation — After Outlier Handling")

# Step 19: Normalizing GDP, Inflation, and Unemployment to a 0–1 scale using Min-Max
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
normalize_dataset <- asia_cleaned
normalize_dataset$GDP_normalized          <- normalize(asia_cleaned$GDP_per_capita)
normalize_dataset$Inflation_normalized    <- normalize(asia_cleaned$Inflation)
normalize_dataset$Unemployment_normalized <- normalize(asia_cleaned$Unemployment)
View(normalize_dataset)

# Step 20: Converting Inflation into categories — Low, Moderate, High
asia_cleaned$Inflation_level <- cut(
  asia_cleaned$Inflation,
  breaks = c(-Inf, 3, 7, Inf),
  labels = c("Low", "Moderate", "High")
)
table(asia_cleaned$Inflation_level)

# Step 21: Checking for duplicate rows and removing them if any exist
duplicates <- sum(duplicated(asia_cleaned))
cat("Duplicate rows:", duplicates, "\n")
asia_cleaned <- asia_cleaned %>% distinct()

# Step 22: Summary statistics for all numeric columns
summary(asia_cleaned[, c("Gini_index", "Inflation", "GDP_per_capita", "Unemployment")])

# Step 23: Detailed Gini stats grouped by Inflation level
asia_cleaned %>%
  group_by(Inflation_level) %>%
  summarise(
    n           = n(),
    Mean_Gini   = round(mean(Gini_index),   2),
    SD_Gini     = round(sd(Gini_index),     2),
    Min_Gini    = round(min(Gini_index),    2),
    Max_Gini    = round(max(Gini_index),    2),
    Median_Gini = round(median(Gini_index), 2)
  )

# Step 24: Average Gini index per inflation group
group_analysis <- asia_cleaned %>%
  group_by(Inflation_level) %>%
  summarise(avg_gini = mean(Gini_index))
print(group_analysis)

# Step 25: How much each country's inequality shifted over time using standard deviation
variation_analysis <- asia_cleaned %>%
  group_by(Country) %>%
  summarise(sd_gini = round(sd(Gini_index), 4))
print(variation_analysis, n = 36)

# Step 26: Histogram — distribution of Gini Index
ggplot(asia_cleaned, aes(x = Gini_index)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  ggtitle("Distribution of Gini Index") + theme_minimal()

# Step 27: Histogram — distribution of GDP per Capita
ggplot(asia_cleaned, aes(x = GDP_per_capita)) +
  geom_histogram(bins = 20, fill = "coral", color = "white") +
  ggtitle("Distribution of GDP per Capita") + theme_minimal()

# Step 28: Histogram — distribution of Inflation
ggplot(asia_cleaned, aes(x = Inflation)) +
  geom_histogram(bins = 20, fill = "gold", color = "white") +
  ggtitle("Distribution of Inflation") + theme_minimal()

# Step 29: Boxplot — Gini index across the three inflation categories
ggplot(asia_cleaned, aes(x = Inflation_level, y = Gini_index, fill = Inflation_level)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  ggtitle("Gini Index across Inflation Levels") +
  xlab("Inflation Level") + ylab("Gini Index") +
  theme_minimal() +
  theme(legend.position = "none")

# Step 30: Top 5 and Bottom 5 countries ranked by average Gini index
country_avg <- asia_cleaned %>%
  group_by(Country) %>%
  summarise(avg_gini = round(mean(Gini_index), 2))
top5    <- country_avg %>% arrange(desc(avg_gini)) %>% head(5)
bottom5 <- country_avg %>% arrange(avg_gini)       %>% head(5)
print(top5)
print(bottom5)

# Step 31: Goal 1 — Scatter plot of Inflation vs Gini Index
ggplot(asia_cleaned, aes(x = Inflation, y = Gini_index)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("Inflation vs Gini Index") +
  xlab("Inflation (Annual %)") + ylab("Gini Index") +
  theme_minimal()

# Step 32: Goal 2 — Scatter plot of GDP per Capita vs Gini Index
ggplot(asia_cleaned, aes(x = GDP_per_capita, y = Gini_index)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("GDP per Capita vs Gini Index") +
  xlab("GDP per Capita (USD)") + ylab("Gini Index") +
  theme_minimal()

# Step 33: Goal 3 — Scatter plot of Unemployment vs Gini Index
ggplot(asia_cleaned, aes(x = Unemployment, y = Gini_index)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  ggtitle("Unemployment vs Gini Index") +
  xlab("Unemployment Rate (%)") + ylab("Gini Index") +
  theme_minimal()

# Step 34: Saving the final cleaned dataset as a CSV file
path <- file.path(dirname(raw_path), "WorldBank_Dataset_Final.csv")
write_csv(asia_cleaned, path)
View(asia_cleaned)