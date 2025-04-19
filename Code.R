## Import Packages and Libraries
install.packages("corrplot")
install.packages("tidyverse")
install.packages("dplyr")  
install.packages("ggplot2") 
install.packages("DT") 
install.packages("rmarkdown")
install.packages("shiny")
install.packages("shinydashboard") 
install.packages("flexdashboard")
install.packages("tidyversa")
library(flexdashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)
library(flexdashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(DT)
library(corrplot)
library(shiny)
library(scales)

## Load Dataset ##

df <- read.csv("C:/Users/d4i7v/OneDrive/Documents/Mtech/sem2/INT553DATACLEANINGR/completeRproject/vgsales.csv")
datatable(df) 
View(df)
### Data Overview ##
colnames(df)
as.list(colnames(df))

df$Name 
df %>% select(Name, Platform, Year, Genre)
datatable(df %>% select(Name, Platform, Year, Genre, Publisher))

## Data Inspection ##

head(df)
head(df,10)
tail(df)
sample_n(df, 15)

str(df)
summary(df)


## DATA PREPROCESSING ##


sapply(df, n_distinct)
as.list(sapply(df, n_distinct))

duplicate_value <- sum(duplicated(df))
cat("Number of Duplicate Value:", duplicate_value, "\n")


# Missing values in each column
df <- read_csv("C:/Users/d4i7v/OneDrive/Documents/Mtech/sem2/INT553DATACLEANINGR/completeRproject/vgsales.csv", na = c("", "NA", "N/A", "n/a", "null", "NULL"))
missing_values <- colSums(is.na(df))
print(missing_values)
datatable(df[!complete.cases(df), ])



datatable(df %>% filter(is.na(Year)))
datatable(df %>% filter(is.na(Publisher)))
datatable(df %>% filter(is.na(Year) | is.na(Publisher)))
datatable(df %>% filter(is.na(Year) & is.na(Publisher)))
datatable(df %>% filter(is.na(Year) & is.na(Publisher)) %>% summarise_all(~sum(!is.na(.))))

df %>% filter(is.na(Year) | is.na(Publisher))
df %>% filter(is.na(Year) & is.na(Publisher)) %>% summarise_all(~sum(!is.na(.)))
# Remove rows with NA in 'Year' or 'Publisher'
df <- df %>% drop_na(Year, Publisher)

# Confirm missing values removed
colSums(is.na(df))
str(df)

## Data TYPE change##

df$Year <- as.numeric(df$Year)
str(df)
head(df, 50)


#  Global Overview
cat("Years covered:", min(df$Year), "to", max(df$Year), "\n")
cat("Total unique games:", n_distinct(df$Name), "\n")
cat("Unique Platforms:", n_distinct(df$Platform), "\n")
cat("Unique Genre:", n_distinct(df$Genre), "\n")
min(df$Year, na.rm = TRUE)
max(df$Year, na.rm = TRUE)
n_distinct(df$Year)
unique(df$Year)
unique(df$Genre)

View(df)

#Top1 in each region 
df %>%filter(NA_Sales == max(NA_Sales, na.rm = TRUE)) #top1
df %>%filter(EU_Sales == max(EU_Sales, na.rm = TRUE)) #top1
df %>%filter(JP_Sales == max(JP_Sales, na.rm = TRUE)) #top1
df %>%filter(Global_Sales == max(Global_Sales, na.rm = TRUE)) #top1

#Which genres and platforms are the top 10 most popular in terms of sales in each region and worldwide?
#Top 10 sales in North America
df %>% arrange(desc(NA_Sales)) %>% select(Name,Year,Genre,Platform,NA_Sales) %>% head(10)
#Top 10 sales in Europe
df %>% arrange(desc(EU_Sales)) %>% select(Name,Year,Genre,Platform,EU_Sales) %>% head(10)
#Top 10 sales in Japan
df %>% arrange(desc(JP_Sales)) %>% select(Name,Year,Genre,Platform,JP_Sales) %>% head(10)
#Top 10 sales for the rest of the world 
df %>% arrange(desc(Other_Sales)) %>% select(Name,Year,Genre,Platform,Other_Sales) %>% head(10)
#Top 10 worldwide sales (Golbal sales)
df %>% arrange(desc(Global_Sales)) %>% select(Name,Year,Genre,Platform,Global_Sales) %>% head(10)


#Game that Released to the Most Platforms
df %>% group_by(Name) %>% count() %>% arrange(desc(n)) %>% head(1)
df %>% filter(Name == "Need for Speed: Most Wanted") %>% arrange(Year)


#### EDA  #####




#1.  Games released per year
df %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Year, y = Count)) +
  geom_line(color = "tomato", size = 1.2) +
  geom_point(size = 2, color = "tomato") +
  labs(title = "Games Released per Year", x = "Year", y = "Number of Games") +
  theme_minimal()



# 2. Global Video Game Sales Trend Over Years

df %>% group_by(Year) %>% count() %>% head()
df %>% group_by(Year) %>% count() %>% tail()

global_trend <- df %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE))

ggplot(global_trend, aes(x = Year, y = Global_Sales)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(title = "Global Video Game Sales Trend Over Years",
       x = "Year", y = "Global Sales (Millions)") +
  theme_minimal()


# 3. Top 10 Games by Sales

df %>% arrange(desc(Global_Sales)) %>% select(Name,Year,Genre,Platform,Global_Sales) %>% head(10)

top10_games <- df %>%
  arrange(desc(Global_Sales)) %>%
  slice(1:10)

ggplot(top10_games, aes(x = reorder(Name, Global_Sales), y = Global_Sales)) +
  geom_col(fill = "Orange") +
  coord_flip() +
  labs(title = "Top 10 Games by Global Sales", x = "Game", y = "Sales (Millions)") +
  theme_minimal()


# 4. Popular Platforms by Number of Games Released

unique(df$Platform)
df %>% group_by(Platform) %>% count(Platform) %>%arrange(desc(n)) %>% head()

platform_game<-df %>% 
  group_by(Platform) %>% 
  count() %>% 
  arrange(desc(n)) %>% head(10) # for top 10 other wise remove head(10)

ggplot(platform_game, aes(x = n, y = reorder(Platform, n), fill = Platform)) +
  geom_col() +
  geom_text(aes(label = n), vjust = 0.5, hjust = -0.1, size = 4) +
  theme_minimal(base_size = 16, base_family = "Source Sans Pro") +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits=c(0, 2500)) +
  labs(x = "Game Released", 
       y = element_blank(),
       title = "Platforms by Number of Games Released") +
  guides(fill = 'none') 


# 5. Top 5 Platforms by Total Sales

 unique(df$Platform)

# Prepare data: top 5 platforms by global sales
platform_sales <- df %>%
  group_by(Platform) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Global_Sales)) %>%
  slice_head(n = 5) %>%
  mutate(Platform = factor(Platform, levels = Platform),
         pct = Global_Sales / sum(Global_Sales) * 100,
         label = paste0(Platform, ": ", round(pct, 1), "%"))
plot_ly(platform_sales, labels = ~Platform, values = ~Global_Sales, type = 'pie',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        marker = list(colors = c('#ff9999', '#66b3ff', '#99ff99', '#ffcc99', '#c2c2f0'),
                      line = list(color = '#000000', width = 1.5)),
        showlegend = FALSE) %>%
  layout(title = 'Top 5 Platforms by Total Sales',
         margin = list(l = 20, r = 20, b = 20, t = 50))


# 6. Popularity of Game Genres by Global Sales

unique(df$Genre)
genre_sales <- df %>%
  group_by(Genre) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Global_Sales))

# Plot with text labels
ggplot(genre_sales, aes(x = reorder(Genre, -Global_Sales), y = Global_Sales)) +
  geom_col(fill = "coral", color = "black", linewidth = 1) +
  geom_text(aes(label = round(Global_Sales, 1)), 
            vjust = -0.3, size = 2) +
  labs(title = "Popularity of Game Genres by Global Sales",
       x = "Genre",
       y = "Total Global Sales (Millions)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Number of Games by Genre (Count Plot)

unique(df$Genre)
genre_count <- df %>% count(Genre)
# Apply a coolwarm gradient fill
ggplot(genre_count, aes(x = reorder(Genre, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(colors = scales::gradient_n_pal
                       (c("#3b4cc0", "#e0f3f8", "#fbb4ae", "#b40426"))(seq(0, 1, length.out = 100))) +
  labs(title = "Number of Games by Genre", x = "Genre", y = "Count") +
  theme_minimal()


# 8. By Trend of Count of games released over the years
genres <- subset(df, select = c(Year, Genre)) %>%
  filter(Year < 2021) %>%
  group_by(Year, Genre) %>%
  summarize (total = n())
ggplot(genres, aes(x = Year, y = total, color = Genre)) +
  geom_line(size = .5, position = position_dodge(width = 0.1)) + geom_point(size = .5)


# 9. genre sales  trend over the year
unique(df$Year)
unique(df$Genre)
df %>%
  group_by (Genre, Year) %>%
  summarize(total_sales = sum(Global_Sales)) %>%
  ggplot(aes(x = Year, y = total_sales, fill = Genre)) +
  geom_col() + scale_fill_brewer(palette = "Set3") + 
  theme_classic() + labs(title = "Genre Popularity over the years by Sales", y = "Global Sales (Millions)")


# 10. genre Sales Per Region
sale = df %>%
  subset(select = c(Genre, NA_Sales, EU_Sales, JP_Sales, Other_Sales)) %>%
  group_by (Genre) %>%
  pivot_longer(c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"), values_to = "Sales")
ggplot(sale, aes(x = Genre, y = Sales, fill = name)) + geom_col(position = "dodge") + 
  labs(title = "Genre Sales per Region", x= "Genre", y = "Sales(Millions)") + 
  theme_bw() + theme(axis.text.x= element_text(size =10, angle = 90)) + scale_fill_brewer(palette = "Set1")


# 11. Top 10 Publishers with Most Games Released
as.list(unique(df$Publisher))
top_publishers_count <- df %>%
  count(Publisher, sort = TRUE) %>%
  slice(1:10)

ggplot(top_publishers_count, aes(x = reorder(Publisher, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top Publishers with Most Games Released", x = "Publisher", y = "Count") +
  theme_minimal()


# 12. Top 10 Publishers by Global Sales

top_publishers_sales <- df %>%
  group_by(Publisher) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Global_Sales)) %>%
  slice(1:10)

ggplot(top_publishers_sales, aes(x = reorder(Publisher,Global_Sales), y = Global_Sales, fill= Global_Sales))+
  geom_col() +
  scale_fill_viridis_c(option = "flare", direction = -1) +
  coord_flip() +
  labs(title = "Top Publishers by Global Sales", x = "Publisher", y = "Sales (Millions)") +
  theme_minimal()



# 13. Publisher Sales Trends Over Time

publisher_trend <- df %>%
  group_by(Year, Publisher) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

top_publishers <- df %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  top_n(3, Total_Sales) %>%
  pull(Publisher)

filtered_trend <- publisher_trend %>%
  filter(Publisher %in% top_publishers)


ggplot(filtered_trend, aes(x = Year, y = Global_Sales, color = Publisher)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Publisher Sales Trends Over Time",
       x = "Year",
       y = "Global Sales (Millions)") +
  theme_minimal()


# 14. Top Publisher by each Regions

publisher_sales <- df %>%
  group_by(Publisher) %>%
  summarise(
    NA_Sales = sum(NA_Sales, na.rm = TRUE),
    EU_Sales = sum(EU_Sales, na.rm = TRUE),
    JP_Sales = sum(JP_Sales, na.rm = TRUE),
    Other_Sales = sum(Other_Sales, na.rm = TRUE)
  )
sales_long <- publisher_sales %>%
  pivot_longer(cols = -Publisher, names_to = "Region", values_to = "Sales")
top_publishers <- sales_long %>%
  group_by(Region) %>%
  slice_max(order_by = Sales, n = 1)
ggplot(top_publishers, aes(x = Region, y = Sales, fill = Publisher)) +
  geom_col() +
  labs(title = "Top 1 Publisher by Region",
       x = "Region",
       y = "Total Sales (Millions)",
       fill = "Publisher") +
  theme_minimal() +
  theme(text = element_text(size = 12))


# 15. Correlation Between Regional and Global Sales
sales_data <- df %>%
  select(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales)
cor_matrix <- cor(sales_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "green")
get_top_publishers <- function(region_col, region_name) {
  df %>%
    group_by(Publisher) %>%
    summarise(Sales = sum(.data[[region_col]], na.rm = TRUE)) %>%
    arrange(desc(Sales)) %>%
    slice_head(n = 5) %>%
    mutate(Region = region_name)
}


# 16. Top 5 publisher by Region
get_top_publishers <- function(region_col, region_name) {
  df %>%
    group_by(Publisher) %>%
    summarise(Sales = sum(.data[[region_col]], na.rm = TRUE)) %>%
    arrange(desc(Sales)) %>%
    slice_head(n = 5) %>%
    mutate(Region = region_name)
}

top_na     <- get_top_publishers("NA_Sales", "North America")
top_eu     <- get_top_publishers("EU_Sales", "Europe")
top_jp     <- get_top_publishers("JP_Sales", "Japan")
top_other  <- get_top_publishers("Other_Sales", "Other")
top_global <- get_top_publishers("Global_Sales", "Global")

top_publishers <- bind_rows(top_na, top_eu, top_jp, top_other, top_global)

ggplot(top_publishers, aes(x = reorder(Publisher, Sales), y = Sales, fill = Publisher)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Region, scales = "free_y") +
  labs(title = "Top 5 Publishers by Region (Including Global Sales)",
       x = "Publisher", y = "Sales (Millions)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12))

#17. Publisher-wise Regional Sales Trends
top_publishers <- df %>%
  group_by(Publisher) %>%
  summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Global_Sales)) %>%
  slice_head(n = 3) %>%
  pull(Publisher)
df_top3 <- df %>%
  filter(Publisher %in% top_publishers) %>%
  group_by(Year, Publisher) %>%
  summarise(
    NA_Sales = sum(NA_Sales, na.rm = TRUE),
    EU_Sales = sum(EU_Sales, na.rm = TRUE),
    JP_Sales = sum(JP_Sales, na.rm = TRUE),
    Other_Sales = sum(Other_Sales, na.rm = TRUE),
    .groups = "drop"
  )
df_long <- df_top3 %>%
  pivot_longer(cols = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"),
               names_to = "Region", values_to = "Sales")
library(ggplot2)
ggplot(df_long, aes(x = Year, y = Sales, color = Publisher)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Region, scales = "free_y", ncol = 1,
             labeller = as_labeller(c(
               NA_Sales = "NA Sales Over Time",
               EU_Sales = "EU Sales Over Time",
               JP_Sales = "JP Sales Over Time",
               Other_Sales = "Other Sales Over Time"
             ))) +
  labs(title = "Regional Sales Trends Over Time (Top 3 Publishers)",
       x = "Year", y = "Sales (Millions)", color = "Publisher") +
  theme_minimal(base_size = 8) +
  theme(strip.text = element_text(size = 8),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))
