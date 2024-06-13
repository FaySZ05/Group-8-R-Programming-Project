# Load Libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(shiny)
library(plotly)

# Read the CSV file
tax_data <- read_csv("~/Documents/UGM Year 1 Semester 2/R Programming Project Kelompok 8/Laporan Pajak Semester 2 2023(Laporan Pajak 10 tahun).csv")

# Remove rows 22 to 50
tax_data1 <- tax_data[-c(22:50), ]

# View the first few rows of the data
head(tax_data)

# Remove rows with missing values
clean_data <- na.omit(data)

# View the first few rows of the cleaned data
head(clean_data)

# Data for tax revenue
tax_revenue <- data.frame(
  Year = 2014:2023,
  Total_Tax_Revenue = c(1146.9, 1240.4, 1285, 1435, 1518.8, 1546.1, 1404.1, 1473.6, 2034.5, 1869),
  Total_Tax_Revenue_Growth = c(7.0, 8.2, 3.6, 4.6, 13.0, 13.2, 10.8, 13.29, 31.4, 8.8), 
  Non_Migas_Tax_Revenue = c(897.7, 1011.2, 1069.9, 1100.7, 1248.6, 1273.3, 1166.8, 1227.3, 1560.7, 1800.45),
  Migas_Tax_Revenue = c(87.4, 49.7, 36.1, 50.3, 64.7, 39.1, 31.8, 33.1, 75.4, 68.79)
)

# Data for tax comparison revenue
tax_revenue_comparison <- data.frame(
  Year = 2014:2023,
  Non_Migas_Tax_Revenue = c(897.7, 1011.2, 1069.9, 1100.7, 1248.6, 1273.3, 1166.8, 1227.3, 1560.7, 1800.45),
  Customs_and_Excise_Revenue = c(161.7, 179.6, 179.0, 192.5, 205.5, 213.4, 205.5, 213.2, 317.7, 300.1), 
  Migas_Tax_Revenue = c(87.4, 49.7, 36.1, 50.3, 64.7, 39.1, 31.8, 33.1, 75.4, 68.79)
)

# Line graph for non-migas and migas tax revenue
plot1 <- ggplot(tax_revenue, aes(x = Year)) +
  geom_line(aes(y = Non_Migas_Tax_Revenue, color = "Non-Migas Tax Revenue"), size = 1) +
  geom_line(aes(y = Migas_Tax_Revenue, color = "Migas Tax Revenue"), size = 1) +
  labs(title = "Migas & Non-Migas Tax Revenue in Numbers (2014-2023)",
       x = "Year",
       y = "Tax Revenue (Rp Trillion)") +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  theme_bw()

# Line graph for 3 main categories of tax revenue source
plot2 <- ggplot(tax_revenue_comparison, aes(x = Year)) +
  geom_line(aes(y = Non_Migas_Tax_Revenue, color = "Non-Migas Tax Revenue"), size = 1) +
  geom_line(aes(y = Migas_Tax_Revenue, color = "Migas Tax Revenue"), size = 1) +
  geom_line(aes(y = Customs_and_Excise_Revenue, color = "Customs and Excise Revenue"), size = 1) +
  labs(title = "Contribution of 3 Main Sources of Tax Revenue in Indonesia (2014-2023)",
       x = "Year",
       y = "Tax Revenue (Rp Trillion)") +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#AE4371")) +
  theme_bw()

# Analyze Trends
# Bar Graph for Total Tax Revenue
plot3 <- ggplot(tax_revenue, aes(x = Year, y = Total_Tax_Revenue)) +
  geom_bar(stat = "identity", fill = "#0072B2", color = "black") +
  labs(title = "Total Tax Revenue in Indonesia (2014-2023)",
       x = "Year",
       y = "Tax Revenue (Rp Trillion)") +
  theme_bw()

# Growth Rate of Total Tax Revenue
plot4 <- ggplot(tax_revenue, aes(x = Year, y = Total_Tax_Revenue_Growth)) +
  geom_line(stat = "identity", color = "#AA4371") +
  labs(title = "Growth Rate of Total Tax Revenue (%)", x = "Year", y = "Growth Rate (%)")

# Contribution of Each Tax Component to Total Tax Revenue
for (i in 2014:2023) {
  year_data <- tax_revenue_comparison %>% filter(Year == i)
  
  # Calculate contribution percentages
  year_data <- year_data %>% mutate(
    Non_Migas_Tax_Contribution = Non_Migas_Tax_Revenue / (Non_Migas_Tax_Revenue + Customs_and_Excise_Revenue + Migas_Tax_Revenue) * 100,
    Customs_and_Excise_Contribution = Customs_and_Excise_Revenue / (Non_Migas_Tax_Revenue + Customs_and_Excise_Revenue + Migas_Tax_Revenue) * 100,
    Migas_Tax_Contribution = Migas_Tax_Revenue / (Non_Migas_Tax_Revenue + Customs_and_Excise_Revenue + Migas_Tax_Revenue) * 100)
  }
