title: "Demographic Impact on Economic Growth"
subtitle: "Analyzing the Impact of Demographic Factors, Including Population Growth, Age Structure, and Education, on Global Economic Growth Between 2013 and 2023"
author: 
  - Alua Aldaniyaz 
  - Asmae Nakib
  - Oleksii Terletskyi 
abstract: >
  This study examined the relationship between demographic factors and GDP growth from 2013 to 2023 using panel regression models and global economic data. Population growth emerged as a statistically significant driver of economic expansion in contexts with limited alternative growth factors (coefficient: 4.51, p = 0.004), while its effect diminished in more diversified economies. Other variables, including age structure and education levels, demonstrated limited explanatory power, with statistically insignificant results across all models. Low R² values (ranging from -0.25 to 0.10) indicate that demographic factors alone are insufficient to explain GDP variability, underscoring the importance of additional economic and institutional drivers. Refinements in the analysis improved model reliability, revealing the importance of robust data preparation and methodological rigor. The findings provide a nuanced understanding of the demographic-economic relationship, highlighting population growth as a key variable in less complex economies while suggesting further exploration of structural factors in advanced contexts.


# INTRODUCTION

**Background**  
Demographic factors, such as population growth, age structure, and education levels, play a pivotal role in shaping economic development. Over the past decade (2013–2023), global economies have faced challenges ranging from demographic shifts to economic disruptions caused by the COVID-19 pandemic. Understanding how these demographic trends influence economic outcomes is essential for policymakers and researchers striving to foster sustainable growth.

**Research Questions**  
This study aims to answer the following primary research question:

**How do demographic factors influence economic growth between 2013 and 2023?**

To address this overarching question, three specific hypotheses are explored:

1. **Population Growth:** Does population growth drive economic growth?

2. **Age Structure:** How does age structure impact economic growth, and is an aging population negatively affecting it?

3. **Education Levels:** Does a more educated population correlate with higher economic growth?

**Significance of the Study**  
By examining these questions, this research seeks to uncover the intricate relationships between demographic trends and economic outcomes. The findings aim to provide insights into the long-term economic implications of demographic changes and inform evidence-based policy decisions.

# DATA

**Data and Sources**  
To address the research questions, this study utilizes data from the World Bank’s World Development Indicators (WDI), a comprehensive and widely recognized dataset. The WDI dataset offers annual, standardized measurements of demographic and economic variables, including population metrics, education levels, and GDP growth, across multiple countries. Data collection integrates inputs from national statistical offices, international organizations, and independent surveys, ensuring high reliability and accuracy.

The dataset spans a decade (2013–2023), allowing for a robust analysis of global trends. Key variables include:

**Population Growth Rate:** Annual percentage change in population size.

**Age Groups:** Proportions of populations aged 0–14, 15–64 (working age), and 65+.

**Education Metrics:** Enrollment rates and levels of educational attainment.

**GDP Growth:** Annual percentage growth in real GDP.

**Time frame:** The period from 2013 to 2023 provides a decade of data, offering a sufficient time frame to observe meaningful demographic and economic trends.  

**Countries:** The report will focus on all countries available in the dataset, to capture global trends.

```{r}
# Install and load necessary libraries
#| label: packages-data-functions
#| message: false
suppressWarnings(suppressMessages({
  packages <- c("WDI", "plm", "dplyr", "ggplot2", "knitr", "kableExtra", "corrplot", "gridExtra", "broom", "lmtest")
    # Check and install missing packages
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, quiet = TRUE)
    }
  }
  # Load packages without messages
  invisible(lapply(packages, library, character.only = TRUE))
}))
# Define the relative file path
data_file <- "data.csv"
# Check if 'data.csv' exists
if (file.exists(data_file)) {
  # Load data from the existing file
  wdi_data <- read.csv(data_file)
} else {
  # Fetch data from the World Development Indicators (WDI) database
  start_year <- 2013
  end_year <- 2023
  wdi_data <- WDI(indicator = c(
    "SP.POP.TOTL",                 # Total Population
    "NY.GDP.MKTP.KD",              # GDP (constant 2015 USD)
    "SP.POP.GROW",                 # Population Growth (%)
    "NY.GDP.MKTP.KD.ZG",           # GDP Growth (%)
    "SP.POP.0014.TO.ZS",           # Age 0-14 (% of total population)
    "SP.POP.1564.TO.ZS",           # Age 15-64 (% of total population)
    "SP.POP.65UP.TO.ZS",           # Age 65 and above (% of total population)
    "SE.PRM.ENRR",                 # Primary School Enrollment (%)
    "SE.SEC.ENRR",                 # Secondary School Enrollment (%)
    "SE.TER.ENRR",                 # Gross Enrollment Ratio (%)
    "SE.PRM.CUAT.ZS"               # Educational attainment (%)
  ), start = start_year, end = end_year, extra = TRUE)
  # Clean and rename columns
  wdi_data <- wdi_data %>%
    select(
      country, year, SP.POP.GROW, NY.GDP.MKTP.KD.ZG, SP.POP.0014.TO.ZS,
      SP.POP.1564.TO.ZS, SP.POP.65UP.TO.ZS, SE.PRM.ENRR, SE.SEC.ENRR,
      SE.PRM.CUAT.ZS, income, SP.POP.TOTL, NY.GDP.MKTP.KD
    ) %>%
    rename(
      pop_growth = SP.POP.GROW,
      gdp_growth = NY.GDP.MKTP.KD.ZG,
      age_0_14 = SP.POP.0014.TO.ZS,
      age_15_64 = SP.POP.1564.TO.ZS,
      age_65_plus = SP.POP.65UP.TO.ZS,
      primary_enrollment = SE.PRM.ENRR,
      secondary_enrollment = SE.SEC.ENRR,
      educational_attainment = SE.PRM.CUAT.ZS,
      income_level = income,
      pop_total = SP.POP.TOTL,
      gdp_real = NY.GDP.MKTP.KD
    ) %>%
    filter(
      !is.na(gdp_growth), !is.na(pop_growth),
      !is.na(age_0_14), !is.na(age_15_64),
      !is.na(age_65_plus), !is.na(educational_attainment),
      !is.na(income_level), !is.na(pop_total),
      !is.na(gdp_real)
    )
  # Save the cleaned data to a CSV file
  write.csv(wdi_data, data_file, row.names = FALSE)
}
# Create the data frame for the table
data <- data.frame(
  "Variable Name" = c(
    "Population Growth", "GDP Growth", "Age 0–14", "Age 15–64", "Age 65+",
    "Primary Enrollment", "Secondary Enrollment", "Educational Attainment",
    "Income Level", "Total Population", "Real GDP"
  ),
  "WDI Code" = c(
    "SP.POP.GROW", "NY.GDP.MKTP.KD.ZG", "SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS",
    "SP.POP.65UP.TO.ZS", "SE.PRM.ENRR", "SE.SEC.ENRR", "SE.PRM.CUAT.ZS",
    "income", "SP.POP.TOTL", "NY.GDP.MKTP.KD"
  ),
  "Significance to the Study" = c(
    "Measures annual population growth, a key factor in assessing demographic changes.",
    "Captures annual economic growth, providing insight into the relationship with demographics.",
    "Represents the percentage of the population aged 0–14, crucial for analyzing age dependency.",
    "Represents the working-age population, which impacts economic productivity and growth.",
    "Indicates the aging population, helping to assess its economic implications.",
    "Tracks primary education enrollment, linked to foundational human capital development.",
    "Tracks secondary education enrollment, reflecting skill development for economic growth.",
    "Measures the percentage of adults completing primary education, showing overall education levels.",
    "Indicates the income category of countries, providing context for economic and demographic analysis.",
    "Measures the total population, essential for understanding demographic scale and trends.",
    "Reflects the real GDP, offering a measure of economic output adjusted for inflation."
  )
)
# Create and display the styled table
kable(data) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, width = "15em", bold = TRUE, background = "#f9f9f9", extra_css = "vertical-align: middle; text-align: center;") %>%
  column_spec(2, width = "15em", extra_css = "vertical-align: middle; text-align: center;") %>%
  column_spec(3, width = "30em", italic = TRUE)
```
This combination of demographic and economic indicators enables a comprehensive exploration of the interplay between demographic shifts and economic performance.


# EXPLORATORY DATA ANALYSIS

The table above presents summary statistics for the year 2015. 
```{r}
# Perform Exploratory Data Analysis (EDA)
# Filter data for specific years
data_2015 <- wdi_data %>% filter(year == 2015)
data_2018 <- wdi_data %>% filter(year == 2018)
data_2022 <- wdi_data %>% filter(year == 2022)
# Calculate summary statistics for selected years
eda_summary <- list(
  "2015" = summary(data_2015),
  "2018" = summary(data_2018),
  "2022" = summary(data_2022)
)
# Prepare filtered summary statistics for a single year (2015)
key_metrics <- data_2015 %>%
  summarise(
    Min_GDP_Growth = min(gdp_growth, na.rm = TRUE),
    Median_GDP_Growth = median(gdp_growth, na.rm = TRUE),
    Mean_GDP_Growth = mean(gdp_growth, na.rm = TRUE),
    Max_GDP_Growth = max(gdp_growth, na.rm = TRUE),
    Min_Pop_Growth = min(pop_growth, na.rm = TRUE),
    Median_Pop_Growth = median(pop_growth, na.rm = TRUE),
    Mean_Pop_Growth = mean(pop_growth, na.rm = TRUE),
    Max_Pop_Growth = max(pop_growth, na.rm = TRUE)
  )
# Convert to a data frame for better display
key_metrics_table <- as.data.frame(t(key_metrics))
colnames(key_metrics_table) <- c("Value")
# Create a styled table
kable(key_metrics_table, caption = "Key Metrics for the Year 2015") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, width = "25em", bold = TRUE) %>%
  column_spec(2, width = "10em", italic = TRUE)
```
Similar calculations have been performed for other years (2018 and 2022), 
but they are omitted here to maintain report brevity and readability.

```{r}
# Boxplots to compare population growth and GDP growth
# Population Growth
ggplot(wdi_data, aes(x = factor(year), y = pop_growth)) +
  geom_boxplot(fill = "#adcede", color = "black") +
  labs(title = "Population Growth in 2013 to 2023") +
  theme_minimal() +
  theme(
    legend.position = "none",                # Remove legend
    plot.title = element_text(hjust = 0.5), # Center align title
    axis.title.x = element_blank(),          # Remove x-axis title
    axis.title.y = element_blank()           # Remove y-axis title
  )
```
From 2013 to 2023, the global population growth rate exhibited a steady decline, beginning at 1.24% in 2013 and reaching 0.98% by 2020. This downward trend was further accelerated by the COVID-19 pandemic, which significantly impacted global population dynamics, reducing the growth rate to 0.83% in 2021. As the effects of the pandemic began to subside, a modest recovery was observed, with the growth rate rebounding slightly to 0.88% in 2023. This trajectory underscores the pandemic's immediate disruptive influence on demographic patterns, followed by a gradual stabilization in subsequent years.

```{r}
# GDP Growth
ggplot(wdi_data, aes(x = factor(year), y = gdp_growth)) +
  geom_boxplot(fill = "#adcede", color = "black", outlier.color = "black") +
  labs(title = "GDP Growth in 2013 to 2023") +
  theme_minimal() +
  theme(
    legend.position = "none",                 # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center-align title
    axis.title = element_blank()             # Remove axis titles
  )
```
Over the past decade, global GDP growth demonstrated variability, generally oscillating between 2% and 3%. Starting at 2.87% in 2013, it peaked at 2.94% in 2019. However, the onset of the COVID-19 pandemic in 2020 triggered a severe contraction, with GDP growth plunging to -2.93% as lockdown measures and halted economic activity disrupted global markets. A swift rebound followed in 2021, with GDP growth surging to 6.26%, marking a period of recovery. Subsequently, growth stabilized within the 2%-3% range, reflecting the economic adjustments and lingering challenges of the post-pandemic environment.

```{r}
# All regressions for 10 years for all countries
# Perform calculations
regression_slopes <- wdi_data %>%
  group_by(year) %>%
  summarize(
    slope = coef(lm(gdp_growth ~ pop_growth))[2] # Extract slope (coefficient of pop_growth)
  )
regression_slopes <- regression_slopes %>%
  mutate(
    classification = ifelse(slope > 0, "positive", "negative")
  )
# Merge classifications back into the dataset
wdi_data_classified <- wdi_data %>%
  inner_join(regression_slopes, by = "year")
# Filter for three key trends: overall (central), maximum growth, and negative growth
central_trend <- wdi_data_classified %>%
  filter(year %in% c(2015, 2016, 2018)) # Example of years for central trend
max_growth_trend <- wdi_data_classified %>%
  filter(year == 2020) # Example of maximum growth
negative_growth_trend <- wdi_data_classified %>%
  filter(year == 2021) # Example of negative growth
# Create the plot with a custom legend
ggplot() +
  # Central trend
  geom_smooth(data = central_trend, aes(x = pop_growth, y = gdp_growth, color = "Central Trend"),
              method = "lm", se = TRUE, formula = y ~ x, fill = "gray80", linetype = "solid") +
  # Maximum growth trend
  geom_smooth(data = max_growth_trend, aes(x = pop_growth, y = gdp_growth, color = "Maximum Growth (2020)"),
              method = "lm", se = FALSE, formula = y ~ x, linewidth = 0.5, linetype = "solid") +
  # Negative growth trend
  geom_smooth(data = negative_growth_trend, aes(x = pop_growth, y = gdp_growth, color = "Negative Growth (2021)"),
              method = "lm", se = FALSE, formula = y ~ x, linewidth = 0.5, linetype = "solid") +
  labs(
    title = "Population Growth vs GDP Growth (Key Trends)",
    x = "Population Growth (%)",
    y = "GDP Growth (%)",
    color = "Trend"
  ) +
  scale_color_manual(
    values = c("Central Trend" = "gray50", "Maximum Growth (2020)" = "#497387", "Negative Growth (2021)" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",               # Move legend to the bottom
    plot.title = element_text(hjust = 0.5),  # Center-align title
    legend.title = element_text(face = "bold"), # Bold legend title
    legend.text = element_text(size = 10)    # Adjust legend text size
  )
```
Positive regression coefficients observed across most years indicate a general positive relationship between population and GDP growth. Higher population growth often correlates with an expanding labor force, increased demand for goods and services, and stimulated economic production. Investments to support a growing population further boost economic activity.

In contrast, 2021 shows a negative regression coefficient, reflecting the COVID-19 pandemic's extraordinary impact. Widespread lockdowns, trade disruptions, and halted production decoupled GDP growth from population trends. While population growth remained steady, economic contraction in 2021 created a temporary negative correlation, driven by external health and economic crises.

```{r}
# Calculate regression slopes by year
regression_slopes <- wdi_data %>%
  group_by(year) %>%
  summarize(
    slope = coef(lm(gdp_growth ~ pop_growth))[2] # Extract slope (coefficient of pop_growth)
  )
# Create density plot with annotations
ggplot(regression_slopes, aes(x = slope)) +
  geom_density(fill = "#497387", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = 0.5, y = 1.4, label = "Positive relationship:\npopulation growth boosts GDP", size = 3, fontface = "italic", color = "black") +
  annotate("text", x = -0.24, y = 0.2, label = "Negative relationship:\npopulation growth slows economy", size = 3, fontface = "italic", color = "black") +
  labs(
    title = "Distribution of Regression Slopes (Population Growth vs GDP Growth)",
    x = "Regression Slope",
    y = "Density"
  ) +
  xlim(-0.4, NA) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
```
The graph illustrates the distribution of regression coefficients, representing the relationship between population growth and GDP growth over the study period. The peak around 0.3 indicates that population growth was most commonly associated with a moderate positive impact on the economy. The presence of negative values to the left of zero highlights rare instances where population growth slowed economic growth, likely due to external factors such as crises.

```{r}
# Calculate mean and standard deviation for population growth and GDP growth
stats <- wdi_data %>%
  summarize(
    mean_pop_growth = mean(pop_growth, na.rm = TRUE),
    sd_pop_growth = sd(pop_growth, na.rm = TRUE),
    mean_gdp_growth = mean(gdp_growth, na.rm = TRUE),
    sd_gdp_growth = sd(gdp_growth, na.rm = TRUE)
  )
# Define the ranges for 1, 2, and 3 sigma
sigma_ranges <- data.frame(
  Metric = c("Population Growth", "GDP Growth"),
  `1 Sigma` = c(
    paste0(round(stats$mean_pop_growth - stats$sd_pop_growth, 2), " to ", 
           round(stats$mean_pop_growth + stats$sd_pop_growth, 2)),
    paste0(round(stats$mean_gdp_growth - stats$sd_gdp_growth, 2), " to ", 
           round(stats$mean_gdp_growth + stats$sd_gdp_growth, 2))
  ),
  `2 Sigma` = c(
    paste0(round(stats$mean_pop_growth - 2 * stats$sd_pop_growth, 2), " to ", 
           round(stats$mean_pop_growth + 2 * stats$sd_pop_growth, 2)),
    paste0(round(stats$mean_gdp_growth - 2 * stats$sd_gdp_growth, 2), " to ", 
           round(stats$mean_gdp_growth + 2 * stats$sd_gdp_growth, 2))
  ),
  `3 Sigma` = c(
    paste0(round(stats$mean_pop_growth - 3 * stats$sd_pop_growth, 2), " to ", 
           round(stats$mean_pop_growth + 3 * stats$sd_pop_growth, 2)),
    paste0(round(stats$mean_gdp_growth - 3 * stats$sd_gdp_growth, 2), " to ", 
           round(stats$mean_gdp_growth + 3 * stats$sd_gdp_growth, 2))
  )
)
# Load necessary libraries for table output
library(knitr)
library(kableExtra)
# Display the sigma ranges table
kable(sigma_ranges, caption = "Ranges for 1, 2, and 3 Sigma for Population and GDP Growth") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))
```
This table shows that most of the data (68.3%) on population and GDP growth rates are concentrated in narrow ranges close to the mean, ranging from 1% for population to 2% for GDP. Most economic changes occurred within predictable ranges, suggesting stable trends. However, the extended ranges for 95.5% and 99.7% of observations indicate rare but significant deviations reflecting the influence of external factors such as crises or economic spikes.

```{r}
# Load necessary libraries
suppressWarnings(library(tidyr))
suppressWarnings(library(knitr))
suppressWarnings(library(kableExtra))
# Define variables for correlation analysis
variables <- c("pop_growth", "age_65_plus", "educational_attainment")
# Calculate correlations
correlations <- wdi_data %>% 
  group_by(year) %>%
  summarize(
    across(all_of(variables), ~ cor(.x, gdp_growth, use = "complete.obs"), .names = "cor_{.col}")
  ) %>%
  pivot_longer(-year, names_to = "variable", values_to = "correlation")

# Display first 10 rows of the correlations table
correlations %>%
  head(10) %>%
  kable(caption = "Correlations of Variables with GDP Growth (First 10 Rows)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, width = "5em") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "10em")
```
This table presents the correlation between key variables (e.g., population growth, percentage of people aged 65 and older, and educational attainment) and GDP growth across different years. For instance, in 2013, population growth had a positive correlation with GDP growth (0.352), indicating a supportive relationship, while the share of people over 65 showed a negative correlation (-0.574), suggesting an opposing effect. This analysis highlights how demographic and educational factors influenced economic growth over time, helping to identify which variables played significant roles in different periods.

```{r}
# load the necessary package
suppressWarnings(library(corrplot))

# Prepare the correlation matrix for year and variables
cor_matrix <- correlations %>%
  pivot_wider(names_from = year, values_from = correlation) %>%
  select(-variable) %>%
  as.matrix()

# Assign row names as variables
rownames(cor_matrix) <- unique(correlations$variable)

# Plot the correlation matrix with specified parameters
corrplot(
  cor_matrix,
  method = "number",           # Use numbers instead of circles
  tl.cex = 0.7,                # Text size for labels
  tl.col = "black",            # Label color
  tl.srt = 90,                 # Rotate top labels for better readability
  cl.cex = 0.7,                # Color legend text size
  col = colorRampPalette(c("darkblue", "white", "darkred"))(200)  # Swap colors
)
```

**Population Growth and GDP Growth**  
Population growth generally shows a positive but moderate correlation with GDP growth, typically under 0.5, indicating that while population expansion supports economic growth, its influence is limited. In 2021, the correlation turned negative, reflecting the economic disruptions caused by the COVID-19 pandemic, such as lockdowns and decreased productivity. This anomaly highlights the pandemic's overriding impact on economic dynamics, despite continued population growth.

**Educational Attainment and GDP Growth**  
Educational attainment generally shows a moderately negative correlation with GDP growth, typically above -0.5, suggesting that initial investments in education may slow economic growth due to associated costs. However, these short-term effects are often offset by long-term gains in human capital, productivity, and economic expansion. In 2021 and 2022, this relationship turned slightly positive, likely reflecting the increased demand for skilled labor driven by the adoption of digital technologies and remote work during the pandemic.

**Age Structure and GDP Growth**  
The negative correlation between age structure and GDP growth indicates that an aging population is generally linked to slower economic expansion. This is often due to a shrinking labor force and potentially lower overall productivity. In 2021, however, the correlation became positive, possibly reflecting increased healthcare spending during the pandemic or a temporary rise in labor force participation among older individuals driven by shifting economic conditions.


# COMBINED CROSS-SECTIONAL ANALYSIS
We will conduct a cross-sectional analysis for the years 2015, 2018, and 2022, sequentially adding variables to each year. We will compare these findings with those from a panel data model to assess the consistency and robustness of our results.

## GDP Growth and Population, Aging and Educatioal 2015-2018-2022
### Population Growth and GDP Growth 2015-2018-2022
```{r}
library(ggplot2)
library(dplyr)
library(gridExtra)

# Filter data for years 2015, 2018, and 2022
data_2015 <- wdi_data %>% filter(year == 2015)
data_2018 <- wdi_data %>% filter(year == 2018)
data_2022 <- wdi_data %>% filter(year == 2022)

# Calculate correlations
cor_2015_pop <- cor(data_2015$pop_growth, data_2015$gdp_growth, use = "complete.obs")
cor_2018_pop <- cor(data_2018$pop_growth, data_2018$gdp_growth, use = "complete.obs")
cor_2022_pop <- cor(data_2022$pop_growth, data_2022$gdp_growth, use = "complete.obs")

# Scatter plot for 2015
plot_2015 <- ggplot(data_2015, aes(x = pop_growth, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2015$pop_growth, na.rm = TRUE), y = max(data_2015$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2015_pop, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = "GDP Growth vs Population Growth (2015, 2018, 2022)",
    x = NULL,
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Scatter plot for 2018
plot_2018 <- ggplot(data_2018, aes(x = pop_growth, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2018$pop_growth, na.rm = TRUE), y = max(data_2018$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2018_pop, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = NULL,
    x = NULL,
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
# Scatter plot for 2022
plot_2022 <- ggplot(data_2022, aes(x = pop_growth, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2022$pop_growth, na.rm = TRUE), y = max(data_2022$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2022_pop, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = NULL,
    x = "Population Growth (%)",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
# Arrange the plots in a single block
grid.arrange(plot_2015, plot_2018, plot_2022, nrow = 3)
```
In 2015, 1% of population growth is associated with 0.32% increase in GDP and this relationship is statistically significant (p-value = 0.0377) with correlation coefficient (19.9%), indicating a moderate impact of demography on economic growth.

In 2018, 1% population growth is associated with 0.4776% increase in GDP and the relationship is statistically significant (p-value = 0.0296) with correlation coefficient (27.1%), indicating a moderately positive effect of demography on economic growth.

In 2022, 1% population growth is associated with 0.5479% increase in GDP and this relationship is statistically significant (p-value = 0.0481) with a correlation coefficient (18.3%), indicating a weak but positive effect of demography on economic growth.

Population growth has a positive but weak effect on GDP growth, explaining only a small part of its variation. The correlation coefficients range from 18.3% to 27.1%, indicating that the bulk of the changes in GDP (about 70% to 80%) are explained by other factors not taken into account in the study. At the same time, statistically significant correlations confirm the moderate influence of demography on economic growth.

### Aging Population and GDP Growth 2015-2018-2022
```{r}
library(ggplot2)
library(dplyr)
library(gridExtra)
# Filter data for years 2015, 2018, and 2022
data_2015 <- wdi_data %>% filter(year == 2015)
data_2018 <- wdi_data %>% filter(year == 2018)
data_2022 <- wdi_data %>% filter(year == 2022)
# Calculate correlations for Aging Population and GDP Growth
cor_2015_age <- cor(data_2015$age_65_plus, data_2015$gdp_growth, use = "complete.obs")
cor_2018_age <- cor(data_2018$age_65_plus, data_2018$gdp_growth, use = "complete.obs")
cor_2022_age <- cor(data_2022$age_65_plus, data_2022$gdp_growth, use = "complete.obs")
# Scatter plot for 2015
plot_2015 <- ggplot(data_2015, aes(x = age_65_plus, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2015$age_65_plus, na.rm = TRUE), y = max(data_2015$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2015_age, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = "GDP Growth vs Aging Population (2015, 2018, 2022)",
    x = NULL,
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Scatter plot for 2018
plot_2018 <- ggplot(data_2018, aes(x = age_65_plus, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2018$age_65_plus, na.rm = TRUE), y = max(data_2018$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2018_age, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = NULL,
    x = NULL,
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
# Scatter plot for 2022
plot_2022 <- ggplot(data_2022, aes(x = age_65_plus, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2022$age_65_plus, na.rm = TRUE), y = max(data_2022$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2022_age, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = NULL,
    x = "Population Aged 65+ (%)",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
# Arrange the plots in a single block
grid.arrange(plot_2015, plot_2018, plot_2022, nrow = 3)
```
Analysis of the relationship between the share of the elderly population (age 65+) and GDP growth shows a weak negative relationship in all years under study. The correlation coefficients were -0.186 in 2015, -0.1518 in 2018 and -0.0482 in 2022, indicating a small inhibiting effect of the increase in the share of elderly population on economic growth. In 2015 and 2018, the correlation was still prominent, which could be due to the increasing pressure on social and pension systems. However, in 2022 the correlation became almost insignificant, indicating a minimal impact of population aging, probably related to the adaptation of economies to demographic changes.

The regression coefficients show that as the proportion of the elderly population increased by 1%, GDP growth decreased by 0.06299% in 2015, 0.04812% in 2018, and 0.07319% in 2022. This confirms that the impact of aging on the economy is measurable, even if small. Moreover, in all years, the relationship was statistically significant (p-value < 0.05), confirming the existence of this effect.

The figures demonstrate that population aging has a weak but statistically confirmed negative effect on GDP growth. At the same time, its influence decreases over time, which may be due to the adaptation of economies or the dominant influence of other factors. The main conclusion is that population ageing is not a key driver of GDP changes, but it creates an additional burden, which requires adaptation measures on the part of the economy.

### Educational Attainment and GDP Growth 2015-2018-2022
```{r}
library(ggplot2)
library(dplyr)
library(gridExtra)
# Filter data for years 2015, 2018, and 2022
data_2015 <- wdi_data %>% filter(year == 2015)
data_2018 <- wdi_data %>% filter(year == 2018)
data_2022 <- wdi_data %>% filter(year == 2022)
# Calculate correlations for Educational Attainment and GDP Growth
cor_2015_edu <- cor(data_2015$educational_attainment, data_2015$gdp_growth, use = "complete.obs")
cor_2018_edu <- cor(data_2018$educational_attainment, data_2018$gdp_growth, use = "complete.obs")
cor_2022_edu <- cor(data_2022$educational_attainment, data_2022$gdp_growth, use = "complete.obs")
# Scatter plot for 2015
plot_2015 <- ggplot(data_2015, aes(x = educational_attainment, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2015$educational_attainment, na.rm = TRUE), y = max(data_2015$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2015_edu, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = "GDP Growth vs Educational Attainment (2015, 2018, 2022)",
    x = NULL,
    y = "GDP Growth (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Scatter plot for 2018
plot_2018 <- ggplot(data_2018, aes(x = educational_attainment, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2018$educational_attainment, na.rm = TRUE), y = max(data_2018$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2018_edu, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = NULL,
    x = NULL,
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
# Scatter plot for 2022
plot_2022 <- ggplot(data_2022, aes(x = educational_attainment, y = gdp_growth)) +
  geom_point(size = 0.5, color = "black") +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "#497387") +
  annotate("text", x = max(data_2022$educational_attainment, na.rm = TRUE), y = max(data_2022$gdp_growth, na.rm = TRUE),
           label = paste0("Correlation: ", round(cor_2022_edu, 3)), hjust = 1, vjust = 1, size = 3, color = "blue") +
  labs(
    title = NULL,
    x = "Tertiary Enrollment (%)",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()
# Arrange the plots in a single block
grid.arrange(plot_2015, plot_2018, plot_2022, nrow = 3)
```
In 2015, a higher level of education shows a weak negative relationship with GDP growth (correlation coefficient: -0.173). The regression coefficient of -0.0082 shows that a 1% increase in education level is associated with a 0.0082% decrease in GDP growth, but this relationship is not statistically significant (p-value = 0.7129). This indicates a minimal impact of education, possibly related to the short-term economic costs of expansion. In 2018, the relationship becomes moderately negative, with a correlation coefficient of -0.3198. The regression coefficient is -0.01719, meaning that a 1% increase in education is associated with a 0.01719% decrease in GDP. However, this relationship is also not statistically significant (p-value = 0.2412), reflecting possible economic adaptations to investment in education. By 2022, the relationship between education and GDP becomes weakly positive, although it remains insignificant (correlation coefficient less than 0.5). This change may reflect the long-term economic benefits of a more highly skilled labor force in the post-pandemic period of digital transformation. Despite the positive shift, the effect of education on GDP growth in these years remains statistically insignificant. 
The final results indicate that education, despite its importance for long-term growth, has a limited short-term impact on GDP, which may be overridden by other dominant factors.


## Coefficients and Summary Statistics of Linear Models 2015-2018-2022
### Population Growth and GDP Growth 2015-2018-2022
```{r}
library(broom)
library(dplyr)
library(kableExtra)
# Fit models for each year
model_pop_2015 <- lm(gdp_growth ~ pop_growth, data = data_2015)
model_pop_2018 <- lm(gdp_growth ~ pop_growth, data = data_2018)
model_pop_2022 <- lm(gdp_growth ~ pop_growth, data = data_2022)
# Extract detailed summaries for each year
extract_full_summary <- function(model, year) {
  tidy_model <- tidy(model) %>% mutate(Year = year)
  glance_model <- glance(model) %>%
    mutate(Year = year,
           Residuals = paste("Min:", signif(min(residuals(model)), 3), 
                             "1Q:", signif(quantile(residuals(model), 0.25), 3), 
                             "Median:", signif(median(residuals(model)), 3),
                             "3Q:", signif(quantile(residuals(model), 0.75), 3),
                             "Max:", signif(max(residuals(model)), 3)))
  list(tidy = tidy_model, glance = glance_model)
}
summary_2015 <- extract_full_summary(model_pop_2015, 2015)
summary_2018 <- extract_full_summary(model_pop_2018, 2018)
summary_2022 <- extract_full_summary(model_pop_2022, 2022)
# Combine all data into a single data frame
all_summaries <- bind_rows(
  summary_2015$tidy,
  summary_2018$tidy,
  summary_2022$tidy
)
all_glances <- bind_rows(
  summary_2015$glance,
  summary_2018$glance,
  summary_2022$glance
)
# Create a table for coefficients
coeff_table <- all_summaries %>%
  select(Year, term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Coefficient = estimate,
    `Std. Error` = std.error,
    `t Value` = statistic,
    `p Value` = p.value
  )
# Create a table for model statistics
stats_table <- all_glances %>%
  select(Year, Residuals, r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  rename(
    `R-squared` = r.squared,
    `Adjusted R-squared` = adj.r.squared,
    `Residual Std. Error` = sigma,
    `F-statistic` = statistic,
    `Model p-value` = p.value
  )
# Combine both tables
coeff_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Coefficients for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
stats_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Statistics for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
```
Population growth has a weak positive relationship with GDP growth in all three years, as confirmed by the coefficients (2015: 0.44%, 2018: 0.48%, 2022: 0.55% GDP growth per 1% population growth) and statistical significance (p-value < 0.05). However, the low R² values (3.35%-7.35%) show that only a small part of the GDP change is explained by population growth, and the main impact is due to other factors not accounted for in the model.

The figures in the table confirm the earlier conclusions that the impact of demographic growth on economic growth is limited and requires analysis of additional variables for a full understanding.

### Population Growth,GDP Growth and Income Level 2015-2018-2022
```{r}
library(broom)
library(dplyr)
library(kableExtra)
# Fit models with additional variable for 2015, 2018, and 2022
model_pop_2015_2 <- lm(gdp_growth ~ pop_growth + income_level, data = data_2015)
model_pop_2018_2 <- lm(gdp_growth ~ pop_growth + income_level, data = data_2018)
model_pop_2022_2 <- lm(gdp_growth ~ pop_growth + income_level, data = data_2022)
# Extract detailed summaries for each model
extract_full_summary <- function(model, year) {
  tidy_model <- tidy(model) %>% mutate(Year = year)
  glance_model <- glance(model) %>%
    mutate(Year = year,
           Residuals = paste("Min:", signif(min(residuals(model)), 3), 
                             "1Q:", signif(quantile(residuals(model), 0.25), 3), 
                             "Median:", signif(median(residuals(model)), 3),
                             "3Q:", signif(quantile(residuals(model), 0.75), 3),
                             "Max:", signif(max(residuals(model)), 3)))
  list(tidy = tidy_model, glance = glance_model)
}
# Extract summaries for all years
summary_2015 <- extract_full_summary(model_pop_2015_2, 2015)
summary_2018 <- extract_full_summary(model_pop_2018_2, 2018)
summary_2022 <- extract_full_summary(model_pop_2022_2, 2022)
# Combine all data into tables
all_summaries <- bind_rows(
  summary_2015$tidy,
  summary_2018$tidy,
  summary_2022$tidy
)
all_glances <- bind_rows(
  summary_2015$glance,
  summary_2018$glance,
  summary_2022$glance
)
# Create a table for coefficients
coeff_table <- all_summaries %>%
  select(Year, term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Coefficient = estimate,
    `Std. Error` = std.error,
    `t Value` = statistic,
    `p Value` = p.value
  )
# Create a table for model statistics
stats_table <- all_glances %>%
  select(Year, Residuals, r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  rename(
    `R-squared` = r.squared,
    `Adjusted R-squared` = adj.r.squared,
    `Residual Std. Error` = sigma,
    `F-statistic` = statistic,
    `Model p-value` = p.value
  )
# Display tables
coeff_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Coefficients with Income Level for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
stats_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Statistics with Income Level for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
```
Income level exhibits a varying and inconsistent relationship with GDP growth across the three years. In 2015, while the coefficients for income levels (Low: 0.959, Lower Middle: 0.490, Upper Middle: -0.391) suggest slight positive and negative associations with GDP growth, the p-values (all > 0.05) indicate that these relationships are statistically insignificant. The R-squared value of 5.3% demonstrates that only a small proportion of GDP variability is explained by the model.

In 2018, the impact of income level becomes more pronounced, with the coefficient for Low Income (1.876) being statistically significant (p-value = 0.014), indicating a meaningful positive association. However, the coefficients for other income levels remain insignificant, and the R-squared of 12.5% suggests the model captures more variability than in 2015 but still leaves a substantial portion unexplained.

By 2022, the coefficients for all income levels, including Low Income (-2.038), are statistically insignificant (all p-values > 0.05). The R-squared drops to 9.5%, further emphasizing the limited explanatory power of income level and population growth combined in this context. The low adjusted R-squared values across all years reinforce that other variables not included in the models are the primary drivers of GDP changes.

These results confirm that while income level occasionally shows a statistically significant association with GDP growth (e.g., Low Income in 2018), its overall impact is limited and inconsistent. This highlights the necessity of incorporating additional variables, such as education and age distribution, to provide a more comprehensive understanding of economic growth determinants.

### Population Growth,GDP Growth, Income Level and Age Structure 2015-2018-2022
```{r}
library(broom)
library(dplyr)
library(kableExtra)
# Fit models for each year with Population Growth, Income Level, Aging Population, and Educational Attainment
model_pop_2015_3 <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus + educational_attainment, data = data_2015)
model_pop_2018_3 <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus + educational_attainment, data = data_2018)
model_pop_2022_3 <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus + educational_attainment, data = data_2022)
# Extract detailed summaries for each year
extract_full_summary <- function(model, year) {
  tidy_model <- tidy(model) %>% mutate(Year = year)
  glance_model <- glance(model) %>%
    mutate(Year = year,
           Residuals = paste("Min:", signif(min(residuals(model)), 3), 
                             "1Q:", signif(quantile(residuals(model), 0.25), 3), 
                             "Median:", signif(median(residuals(model)), 3),
                             "3Q:", signif(quantile(residuals(model), 0.75), 3),
                             "Max:", signif(max(residuals(model)), 3)))
  list(tidy = tidy_model, glance = glance_model)
}
summary_2015 <- extract_full_summary(model_pop_2015_3, 2015)
summary_2018 <- extract_full_summary(model_pop_2018_3, 2018)
summary_2022 <- extract_full_summary(model_pop_2022_3, 2022)
# Combine all data into a single data frame
all_summaries <- bind_rows(
  summary_2015$tidy,
  summary_2018$tidy,
  summary_2022$tidy
)
all_glances <- bind_rows(
  summary_2015$glance,
  summary_2018$glance,
  summary_2022$glance
)
# Create a table for coefficients
coeff_table <- all_summaries %>%
  select(Year, term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Coefficient = estimate,
    `Std. Error` = std.error,
    `t Value` = statistic,
    `p Value` = p.value
  )
# Create a table for model statistics
stats_table <- all_glances %>%
  select(Year, Residuals, r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  rename(
    `R-squared` = r.squared,
    `Adjusted R-squared` = adj.r.squared,
    `Residual Std. Error` = sigma,
    `F-statistic` = statistic,
    `Model p-value` = p.value
  )
# Display the tables
coeff_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Coefficients with Population Growth, Income Level, Aging Population, and Educational Attainment for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
stats_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Statistics with Population Growth, Income Level, Aging Population, and Educational Attainment for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
```
The combined model, which includes population growth, income level, and the share of the population aged 65+, provides insights into the limited explanatory power of these variables on GDP growth across the years. In 2015, population growth has a negligible impact on GDP growth, with a coefficient of 0.173 and a p-value of 0.615, indicating no statistical significance. Income level coefficients for the same year also lack statistical significance, while the aging population coefficient (-0.063) suggests a weak negative effect, though not significant. The R-squared value of 5.6% confirms that the model explains only a small portion of GDP variability.

In 2018, the model demonstrates slightly improved explanatory power with an R-squared value of 15.6%. Population growth shows a statistically significant coefficient of 0.528 (p-value = 0.015), indicating a more pronounced impact compared to 2015. Income levels (e.g., Low Income: 2.614, p-value = 0.002) exhibit stronger and significant contributions to GDP growth. However, the effect of the aging population (0.100, p-value = 0.047) remains minimal despite achieving significance.

In 2022, population growth maintains a positive impact on GDP growth, with a coefficient of 1.020 and a statistically significant p-value of 0.031. However, the effects of income levels and aging population become statistically insignificant, with R-squared declining to 9.6%, highlighting a reduced explanatory capacity of the model.

Overall, the combined model illustrates that while population growth and certain income levels influence GDP growth, their contributions are modest, and the aging population's impact remains minimal or statistically insignificant across all years. These findings confirm that the majority of GDP growth variability stems from other unaccounted factors, necessitating broader analyses to capture the full dynamics.

### Population Growth, Income Level, Age Structure and Educational Attainment  2015-2018-2022
```{r}
library(broom)
library(dplyr)
library(kableExtra)
# Fit models for each year with Population Growth, Income Level, Aging Population, and Educational Attainment
model_pop_2015_4 <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus + educational_attainment, data = data_2015)
model_pop_2018_4 <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus + educational_attainment, data = data_2018)
model_pop_2022_4 <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus + educational_attainment, data = data_2022)

# Extract detailed summaries for each year
extract_full_summary <- function(model, year) {
  tidy_model <- tidy(model) %>% mutate(Year = year)
  glance_model <- glance(model) %>%
    mutate(Year = year,
           Residuals = paste("Min:", signif(min(residuals(model)), 3), 
                             "1Q:", signif(quantile(residuals(model), 0.25), 3), 
                             "Median:", signif(median(residuals(model)), 3),
                             "3Q:", signif(quantile(residuals(model), 0.75), 3),
                             "Max:", signif(max(residuals(model)), 3)))
  list(tidy = tidy_model, glance = glance_model)
}
summary_2015 <- extract_full_summary(model_pop_2015_4, 2015)
summary_2018 <- extract_full_summary(model_pop_2018_4, 2018)
summary_2022 <- extract_full_summary(model_pop_2022_4, 2022)
# Combine all data into a single data frame
all_summaries <- bind_rows(
  summary_2015$tidy,
  summary_2018$tidy,
  summary_2022$tidy
)
all_glances <- bind_rows(
  summary_2015$glance,
  summary_2018$glance,
  summary_2022$glance
)
# Create a table for coefficients
coeff_table <- all_summaries %>%
  select(Year, term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Coefficient = estimate,
    `Std. Error` = std.error,
    `t Value` = statistic,
    `p Value` = p.value
  )
# Create a table for model statistics
stats_table <- all_glances %>%
  select(Year, Residuals, r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  rename(
    `R-squared` = r.squared,
    `Adjusted R-squared` = adj.r.squared,
    `Residual Std. Error` = sigma,
    `F-statistic` = statistic,
    `Model p-value` = p.value
  )
# Display the tables
coeff_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Coefficients with Population Growth, Income Level, Aging Population, and Educational Attainment for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

stats_table %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Linear Model Statistics with Population Growth, Income Level, Aging Population, and Educational Attainment for 2015, 2018, and 2022"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
```
The extended model, including population growth, income levels, aging population, and educational attainment, shows limited explanatory power for GDP growth across all three years.

In 2015, population growth has a negligible effect (coefficient = 0.164, p-value = 0.637), and the coefficients for income levels, aging population, and educational attainment are statistically insignificant (e.g., educational attainment = -0.008, p-value = 0.713). The R-squared value of 5.8% indicates the model explains only a small fraction of GDP variability.

In 2018, the model improves slightly with an R-squared of 16.6%. Population growth has a moderate, statistically significant effect (coefficient = 0.478, p-value = 0.030), and the aging population shows a weak positive effect (coefficient = 0.112, p-value = 0.022). However, income levels and educational attainment remain mostly insignificant.

In 2022, the R-squared drops to 10.0%. Population growth retains significance (coefficient = 1.010, p-value = 0.033), but the other variables, including income levels and aging population, are statistically insignificant.

Overall, the model confirms that population growth is the most consistent factor influencing GDP growth, though its impact is modest. The other variables show inconsistent and limited effects, emphasizing the need for additional factors to better explain GDP variability.

## CHECK MODEL DIAGNOSTICS
For each model, generate diagnostic plots to ensure that assumptions of linear regression are met:  
- Residuals vs Fitted: Check for non-linearity.  
- QQ Plot: Check for normality of residuals.  
- Scale-Location Plot: Check for homoskedasticity.  
- Residuals vs Leverage: Identify influential points.

```{r}
# Diagnostic plots for Population Growth model
par(mfrow = c(2, 2))
plot(model_pop_2015)
par(mfrow = c(1, 1))
# Diagnostic plots for Aging Population model
par(mfrow = c(2, 2))
plot(model_pop_2015_2)
par(mfrow = c(1, 1))
# Diagnostic plots for Education Level model
par(mfrow = c(2, 2))
plot(model_pop_2015_3)
par(mfrow = c(1, 1))
par(mfrow = c(2, 2))
plot(model_pop_2015_4)
par(mfrow = c(1, 1))
```

The following conclusions can be drawn from the graphs presented:

Residuals vs Fitted (linearity test):
The graphs show that the residuals are mostly distributed uniformly around the zero line, which confirms the acceptable linearity of the model. However, outliers are observed, such as points 47, 51 and 81, which may violate linearity.

QQ Plot (checking the normality of the residuals):
The residuals mostly follow the theoretical normal line, but outliers are observed in the tails of the distribution in both cases (e.g. points 47 and 51). This indicates that the normality of the residuals is partially broken.

Scale-Location Plot (Homoscedasticity):
The plots show that the variance of the residuals remains roughly constant along the range of predicted values, except for outliers (points 47, 51). This indicates that homoscedasticity is generally observed.
Residuals vs Leverage (outliers and influencing points):

The graphs reveal high influence points (47, 51, 81), especially in the model for population growth. The Cook's distance coefficients for these points exceed the 0.5 threshold, making them significant outliers.

Conclusion:
The models generally satisfy the basic assumptions of linear regression (linearity, homoscedasticity), but the outliers identified (47, 51, 81) require further removal.

```{r}
# Load necessary libraries
library(knitr)
library(kableExtra)

# Remove rows 47, 51, and 81 by subsetting
data_cleaned <- data_2015[-c(47, 51, 81), ]
removed_data <- data_2015[c(47, 51, 81), ]

# Display removed data in a formatted table
kable(removed_data, format = "html", table.attr = "style='width:50%; margin-left:auto; margin-right:auto;'") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```
The table contains data from the outlier countries that were removed from the analysis: Ireland, Kiribati and Qatar. These countries are highlighted by extreme values, such as Qatar's population growth rate of 8.58% and Ireland's GDP growth rate of 24.6%, as well as specific age structure and economic characteristics. Removing these points improves the quality of the model by removing the influence of anomalous data.

```{r}
# Load necessary libraries
library(broom)
library(knitr)
library(kableExtra)

# Fit the models
model_cleaned <- lm(gdp_growth ~ pop_growth + income_level + age_65_plus, data = data_cleaned)
model_cleaned_pop <- lm(gdp_growth ~ pop_growth, data = data_cleaned)

# Summarize the models
summary_cleaned <- tidy(model_cleaned)
summary_cleaned_pop <- tidy(model_cleaned_pop)

# Create tables
cat("\nModel: gdp_growth ~ pop_growth + income_level + age_65_plus\n")
kable(summary_cleaned, format = "html", table.attr = "style='width:80%; margin-left:auto; margin-right:auto;'") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

cat("\nModel: gdp_growth ~ pop_growth\n")
kable(summary_cleaned_pop, format = "html", table.attr = "style='width:80%; margin-left:auto; margin-right:auto;'") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```
The first model shows that none of the factors (pop_growth, income_level, age_65_plus) has a statistically significant effect on GDP growth, e.g., for pop_growth p = 0.181. The second model shows that population growth has a significant positive effect (coefficient = 0.545, p = 0.0018), increasing GDP by 0.545% for a population growth of 1%. The intersept of the second model (2.40, p < 0.001) reflects the average level of GDP growth without the influence of population growth. The overall conclusion is that population growth is important for GDP, but the other included variables explain little of its variation.

Upon analyzing the multiple linear regression model, we observe that removing outliers has improved the model's performance. When comparing the full model, which includes all variables, to a simpler model that only considers population growth, we find that population growth significantly influences GDP growth. However, the model is not solely explained by this variable. The inclusion of other parameters does not enhance the model's explanatory power; instead, they introduce noise and do not have a significant effect on GDP growth. This suggests that while population growth is a key determinant of GDP growth, other factors may not be as impactful in this context. The presence of non-significant variables can lead to overfitting, where the model captures random noise rather than underlying patterns. Therefore, it is advisable to focus on variables that have a meaningful and statistically significant relationship with the dependent variable to improve model accuracy and interpretability.

```{r}
#model with all variables 
par(mfrow = c(2, 2))
plot(model_cleaned)
par(mfrow = c(1, 1))

#model with gdp ~ pop growth 
par(mfrow = c(2, 2))
plot(model_cleaned_pop)
par(mfrow = c(1, 1))
```
The graphs show that the model generally meets key statistical assumptions, including linearity, normality of residuals and homoscedasticity, which confirms its suitability for analysis and forecasting. However, deviations of the residuals at the edges of the distribution indicate possible limitations of the model for extreme values of the predictors, which may reduce accuracy in these areas. Points with high influence, such as 87 and 610, although identified as outliers, do not significantly distort the model (Cook's distance < 0.5), indicating its robustness. At the same time, further work with outliers and analysis of anomalous observations can improve the accuracy and explanatory power of the model.


## Panel Model
In this section, factors that do not have a significant impact on GDP, including the share of the elderly population (age_65_plus) and educational attainment (educational_attainment), were excluded from further analysis. The analysis showed that the coefficients for these variables remained low in all models, for example, for age_65_plus the coefficients ranged from -0.18 to 0.20 with p-values above 0.24, making them statistically insignificant. Education level also showed a weak effect with coefficients around 0.05 and p-values above 0.39, indicating no association with GDP growth in the short run. Excluding these factors allowed us to focus on population growth, which shows a significant impact, especially in low-income countries, and simplify the interpretation of the model.

```{r}
# Load necessary libraries
library(plm)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

# Convert to pdata.frame for panel data
panel_data <- pdata.frame(wdi_data, index = c("country", "year"))

# Build fixed effects model for population growth
fixed_model <- plm(gdp_growth ~ pop_growth, data = panel_data, model = "within")

# Summarize results for groups by income level
low_income_data <- pdata.frame(wdi_data %>% filter(income_level == "Low income"), index = c("country", "year"))
lower_middle_income_data <- pdata.frame(wdi_data %>% filter(income_level == "Lower middle income"), index = c("country", "year"))
upper_middle_income_data <- pdata.frame(wdi_data %>% filter(income_level == "Upper middle income"), index = c("country", "year"))
high_income_data <- pdata.frame(wdi_data %>% filter(income_level == "High income"), index = c("country", "year"))

# Build models for each income group
fixed_model_low <- plm(gdp_growth ~ pop_growth, data = low_income_data, model = "within")
fixed_model_lower_middle <- plm(gdp_growth ~ pop_growth, data = lower_middle_income_data, model = "within")
fixed_model_upper_middle <- plm(gdp_growth ~ pop_growth, data = upper_middle_income_data, model = "within")
fixed_model_high <- plm(gdp_growth ~ pop_growth, data = high_income_data, model = "within")

# Combine results into a single table
results_table <- data.frame(
  Income_Level = c("Low income", "Lower middle income", "Upper middle income", "High income"),
  Coefficient = c(coef(fixed_model_low)["pop_growth"],
                  coef(fixed_model_lower_middle)["pop_growth"],
                  coef(fixed_model_upper_middle)["pop_growth"],
                  coef(fixed_model_high)["pop_growth"]),
  Std_Error = c(summary(fixed_model_low)$coefficients["pop_growth", "Std. Error"],
                summary(fixed_model_lower_middle)$coefficients["pop_growth", "Std. Error"],
                summary(fixed_model_upper_middle)$coefficients["pop_growth", "Std. Error"],
                summary(fixed_model_high)$coefficients["pop_growth", "Std. Error"]),
  p_value = c(summary(fixed_model_low)$coefficients["pop_growth", "Pr(>|t|)"],
              summary(fixed_model_lower_middle)$coefficients["pop_growth", "Pr(>|t|)"],
              summary(fixed_model_upper_middle)$coefficients["pop_growth", "Pr(>|t|)"],
              summary(fixed_model_high)$coefficients["pop_growth", "Pr(>|t|)"]),
  R_Squared = c(summary(fixed_model_low)$r.squared,
                summary(fixed_model_lower_middle)$r.squared,
                summary(fixed_model_upper_middle)$r.squared,
                summary(fixed_model_high)$r.squared)
)

# Display results table
kable(results_table, format = "html", table.attr = "style='width:100%; margin:auto;'") %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Create a visualization with updated settings
ggplot(results_table, aes(x = Income_Level, y = Coefficient, fill = Income_Level)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = Coefficient - 1.96 * Std_Error, ymax = Coefficient + 1.96 * Std_Error), width = 0.2) +
  scale_fill_manual(values = c("#dce3f0", "#a5b8d0", "#6f8fb0", "#375d8a")) + # Shades of blue-gray
  labs(
    title = "Impact of Population Growth on GDP Growth by Income Level",
    y = "Coefficient (Effect Size)" # Removed x-axis label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = rel(0.85), hjust = 0.5, vjust = 0.5), # Reduce x-axis text size to 75%
    axis.title.x = element_blank(), # Remove x-axis label
    plot.title = element_text(hjust = 0.5), # Centered title
    panel.grid.major.x = element_blank() # Optional: Clean grid
  )
```
The table and graph show that population growth has a significant impact on economic growth only in low-income countries, where the coefficient is 4.51 with a p-value of 0.004, confirming its statistical significance. In middle and high income countries, the impact of population growth is insignificant with coefficients ranging from 0.16 to 0.70 and p-values above 0.26, making it statistically insignificant. This indicates that in high-income countries, economic growth is driven by factors other than demographics. The low R² values (-0.25 to 0.10) demonstrate that the models explain only a small fraction of GDP changes, which limits their predictive power. These findings emphasize the importance of population growth in low-income countries and point to the need to find other key factors for advanced economies.

# CONCLUSIONS AND RECOMMENDATIONS
The study of the impact of demographic factors on economic growth revealed that the role of these variables varies significantly depending on the income level of countries. In low-income countries, population growth showed a statistically significant impact on GDP growth (coefficient 4.51, p = 0.004), which confirms the hypothesis about the importance of demography for economic development in the initial stages. On the contrary, in middle- and high-income countries, the coefficients of the impact of population growth range from 0.16 to 0.70 with p-values above 0.26, making them statistically insignificant. This indicates that in these countries, GDP growth is driven by other factors such as technological development, investment and institutional quality.

One of the key problems with the study was the low R² values, ranging from -0.25 to 0.10. These values indicate that the included models explain only a small proportion of GDP changes. This result emphasizes the limitations of demographic variables as an explanatory factor for economic growth, which in turn requires the inclusion of additional variables such as infrastructure investment, level of innovation, and political stability. The exclusion of outliers (e.g. Qatar and Ireland data) significantly improved the consistency of the results, which confirms the need for careful handling of the data at the data preparation stage.

The analysis showed that such factors as the share of the elderly population and the level of education have no significant impact on economic growth in the short run. The coefficients for these variables remain low and statistically insignificant in all groups of countries, which confirms the hypothesis of their limited influence. At the same time, demographic factors have a more pronounced impact in low-income countries, suggesting the need for targeted policies to stimulate growth through investment in human capital and employment programs.

This student paper emphasized basic methods of data analysis and interpretation. We do not claim to be an absolute academic and exhaustive study of all factors. However, the correlations identified suggest that population growth is key to economic development in low-income countries, while other factors such as innovation and investment are important for high-income countries. This study forms the basis for further work aimed at a comprehensive analysis of economic growth.
