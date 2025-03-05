# AHIES_EDA.R

# ------------------------------------------------------------------------------
# 1. Setup and Libraries
# ------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(reshape2)
source("./theme.R")

# ------------------------------------------------------------------------------
# 2. Load Data
# ------------------------------------------------------------------------------

# Load dataset
ahies <- read.csv('./data/10%AHIES.csv')

# Data exploration
dim(ahies)
head(ahies)
summary(ahies)
colnames(ahies)

# Select relevant columns
df <- ahies %>%
  select(region, s1aq1, s1aq4y, urbrur, s1aq5, s2aq3, s2aq11a29, s4aq2,
         s4aq40a1, s4aq43m, s4aq49, s4aq55a, s4gq13c, hhid, s3aq23hhid)

# ------------------------------------------------------------------------------
# 3. Data Cleaning
# ------------------------------------------------------------------------------

clean_data <- function(df) {
  df <- df %>%
    mutate(
      urbrur = str_replace(urbrur, "Urbanb", "Urban"),
      s2aq3 = case_when(
        s2aq3 == '0' ~ 'No Formal Education',
        TRUE ~ s2aq3
      )
    )
  
  # Fill missing wages by occupation type
  df <- df %>%
    group_by(s4aq40a1) %>%
    mutate(
      s4aq55a = ifelse(
        is.na(s4aq55a) & !is.na(s4aq40a1),
        mean(s4aq55a, na.rm = TRUE),
        s4aq55a
      )
    ) %>%
    ungroup()
  
  # Fill missing wages by education level
  df <- df %>%
    group_by(s2aq3) %>%
    mutate(
      s4aq55a = ifelse(
        is.na(s4aq55a),
        mean(s4aq55a, na.rm = TRUE),
        s4aq55a
      )
    ) %>%
    ungroup()
  
  # Encode categorical variables
  df <- df %>%
    mutate(
      education_rank = case_when(
        s2aq3 == "No Formal Education" ~ 0,
        s2aq3 == "Kindergarten" ~ 2,
        s2aq3 == "Nursery" ~ 1,
        s2aq3 == "Primary" ~ 3,
        s2aq3 == "Middle" ~ 5,
        s2aq3 == "JSS/JHS" ~ 4,
        s2aq3 == "SSS/SHS" ~ 6,
        s2aq3 == "Secondary" ~ 7,
        s2aq3 == "Voc/Tech/Commercial" ~ 8,
        s2aq3 == "Postmiddle/secondary certificate" ~ 9,
        s2aq3 == "Postmiddle/secondary diploma" ~ 10,
        s2aq3 == "Tertiary - HND" ~ 11,
        s2aq3 == "Tertiary - Bachelor's Degree" ~ 12,
        s2aq3 == "Tertiary - Post graduate certificate / diploma" ~ 13,
        s2aq3 == "Tertiary - Master's Degree" ~ 14,
        s2aq3 == "Tertiary - PhD" ~ 15,
        s2aq3 == "Others specify" ~ 16,
        TRUE ~ NA_real_
      ),
      urban_binary = if_else(urbrur == 'Urban', 1, 0),
      abv_avg_wage = if_else(s4aq55a > median(s4aq55a, na.rm = TRUE), 'High', 'Low')
    )
  
  return(df)
}

# Clean data
df <- clean_data(df)

# Save cleaned data
write.csv(df, './outputs/results/cleaned_data.csv', row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. Data Analysis
# ------------------------------------------------------------------------------

calculate_mode <- function(x) {
  ux <- unique(na.omit(x))
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

aggregate_data <- function(df) {
  aggregated_df <- df %>%
    group_by(region) %>%
    summarise(
      total_respondents = n(),
      total_households = n_distinct(hhid),
      avg_household_size = n() / n_distinct(hhid),
      mean_age = mean(s1aq4y, na.rm = TRUE),
      total_males = sum(s1aq1 == "Male", na.rm = TRUE),
      total_females = sum(s1aq1 == "Female", na.rm = TRUE),
      total_rural = sum(urbrur == 'Rural', na.rm = TRUE),
      total_urban = sum(urbrur == 'Urban', na.rm = TRUE),
      prop_urban = mean(urban_binary),
      most_relationship_type = calculate_mode(s1aq5),
      modal_education = calculate_mode(s2aq3),
      avg_edu = mean(education_rank, na.rm = TRUE),
      total_edu_spending = sum(s2aq11a29, na.rm = TRUE),
      avg_weekly_hours = mean(s4aq2, na.rm = TRUE),
      modal_occupation = calculate_mode(s4aq40a1),
      min_wage = min(s4aq55a, na.rm = TRUE),
      average_wage = mean(s4aq55a, na.rm = TRUE),
      max_wage = max(s4aq55a, na.rm = TRUE),
      prop_min_wage = mean(s4gq13c == "Yes", na.rm = TRUE)
    )
  
  return(aggregated_df)
}

# Aggregate data
aggregated_df <- aggregate_data(df)

# Save aggregated data
write.csv(aggregated_df, './outputs/results/aggregated_data.csv', row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Top Metrics Function
# ------------------------------------------------------------------------------

top_metrics <- function(df, metric, n = 5) {
  df %>%
    select(region, {{metric}}) %>%
    arrange(desc({{metric}})) %>%
    head(n)
}

# Get top metrics
top_hse_size <- top_metrics(aggregated_df, total_households)
top_wage <- top_metrics(aggregated_df, average_wage)
edu_spending <- top_metrics(aggregated_df, total_edu_spending)
urb_pro <- top_metrics(aggregated_df, prop_urban)
min_wage_prop <- top_metrics(aggregated_df, prop_min_wage)

# Creating Vectors and Matrices
avg_wage_v <- aggregated_df$average_wage
names(avg_wage_v) <- aggregated_df$region

hse_size_v <- aggregated_df$avg_household_size
names(hse_size_v) <- aggregated_df$region

# A matrix of educ. vs wage
edu_wage_mat <- matrix(
  c(aggregated_df$modal_education, aggregated_df$average_wage),
  nrow = nrow(aggregated_df),
  dimnames = list(aggregated_df$region, c('Education Mode', 'Avg Wage'))
)  


# Putting everything into a list
results_list <- list(
  top_households=top_households,
  top_wage=top_wage,
  urban_rural_prop=urb_rur_pro,
  min_wage_prop=min_wage_prop,
  modal_marital_status=aggregated_df$most_relationship_type,
  vectors = list(
    avg_wage = avg_wage_v,
    household_size = hse_size_v),
  matrices = list(
    education_wage=edu_wage_mat
  )
)

# ------------------------------------------------------------------------------
# 6. Key Comparisons
# ------------------------------------------------------------------------------

# Urban vs Rural Wages
urb_rur_wage <- df %>%
  group_by(urbrur) %>%
  summarise(
    avg_wage = mean(s4aq55a, na.rm = TRUE),
    avg_edu_rank = mean(education_rank, na.rm = TRUE)
  )

# Wages by Educational Level
edu_mets <- df %>%
  group_by(s2aq3) %>%
  summarise(
    avg_wage = mean(s4aq55a, na.rm = TRUE),
    prop_min_wage = mean(s4gq13c == 'Yes', na.rm = TRUE),
    respondents = n()
  ) %>%
  arrange(desc(avg_wage))

# ------------------------------------------------------------------------------
# 7. Visualizations
# ------------------------------------------------------------------------------

# Average Wage by Region
avg_wage_chart <- aggregated_df %>%
  ggplot(aes(x = region, y = average_wage)) +
  geom_col(width = 0.8) +
  gssthemes() +
  scale_y_continuous(
    expand = c(0, 1),
    limits = nicelimits,
    labels = scales::comma,
    breaks = scales::extended_breaks(only.loose = TRUE)
  ) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(
    x = 'Region',
    y = "Average Wage (GHS)",
    title = "Average Wage by Region"
  ) +
  coord_cartesian(clip = "off")
ggsave('./outputs/plots/avg_wage_chart.png', avg_wage_chart, width = 8, height = 6)

# Bar chart for urban proportion
urb_rur_chart <- df %>%
  group_by(region, urbrur) %>%
  summarise(count = n()) %>%  
  ungroup() %>%
  group_by(region) %>%
  mutate(percentage = count / sum(count)) %>% 
  ungroup() %>%
  ggplot(aes(x = region, y = percentage, fill = urbrur)) +
  geom_col() +
  coord_flip(expand = FALSE, clip = "off") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(
    aes(
      label = ifelse(percentage < 0.12, "", 
                     paste0(format(round(percentage * 100, 1), nsmall = 1), "%")),
      y = ifelse(urbrur == "Urban", 0.04, 0.91) 
    ),
    size = 3.5,
    position = "stack"
  ) +
  gssthemes() +
  labs(
    x = NULL,
    y = "Share of Respondents",
    title = "Share of Respondents by Region and Locality"
  ) +
  scale_fill_manual(
    values = c(Urban = urban_color, Rural = rural_color),
    labels = c("Rural", "Urban")
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    panel.grid.major.x = element_line(color = "gray", size = 0.25),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(vjust = 0.5)
  )
ggsave('./outputs/plots/locality_pro_chart.png', urb_rur_chart, width = 8, height = 6)

# Education Rank by Region
avg_edu_chart <- df %>% mutate(education_rank=as.numeric(education_rank)) %>% 
  group_by(region) %>%
  summarise(avg_education_rank = mean(education_rank, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = reorder(region, avg_education_rank), y = avg_education_rank)) +
  geom_col(width = 0.8) +
  geom_text(mapping = aes(label = scales::comma(avg_education_rank, accuracy = 0.1)), hjust = -0.2) +
  gssthemes() +
  scale_y_continuous(
    expand = c(0, 1),
    limits = nicelimits(df$education_rank),
    labels = scales::comma,
    breaks = scales::extended_breaks(only.loose = TRUE)
  ) +
  labs(
    x = NULL,
    y = "Average Education Rank"
  ) +
  coord_flip(clip = "off") +
  theme(
    panel.grid.major.x = element_line(color = "gray", size = 0.25),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(vjust = 0.5)
  )
ggsave('./outputs/plots/edu_rank.png', avg_edu_chart, width = 8, height = 6)

# Box plot Wage Distribution by Education Level
wage_dist_by_edu_level <- df %>% mutate(education_rank = factor(education_rank)) %>%  
  ggplot(aes(x = education_rank, y = s4aq55a, fill = s2aq3)) +
  geom_boxplot() +
  labs(title = "Wage Distribution by Education Level", 
       x = "Education Level",
       y = "Wage (GHS)")+
  gssthemes()
ggsave('./outputs/plots/wage_edu_box_plot.png', wage_dist_by_edu_level, width = 8, height = 6)

# Wage Distribution by Urban/Rural
wage_dist_by_urbru <- df %>% mutate(education_rank = factor(education_rank)) %>%  
  ggplot(aes(x = urbrur, y = s4aq55a, fill = urbrur)) +
  geom_boxplot() +
  labs(title = "Wage Distribution by Locality Type", 
       x = "Locality",
       y = "Wage (GHS)")+
  gssthemes()
ggsave('./outputs/plots/wage_locality_box_plot.png', wage_dist_by_urbru, width = 8, height = 6)

# Scatter plot for Wage vs Age
wage_age_chart <- df %>% 
  ggplot(aes(x=s1aq4y, y=s4aq55a)) +
  geom_point(alpha = 0.5, color = '#206095') +
  labs(
    title = "Wage vs. Age",
    x = "Age",
    y = "Wage (GHS)"
  ) +
  gssthemes()
ggsave('./outputs/plots/wage_vs_age_scatter.png', wage_age_chart, width = 8, height = 6)

# Scatter plot for Wage vs Education Rank
wage_education_chart <- df %>% 
  ggplot(aes(x=education_rank, y=s4aq55a)) +
  geom_point(alpha = 0.5, color = '#206095') +
  labs(
    title = "Wage vs. Education Rank",
    x = "Education Rank",
    y = "Wage (GHS)"
  ) +
  gssthemes()
ggsave('./outputs/plots/wage_vs_edu_scatter.png', wage_education_chart, width = 8, height = 6)

# Wage vs Hours worked
wage_hrs_chart <- df %>% 
  ggplot(aes(x=s4aq2, y=s4aq55a)) +
  geom_point(alpha = 0.5, color = '#206095') +
  labs(
    title = "Wage vs. Hours Worked",
    x = "Hours Worked per Week",
    y = "Wage (GHS)"
  ) +
  gssthemes()
ggsave('./outputs/plots/wage_vs_hrs_scatter.png', wage_age_chart, width = 8, height = 6)

# Correlation Heatmap
num_vars <- df %>%
  select(s4aq55a, education_rank, s4aq2, s1aq4y)

corr_matrix <- cor(num_vars, use = 'complete.obs')
corr_long <- melt(corr_matrix)
corr_long <- corr_long %>%
  mutate(
    Var1 = case_when(
      Var1 == "s4aq55a" ~ "Wage",
      Var1 == "education_rank" ~ "Education Rank",
      Var1 == "s4aq2" ~ "Hours Worked",
      Var1 == "s1aq4y" ~ "Age"
    ),
    Var2 = case_when(
      Var2 == "s4aq55a" ~ "Wage",
      Var2 == "education_rank" ~ "Education Rank",
      Var2 == "s4aq2" ~ "Hours Worked",
      Var2 == "s1aq4y" ~ "Age"
    )
  )

corr_heatmap <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = 'black', size = 2) +
  scale_fill_gradientn(colours = incidence_color_scheme) +
  theme_minimal() +
  labs(
    title = 'Correlation Heatmap',
    x = NULL,
    y = NULL,
    fill = 'Correlation'
  ) +
  gssthemes() +
  theme(legend.position = 'bottom')
ggsave('./outputs/plots/corr_heatmap.png', corr_heatmap, width = 8, height = 6)

# Density plots
dense_plot <- ggplot(df, aes(x = s4aq55a, fill = urbrur)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(Urban = urban_color, Rural = rural_color)) +
  labs(
    title = "Wage Distribution by Locality",
    x = 'Wage (GHS)',
    y = 'Density',
    fill = 'Locality'
  ) + 
  gssthemes()
ggsave('./outputs/plots/wage_locality_density.png', dense_plot, width = 8, height = 6)

# ------------------------------------------------------------------------------
# 8. Statistical Analysis
# ------------------------------------------------------------------------------

# Chi-square test for wage and locality type
cont_table1 <- table(df$urbrur, df$abv_avg_wage)
chi_square_test <- chisq.test(cont_table)

# Chi-square test for wage and gender
cont_table2 <- table(df$s1aq1, df$abv_avg_wage)
chi_square_test_2 <- chisq.test(con_table)

# T-tests
t_test1 <- t.test(education_rank ~ abv_avg_wage, data = df)
t_test2 <- t.test(s4aq2 ~ abv_avg_wage, data = df)

# Linear Model to predict wage
l_model <- lm(s4aq55a ~ education_rank + s4aq2 + urbrur + s1aq1, data = df)
summary(l_model)