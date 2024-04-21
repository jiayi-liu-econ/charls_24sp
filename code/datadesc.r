library(gtsummary)
library(grDevices)
library(ggplot2)
library(data.table)
library(tidyverse)
library(grDevices)
library(Hmisc) # can be used to describe data as "codebook" in stata # should be used in dataprep
library(expss) # to label values of variables
library(reshape2)
library(dplyr)
library(stringr)
library(ggthemes)
library(extrafont)
library(webshot2)

data20 <- fread("./data/clean/data20_prep.csv")

# label for ends_meet, life_satisfy, selfrepo_health, attitude_govcovid
data20 <- apply_labels(data20,
    ends_meet = c("Very Difficult" = 1,
    "Difficult" = 2,
    "Easy" = 3,
    "Very Easy" = 4),
    life_satisfy = c("Completely Satisfied" = 1,
    "Very Satisfied" = 2,
    "Somewhat Satisfied" = 3,
    "Not Very Satisfied" = 4,
    "Not at All Satisfied" = 5),
    selfrepo_health = c("Very Good" = 1,
    "Good" = 2,
    "Fair" = 3,
    "Poor" = 4,
    "Very Poor" = 5,
    "Don't Know" = 997),
    attitude_govcovid = c("Stricter Than Necessary" = 1,
    "Satisfied" = 2,
    "Less Strict Than Necessary" = 3,
    "Don't Know" = 997,
    "Refuse to Answer" = 999))

fwrite(data20, "./data/clean/data20_desc.csv")

# process hhexp* and expense_mi
# describe
d <- data20 %>% select(hhexp, hhexp_food,hhexp_med,hhexp_covidprev, expense_mi) %>% describe()
html(d, size = 80, scroll = TRUE)

# "-1" for hhexp_med means "don't know" -> NA ######***** "." is not NA in R *****######
data20 <- data20 %>% mutate(hhexp_med = ifelse(hhexp_med == "-1", NA, hhexp_med),
    hhexp_covidprev = ifelse(hhexp_covidprev == "-1", NA, hhexp_covidprev),
    expense_mi = ifelse(expense_mi == "-1", NA, expense_mi))

# Summarize and draw boxplots first, then drop outliers and do analysis, or just take log and do analysis.

# # draw boxplots
# data_long <- reshape2::melt(data20, measure.vars = c("hhexp", "hhexp_food", "hhexp_med", "hhexp_covidprev", "expense_mi"))

# # Plotting the boxplots
# ggplot(data_long, aes(x = variable, y = value)) +
#   geom_boxplot() +
#   labs(title = "Boxplots of Household Expenses",
#        x = "Variable",
#        y = "Value") +
#   theme_minimal()

# The outliers are so extreme that compress the boxplots. Let's drop outliers outside of (1%,99%).

# drop outliers
# data20 %>% drop_na(expense_mi) %>% 
#     mutate(expense_mi_d = expense_mi[!expense_mi %in% boxplot.stats(expense_mi)$out]) # failed code

# Define a function to remove outliers based on the 1% and 99% percentiles
remove_outliers <- function(x) {
  lower_bound <- quantile(x, 0.01, na.rm = TRUE)
  upper_bound <- quantile(x, 0.99, na.rm = TRUE)
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

# Apply the function to the specific columns in the dataframe
data20_clean <- data20 %>%
  mutate(
    hhexp = remove_outliers(hhexp),
    hhexp_food = remove_outliers(hhexp_food),
    hhexp_med = remove_outliers(hhexp_med),
    hhexp_covidprev = remove_outliers(hhexp_covidprev),
    expense_mi = remove_outliers(expense_mi)
  )

# Describe again
# d_clean <- data20_clean %>% select(hhexp, hhexp_food,hhexp_med,hhexp_covidprev, expense_mi) %>% describe()
# html(d_clean, size = 80, scroll = TRUE)

# Note: hhexp_food is measured per week; hhexp is measured per month; other expenses are measured per year
# Transform hhexp to per year. (Transform hhexp as follows; For hhexp_food, leave it aside for summary this time.)
data20_clean <- data20_clean %>% mutate(hhexp = hhexp*12)
data20 <- data20 %>% mutate(hhexp = hhexp*12)

fwrite(data20, "./data/clean/data20_desc.csv")
fwrite(data20_clean, "./data/clean/data20_clean.csv")

# generate log(expenditure) variables in the original dataset
data20 <- data20 %>% mutate(lexpense_mi = log(expense_mi+1), lhhexp = log(hhexp+1), lhhexp_food = log(hhexp_food+1), lhhexp_med = log(hhexp_med+1), lhhexp_covidprev = log(hhexp_covidprev+1))
fwrite(data20, "./data/clean/data20_desc.csv")
data20_clean <- data20_clean %>% mutate(lexpense_mi = log(expense_mi+1), lhhexp = log(hhexp+1), lhhexp_food = log(hhexp_food+1), lhhexp_med = log(hhexp_med+1), lhhexp_covidprev = log(hhexp_covidprev+1))
fwrite(data20_clean, "./data/clean/data20_clean.csv")


#######################################################3
# Draw boxplots again
data_long <- reshape2::melt(data20_clean, measure.vars = c("hhexp","hhexp_covidprev","hhexp_med","expense_mi"))

# Plotting the boxplots
boxplot <- ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") + # Allows each subplot to have its own y-axis scale
  labs(title = "Boxplots of Household Expenses and Individual Expenses on Medical Insurance (Per Year)",
  x = " ",
  y = " ") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, face = "bold", lineheight = 1.2)) # Adjust size and lineheight

ggsave("./output/figures/boxplot_expenses.pdf", boxplot, width = 6, height = 6, units = "in")

# Better drawing histogram for expense_mi
hist <- ggplot(data20_clean, aes(x = expense_mi)) +
  geom_histogram(bins = 50, fill = "#ffc400", color = "black") +
  labs(title = "Histogram of Individual Expenses on Medical Insurance (CNY/Year)",
        x = " ",
        y = "Frequency (Bins = 50)") +
        theme_grey() +
        theme(plot.title = element_text(family = "Times", face = "bold", size = 13, lineheight = 1.2),
        text = element_text(family = "Times"))
#   annotate("text", x = Inf, y = Inf, label = "Bins = 50", hjust = 1.2, vjust = 1.2, size = 3.5, color = "black")  # Add annotation for bins 
ggsave("./output/figures/histogram_expensemi.pdf", hist, width = 6, height = 6, units = "in")

######################################################
# summary
# data20_clean <- fread("./data/clean/data20_clean.csv")
sum <- data20_clean %>% select(age,male,working,urban_nbs,rural_hk,married,eduyr,expense_mi,UEMI,URRMI,URMI,NCMI,PMI,OtherMI,attitude_govcovid,selfrepo_health,life_satisfy,hhexp,hhexp_med,hhexp_covidprev,ends_meet) %>% 
    tbl_summary(
        # type = list(c(attitude_govcovid) ~ "categorical",
        # c(male, working, urban_nbs, rural_hk, married, UEMI,URRMI,URMI,NCMI,PMI,OtherMI, selfrepo_health,life_satisfy, ends_meet) ~ "continuous"),
        statistic = list(all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{p}%"),
        # missing_text = "(Missing)"
        missing = "no",
        digits = everything() ~ 2) %>%
        modify_header(label ~ "**Variable**") %>%
        modify_footnote(all_stat_cols() ~ "Mean (SD) or Frequency (%)") %>%
        modify_caption("**Table 1. Summary Statistics**") %>%
        bold_labels()

sum %>%
  as_gt() %>%
  gt::gtsave("./output/tables/sum.pdf", expand = TRUE) # use extensions .png, .html, .docx, .rtf, .tex, .ltx
