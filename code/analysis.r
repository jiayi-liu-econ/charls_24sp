library(fixest)
library(Hmisc)
library(modelsummary)
library(rmarkdown)
library(tidyverse)

data20 <- read.csv("./data/clean/data20_desc.csv")

data20 <- data20 %>% filter(selfrepo_health != 997)

# describe check
# d <- data20$selfrepo_health %>% describe()
# html(d, size = 80, scroll = TRUE)
# str(data20$selfrepo_health) # integer type
# str(data20$life_satisfy) # integer type

# regression with pronvince FE
# age,male,working,urban_nbs,rural_hk,married,eduyr,UEMI,URRMI,URMI,NCMI,PMI,OtherMI,attitude_govcovid,selfrepo_health,life_satisfy,lexpense_mi,lhhexp,lhhexp_med
m1 <- feols(selfrepo_health ~ UEMI + URRMI + URMI + NCMI + PMI + OtherMI + age + male + working + urban_nbs + rural_hk + married + eduyr + lhhexp| province,
  cluster = ~ province, data = data20)
summary(m1)
saveRDS(m1, "./output/models/feols_selfrepo_health.rds")

m2 <- feols(life_satisfy ~ UEMI + URRMI + URMI + NCMI + PMI + OtherMI + age + male + working + urban_nbs + rural_hk + married + eduyr + lhhexp| province,
  cluster = ~ province, data = data20)
summary(m2)
saveRDS(m2, "./output/models/feols_life_satisfy.rds")

# table output (same table, just two different output types)
modelsummary(list(`Self-reported Health Status` = m1, `Life Satisfaction` = m2),
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    # notes = c("*** p<0.01, ** p<0.05, * p<0.1"),
    output = "./output/tables/feols.md", # markdown output
    gof_map = c("nobs", "r.squared")
    )

modelsummary(list(`Self-reported Health Status` = m1, `Life Satisfaction` = m2),
    stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
    # notes = c("*** p<0.01, ** p<0.05, * p<0.1"),
    output = "./output/tables/feols.tex", # latex output
    gof_map = c("nobs", "r.squared")
    )

# render("./output/tables/feols.md", output_file = "./output/tables/feols.pdf") # strangely failed again and again...
