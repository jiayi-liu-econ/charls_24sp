library(haven)
library(data.table)
library(tidyverse)
library(grDevices)

demo20 <- read_dta("./data/raw/Demographic_Background.dta")

demo20 <- as_tibble(demo20)

demo20 <- demo20 %>% select(ID, householdID, communityID, xrage, xrgender, ba008, ba009, ba011, ba015, ba016, ba016_1, ba017, zredu) %>% 
        setNames(c("ID", "householdID", "communityID", "age", "gender", "resid_loc", "hukou", "marital", "enodw_insurance", "pay_mi", "expense_mi", "med_insurance", "edu"))

demo20 <- demo20 |> mutate(gender = ifelse(gender == 2, 0, gender)) |>
    rename(male = gender)

# drop na
demo20 <- demo20 |>
  drop_na(ID, householdID, communityID, age, male, edu, marital, resid_loc, hukou)

demo20 <- demo20 |> mutate(med_insurance = ifelse(pay_mi == 3, 0, med_insurance))

demo20 <- demo20 |>
  drop_na(med_insurance)

# drop hukou==4
demo20 <- demo20 |> filter(hukou != 4)

# process marital and hukou
demo20 <- demo20 |> 
    mutate(rural_hk = ifelse(hukou == 1, 1, 0))

demo20 <- demo20 |> 
    mutate(married = ifelse(marital == 1 | marital == 2 | marital == 3, 1, 0))

# create eduyr = years of schooling

demo20 <- demo20 |>
    mutate(eduyr = case_when(edu == 1 ~ 0,
                        edu == 2 ~ 2.5,
                        edu == 3 ~ 2.5,
                        edu == 4 ~ 5,
                        edu == 5 ~ 9,
                        edu == 6 ~ 12,
                        edu == 7 ~ 10.5,
                        edu == 8 ~ 13.5,
                        edu == 9 ~ 16,
                        edu == 10 ~ 18,
                        edu == 11 ~ 23))


# create dummies for medical insurance types
# base group is no medical insurance
demo20 <- demo20 |> 
    mutate(UEMI = ifelse(med_insurance == 1, 1, 0),
    URRMI = ifelse(med_insurance == 2, 1, 0),
    URMI = ifelse(med_insurance == 3, 1, 0),
    NCMI = ifelse(med_insurance == 4, 1, 0),
    PMI = ifelse(med_insurance == 5, 1, 0),
    OtherMI = ifelse(med_insurance == 6, 1, 0))


demo20 |> nrow()
demo20 |> drop_na() |> nrow()
# 6000+ NA caused by "enodw_insurance", "pay_mi", "expense_mi" 
# demo20 <- demo20 |> select(ID, householdID, communityID, age, male, edu, marital, resid_loc, hukou, rural_hk, married, med_insurance, UEMI, URMI, URRMI, OtherMI, PMI, NCMI)
fwrite(demo20, "./data/clean/demo20.csv")
# obs count 17670 for no NA data excluding "enodw_insurance", "pay_mi", "expense_mi"
# obs count 11923 for no NA data includling "enodw_insurance", "pay_mi", "expense_mi"

# merge province and city
prov <- read_dta("./data/raw/charls2013stata_psu_psu编码.dta")
prov <- as_tibble(prov)
prov <- prov %>% select(communityID, province, city, urban_nbs)
fwrite(prov, "./data/clean/prov.csv")
demo20 <- demo20 |> left_join(prov, by = "communityID")
fwrite(demo20, "./data/clean/demo20.csv")

# merge household information (expenditure)
house <- read_dta("./data/raw/Household_Income.dta")
house <- as_tibble(house)
house <- house %>% select(householdID, gf001, gf006, gf013_6, gf013_16, gf014, gc001, gc007, gc007_1, gc007_2, gd001, gd005, gd006, ge008, ge008_1) %>%
    setNames(c("householdID", "hhexp", "hhexp_food", "hhexp_med", "hhexp_covidprev", "ends_meet", "gc001", "gc007", "gc007_1", "gc007_2", "gd001", "gd005", "gd006", "ge008", "ge008_1"))
# Note: the unnamed variables (info about how COVID impact wages) are not gonna used this time.

house <- house %>% select(householdID, hhexp, hhexp_food, hhexp_med, hhexp_covidprev, ends_meet)

# standardize 先不要吧...先summary以后再做这个...
# house <- house %>% mutate(hhexp = log(hhexp+1), hhexp_food = log(hhexp_food+1), hhexp_med = log(hhexp_med+1), hhexp_covidprev = log(hhexp_covidprev+1))

house |> nrow()
house <- house |>
  drop_na()
fwrite(house, "./data/clean/house20.csv")

data20 <- demo20 |> left_join(house, by = "householdID")
fwrite(data20, "./data/clean/data20_prep.csv")

# merge covid information
covid <- as_tibble(read_dta("./data/raw/COVID_Module.dta"))
covid <- covid %>% select(ID, va007, va003, va004, va006_s1, va006_s2, vd001_s1, vd001_s2, vd001_s3, vd001_s4, vd001_s5, vd001_s997, vd001_s999, vd001_1, vd001_2, vd001_3, vd001_4, va002_s1, va002_s2, va002_s3, va002_s4, va002_s5, va002_s6, va002_s7, va002_s8, va002_s9, va002_s10, va001_s1, va001_s2, va001_s3, va001_s4, va001_s5, va001_s6, va001_s7, va001_s8, va001_s9) %>%
    setNames(c("ID","attitude_govcovid", "mask_duringcovid", "mask_rn", "va006_s1", "va006_s2", "vd001_s1", "vd001_s2", "vd001_s3", "vd001_s4", "vd001_s5", "vd001_s997", "vd001_s999", "vd001_1", "vd001_2", "vd001_3", "vd001_4", "va002_s1", "va002_s2", "va002_s3", "va002_s4", "va002_s5", "va002_s6", "va002_s7", "va002_s8", "va002_s9", "va002_s10", "va001_s1", "va001_s2", "va001_s3", "va001_s4", "va001_s5", "va001_s6", "va001_s7", "va001_s8", "va001_s9"))
# Note: the unnamed variables (info about stockpile food/med, COVID knowledge, and media via which to get knowledge) are not gonna used this time.
# fwrite(covid, "./data/clean/covid20.csv")

covid |> nrow()
covid <- covid %>% select(ID, attitude_govcovid, mask_duringcovid) %>%
    drop_na()

data20 <- data20 |> left_join(covid, by = "ID")
fwrite(data20, "./data/clean/data20_prep.csv")

# merge health information
health <- as_tibble(read_dta("./data/raw/Health_Status_and_Functioning.dta"))
health <- health %>% select(ID, da001, dc026, da010_s1, da010_s2, da010_s3, da010_s4, da010_s5, da010_s6, da011_s1, da011_s2, da011_s3, da012_s1, da012_s2, da012_s3, da012_s4, da012_s5) %>% 
    setNames(c("ID", "selfrepo_health", "life_satisfy", "da010_s1", "da010_s2", "da010_s3", "da010_s4", "da010_s5", "da010_s6", "da011_s1", "da011_s2", "da011_s3", "da012_s1", "da012_s2", "da012_s3", "da012_s4", "da012_s5"))
# Note: the unnamed variables (info about how COVID delayed medical services) are not gonna used this time.

health <- health |> select(ID, selfrepo_health, life_satisfy)
health <- health |> filter(selfrepo_health != 997)
health |> nrow()
health <- health |> drop_na()

data20 <- data20 |> left_join(health, by = "ID")
fwrite(data20, "./data/clean/data20_prep.csv")

# merge work_retirement
work <- as_tibble(read_dta("./data/raw/Work_Retirement.dta"))
work <- work %>% select(ID, xworking) %>% setNames(c("ID", "working"))

work |> nrow()
work <- work |> drop_na()

data20 <- data20 |> left_join(work, by = "ID")
fwrite(data20, "./data/clean/data20_prep.csv")

data20 |> nrow()
data20 |> drop_na() |> nrow()
# obs count 17670
# obs count 11079 for no NA data

data20 <- data20 |> relocate(ID,householdID,communityID,age,male,working,resid_loc,province,city,urban_nbs,hukou,rural_hk,marital,married,edu,eduyr,enodw_insurance,pay_mi,expense_mi,med_insurance,UEMI,URRMI,URMI,NCMI,PMI,OtherMI,attitude_govcovid,mask_duringcovid,selfrepo_health,life_satisfy,hhexp,hhexp_food,hhexp_med,hhexp_covidprev,ends_meet)
fwrite(data20, "./data/clean/data20_prep.csv")