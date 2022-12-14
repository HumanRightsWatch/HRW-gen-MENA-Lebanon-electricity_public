#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive - Human Rights Watch/HRW/HRW-gen-MENA-Lebabon-electricity/descriptives/src/lebanon_electricity_descriptives.r
library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, rstatix, #get summary table function#
       survey, srvyr
       )
options(scipen=999)

here <- here::here

source("../plot_themes.R")


########### input and output files ##############
# input files:
inputfiles <- list(
   df = "processing/output/df.rds"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   bills_per_ampereHR = "descriptives/output/bills_per_ampereHR.csv",
   income_hours_no_electricity = "descriptives/output/income_hours_no_electricity.csv",
   income_no_generator = "descriptives/output/income_no_generator.csv",
   electricity_shutoff = "descriptives/output/electricity_shutoff.csv",
   income_pay_essentials = "descriptives/output/income_pay_essentials.csv",
   total_burden = "descriptives/output/total_burden.csv",
   difficulty_paying = "descriptives/output/difficulty_paying.csv",
   income_proportion_households = "descriptives/output/income_proportion_households.csv",
   distribution_gov_assistance = "descriptives/output/distribution_gov_assistance.csv"
   
) %>% map(here)

#read in
df <- read_rds(inputfiles$df)

#set up survey design (equal weights)
df <- df %>%
   mutate(wt = 1)

svy <- as_survey_design(df, weights = wt)

#sources of income
t <- svy %>%
   group_by(income_source) %>% 
   summarise(avg_percentage = survey_prop( vartype = c( "ci"), proportion = T))

#median income
t <- svy %>%
   summarise(avg_current = round(survey_median(total_monthly_USD, vartype = c( "ci"),
                                             na.rm = T), 1))

#income range per decile
t <- df %>%
   filter(!is.na(total_monthly_USD)) %>% 
   group_by(income_decile) %>% 
   summarise(min_income = min(total_monthly_USD), 
             median_income = median(total_monthly_USD), 
             max_income = max(total_monthly_USD))
  
#proportion of population by income
t <- df %>% 
   filter(!is.na(total_monthly_USD)) %>% 
   group_by(total_monthly_USD) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          cumperc = cumsum(perc))
write_csv(t, outputfiles$income_proportion_households)

#household size
t <- df %>% 
   mutate(household_size = q52 + q54) %>% 
   summarise(min_household = min(household_size),
             mean_household = mean(household_size),
             max_household = max(household_size),
             min_adults = min(q52),
             mean_adults = mean(q52),
             max_adults = max(q52),
             min_kids = min(q54),
             mean_kids = mean(q54),
             max_kids = max(q54)
           )

#monthly median income by governorate
#median income
t <- svy %>%
   group_by(Governorate) %>% 
   summarise(avg_current = round(survey_median(total_monthly_USD, vartype = c( "ci"),
                                               na.rm = T), 1))

# percent with a meter
svy %>%
   group_by(q3) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#avg electricity by governorate
sum(is.na(df$q4))
t <- svy %>%
   group_by(Governorate) %>%
   summarise(mean_current = round(survey_mean(q4, vartype = c( "ci"),
                                              na.rm = T), 1),
             median_current = round(survey_median(q4, vartype = c( "ci"),
                                                  na.rm = T), 1),
             mean_pre_crisis = round(survey_mean(q5, vartype = c( "ci"),
                                                 na.rm = T), 1))

#nationwide
t1 <- svy %>%
   summarise(mean_current = round(survey_mean(q4, vartype = c( "ci"),
                                             na.rm = T), 1),
               median_current = round(survey_median(q4, vartype = c( "ci"),
                                                  na.rm = T), 1),
             mean_pre_crisis = round(survey_mean(q5, vartype = c( "ci"),
                                                na.rm = T), 1)) %>% 
   mutate(Governorate = "Total Lebanon")

t <- bind_rows(t, t1) 

t <- t %>% 
   mutate(`95% confidence interval` = paste("(", mean_current_low, " - ", mean_current_upp, ")")) %>% 
   select(Mohafaza = Governorate, `Average (mean) hours per day from EDL` = mean_current, `95% confidence interval`)

#power outages
svy %>%
   group_by(q23) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

t <- df %>%
   filter(q23 == 1 & q23_a != 0) %>%
   get_summary_stats(q23_a, type = "common")

t <- df %>%
   group_by(Governorate, q23) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count)) %>%
   filter(q23 == 1)

t <- svy %>%
   group_by(income_quintile, q23) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>%
   filter(q23 == 1)

t <- svy %>%
   group_by(no_generator, q23) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

t <- df %>%
   group_by(q23_a) %>%
   summarise(count = n()) %>% 
   arrange(desc(q23_a)) %>%
   mutate(perc = count/sum(count),
          cumperc = cumsum(perc))

#EDL bills
#% that paid bill pre-crisis
t <- svy %>%
   filter(q6 == 1 | q6 == 2) %>%
   group_by(q6) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#% that paid most recent edl bill

t <- svy %>%
   filter(!is.na(q10)) %>%
   group_by(q10) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#generators - total and by income group
t1 <- df %>%
   group_by(q11a_n) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          cat = "Total Lebanon",
          type = "Neighborhood generator") %>%
   filter(q11a_n == 1) 

ci <- svy %>%
   group_by(q11a_n) %>% 
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) 

t2 <- df %>%
   group_by(q11a_o) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          cat = "Total Lebanon",
          type = "Personal generator") %>%
   filter(q11a_o == 1)

t3 <- df %>%
   group_by(q11a_b) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          cat = "Total Lebanon",
          type = "Building generator") %>%
   filter(q11a_b == 1)

t4 <- df %>%
   group_by(income_quintile, q11a_n) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          type = "Neighborhood generator",
          cat = as.character(income_quintile)) %>%
   filter(q11a_n == 1) 

t5 <- df %>%
   group_by(income_quintile, q11a_o) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          type = "Personal generator",
          cat = as.character(income_quintile)) %>%
   filter(q11a_o == 1) 

t6 <- df %>%
   group_by(income_quintile, q11a_b) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          type = "Building generator",
          cat = as.character(income_quintile)) %>%
   filter(q11a_b == 1)

table <- bind_rows(t1, t2, t3, t4, t5, t6) %>%
   mutate(`Percentage of Households` = round(perc * 100, 3)) %>%
   select(cat, type,
          `Percentage of Households`) %>% 
   filter(!is.na(cat))

#no generator total
t <- svy %>% 
   group_by(no_generator) %>% 
   summarize(perc = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Total Lebanon")

#no generator per income group
t1 <- svy %>% 
   filter(!is.na(total_monthly_USD)) %>% 
   group_by(income_quintile, no_generator) %>% 
   summarize(perc = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = as.character(income_quintile))

t <- bind_rows(t, t1) %>% 
   mutate(`Percentage of Households` = round(perc * 100, 3),
          type = "No generator") %>% 
   filter(no_generator == "X")  %>% 
   select(cat, type,
          `Percentage of Households`)

table <- bind_rows(table, t) 
rm(t1, t2, t3, t4, t5, t6)
table <- table %>% 
   pivot_wider(values_from = `Percentage of Households`, names_from = type)

#personal and building generators by quartile
t <- df %>%
   group_by(income_quartile, q11a_o) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          type = "Personal generator") %>%
   filter(q11a_o == 1) 

t1 <- df %>%
   group_by(income_quartile, q11a_b) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count),
          type = "Building generator") %>%
   filter(q11a_b == 1)

#building generators by governorate
t <- df %>% 
   filter(q11a_b == 1) %>% 
   group_by(Governorate) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))

ci <-  svy %>%
   group_by(Governorate, q11a_b) %>% 
   summarize(perc = survey_prop( vartype = c( "ci")))

#hours of electricity from neighborhood gen.
ci <-  svy %>%
   filter(q11a_n == 1) %>% 
   summarize(avg = survey_mean(q11b_n, vartype = c( "ci")))

#hours of electricity from building gen.
ci <-  svy %>%
   filter(q11a_b == 1) %>% 
   summarize(avg = survey_mean(q11b_b, vartype = c( "ci")))

#hours of electricity from personal gen.
ci <-  svy %>%
   filter(q11a_o == 1) %>% 
   summarize(avg = survey_mean(q11b_o, vartype = c( "ci")))

#other source
#general alternative source
ci <- svy %>%
   group_by( q14) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")))

#UPS/APS
ci <- svy %>%
   group_by(q14_a1) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")))

#solar
ci <- svy %>%
   group_by(q14_a2) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")))

#people with no generator use alternative source?
t <- df %>%
   group_by(no_generator, q14) %>%
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))

#solar
ci <- svy %>%
   group_by(q14_a2) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci")))
ci <- svy %>%
   filter(!is.na(income_quintile) & q14_a2 ==1) %>% 
   group_by(income_quintile) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci")))

#hours per day with no electricity
ci <-  svy %>%
   summarize(hrs_pr_day = survey_mean(q11j, vartype = c( "ci")))

#hours no electricity pre-crisis
ci <-  svy %>%
   summarize(hrs_pr_day = survey_mean(q12f, vartype = c( "ci")))

#current hrs no electricity by governorate
t <- df %>%
group_by(Governorate) %>%
                summarize(min = min(q11j),
                        median = median(q11j),
                          mean = mean(q11j),
                          max = max(q11j))

t <- df %>%
   group_by(Governorate, no_generator) %>%
   summarize(
             mean = mean(q11j)) %>%
   mutate(no_generator = ifelse(is.na(no_generator), "Has generator",
                                "No generator")) %>%
   pivot_wider(values_from = mean, names_from = no_generator) %>%
   mutate(across(where(is.numeric), round, digits = 0))

ci <-  svy %>%
   group_by(no_generator) %>%
   summarize(hrs_pr_day = survey_mean(q11j, vartype = c( "ci")))

#look at hours without electricity by income level
t <- df %>%
   filter(!is.na(total_monthly_USD)) %>% 
   group_by(income_quintile) %>%
   summarize(min = min(q11j),
             median = median(q11j),
             mean = round(mean(q11j), 0),
             max = max(q11j))
write_csv(t, outputfiles$income_hours_no_electricity)

#Percentage with no generator by income
ci <-  svy %>%
   group_by(income_quintile, no_generator) %>%
   summarize(pct = round(survey_prop(vartype = c( "ci"), proportion = T), 3) * 100) %>% 
   filter(!is.na(income_quintile), !is.na(no_generator))

write_csv(ci, outputfiles$income_no_generator)

#for neighborhood, how many with fixed bill or meter?
ci <-  svy %>%
   filter(q11a_n == 1) %>% 
   group_by(q11c_n_001) %>%
   summarize(pct = round(survey_prop(vartype = c( "ci"), proportion = T), 3) * 100) 


#Reasons why no generator
table(df$q11i)

#1 and 6 most common reasons. What percent?
t <- df %>% 
   filter(q11i != "") %>% 
   group_by(q11i1) %>% 
   summarise(count = n()) %>%
   mutate(perc = count/sum(count))

t <- df %>% 
   filter(q11i != "") %>% 
   group_by(q11i6) %>% 
   summarise(count = n()) %>%
   mutate(perc = count/sum(count))

#of those with no generator, how does the use of alternative sources
#differ geographically?
ci <- svy %>%
   filter(no_generator == "X") %>%
   group_by(Governorate, q11i6) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci")))

table <- df %>%
   filter(no_generator == "X") %>%
   group_by(Governorate, q11i6) %>%
   summarise(count = n())

ci <- svy %>%
   filter(no_generator == "X") %>%
   group_by( q11i6) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci")))

#difficulty paying bills
ci <- svy %>%
   group_by(q19) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(Governorate, q19) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci"))) %>%
   filter(q19 == 1)

#by income
ci <- svy %>%
   group_by(income_quintile, q19) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci"))) %>%
   filter(q19 == 1)

#more difficult to pay since crisis
ci <- svy %>%
   filter(q19 == 1) %>%
   group_by(q20) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#shutoffs
ci <- svy %>%
   group_by(shutoff_bc_unablepay) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(Governorate, shutoff_bc_unablepay) %>%
   summarize(proportion = survey_prop(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci"))) %>%
   filter(shutoff_bc_unablepay == "X") %>%
   select(-shutoff_bc_unablepay)

#type of shutoff
ci <- svy %>%
   group_by(q21) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#EDL shutoff
ci <- svy %>%
   group_by(Governorate, shutoff_EDL) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci"))) 

#any shutoff by income
ci <- svy %>%
   group_by(income_quintile, shutoff_bc_unablepay) %>%
   summarize(proportion = round(survey_prop(vartype = c( "ci")), 3) * 100,
             total = survey_total(vartype = c( "ci"))) %>%
   filter(shutoff_bc_unablepay == "X") %>%
   select(-shutoff_bc_unablepay) %>% 
   filter(!is.na(income_quintile))
write_csv(ci, outputfiles$electricity_shutoff)

#ability to pay
ci <- svy %>%
   group_by(affect_ability_pay_other_essentials) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(Governorate, affect_ability_pay_other_essentials) %>%
   summarize(proportion = survey_mean(vartype = c( "ci")),
             total = survey_total(vartype = c( "ci"))) 

ci <- svy %>%
   group_by(q22) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#ability to pay essentials by income
ci <- svy %>%
   group_by(income_quintile, q22) %>%
   summarize(proportion = round(survey_prop(vartype = c( "ci")), 3) * 100,) %>% 
   filter(!is.na(income_quintile) & q22 == 2)
write_csv(ci, outputfiles$income_pay_essentials)

#affected by shortages
#cook or eat food
#safe temp
t1 <- svy %>%
   group_by(q24a) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Cook or heat food") %>% 
   filter(q24a == 1)

ci <- df %>%
   group_by(income_quintile, q24a, q24a_1) %>%
   summarise(count = n()) %>% 
   group_by(income_quintile) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(!is.na(income_quintile) & q24a == 1 & q24a_1 == 1)

ci <- svy %>%
   group_by(children_in_home, q24a) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#refrigeration
t2 <- svy %>%
   group_by(q24b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Keep food refrigerated/frozen") %>% 
   filter(q24b == 1)

ci <- df %>%
   group_by(income_quintile, q24b) %>%
   summarise(count = n()) %>% 
   group_by(income_quintile) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(!is.na(income_quintile) & q24b == 1 )

ci <- df %>%
   group_by(income_quintile, q24b, affect_food_refridge) %>%
   summarise(count = n()) %>% 
   group_by(income_quintile) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(!is.na(income_quintile) & q24b == 1 )

#water
t3 <- svy %>%
   group_by(q24g) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Receive sufficient water") %>% 
   filter(q24g == 1)

ci <- svy %>%
   group_by(income_quintile, q24g) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   filter(!is.na(income_quintile) & q24g == 1 )

ci <- svy %>%
   filter(q24g == 1) %>%
   group_by(q24g_1) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#safe temp
t4 <- svy %>%
   group_by(q24h) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Keep home at a temperature you feel is safe and healthy") %>% 
   filter(q24h == 1)

t <- df %>% 
   filter(q24h == 1) %>% 
   group_by(q24h_1) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))
   
ci <- svy %>%
   group_by(income_quintile, q24h) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   filter(!is.na(income_quintile) & q24h == 1 )

t4 <- svy %>%
   group_by(q24h) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Keep home at a temperature you feel is safe and healthy") %>% 
   filter(q24h == 1)

#any difference in cooking food if children are present?
df$cook_food <- as.numeric(df$q24a)
table <- df %>%
   group_by(children_present, cook_food) %>%
   summarise(count = n())

table <- table(df$children_present, df$cook_food)
chisq_test(table)

#education
t5 <- svy %>%
   #filter(q24e != 3) %>%
   group_by(q24e) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Participate in an education-related activity") %>% 
   filter(q24e == 1)

ci <- svy %>%
   filter(q24e != 3) %>%
   group_by(q24e) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   filter(q24e != 3) %>%
   group_by(children_in_home, q24e) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   filter(q24e != 3) %>%
   group_by(q24e, children_in_home) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   filter(q24e != 3 & !is.na(income_quintile)) %>%
   group_by(q24e, income_quintile) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#work
t6 <- svy %>%
   group_by(q24f) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Participate in a work-related activity") %>% 
   filter(q24f == 1)

ci <- svy %>%
   #filter(q24f != 3) %>%
   group_by(q24f) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))


ci <- svy %>%
   filter(q24f != 3 & !is.na(income_quintile)) %>%
   group_by(q24f, income_quintile) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   filter(q24f != 3 & !is.na(income_quintile)) %>%
   group_by( income_quintile, q24f) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   filter( !is.na(income_quintile)) %>%
   group_by(q24f, income_quintile) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#shortage affected work and employment status
ci <- svy %>%
   filter(q24f ==1 & !is.na(q24f_2)) %>%
   group_by(q24f_2) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q24f_2) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#access home
t7 <- svy %>%
   group_by(q24i) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Access home") %>% 
   filter(q24i == 1)

ci <- svy %>%
   group_by( income_quintile, q24i) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(older_people_in_home, q24i) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q24i, older_people_in_home) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#bring together
total <- bind_rows(t1, t2, t3, t4, t5, t6, t7) %>% 
   select(cat, pct)

#medical equip & assistive
#what percentage was NA for both?
t <- df %>% 
   group_by(q24c, q24d) %>% 
   summarise(count = n()) %>% 
   ungroup() %>% 
   mutate(perc = round(count/sum(count), 3) * 100)

ci <- svy %>% 
   filter(q24c != 3 & q24d != 3) %>% 
   group_by(interact(q24c, q24d)) %>% 
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>% 
   group_by(interact(q24c, q24d)) %>% 
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q24c) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q24c, income_quintile) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#assistive devices
ci <- svy %>%
   group_by(q24d) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q24d, income_quintile) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   filter(q24d != 3) %>%
   group_by(q24d) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#med equipment or assistive devices
ci <- svy %>%
   group_by(affect_med_equip_or_assistive_device) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(income_quintile, affect_med_equip_or_assistive_device) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#med and disability
ci <- svy %>%
   group_by(disability_in_home, affect_med_equip_or_assistive_device) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#med and elderly
ci <- svy %>%
   group_by(older_people_in_home, affect_med_equip_or_assistive_device) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

### total number of these hardships by income level ###
table <- df %>% 
   select(income_quintile, q24a, q24b, q24e, q24f, q24g, q24h) %>% 
   mutate(across(starts_with("q"), ~ ifelse(.x != 1, NA, .x))) %>% 
   mutate(sum = rowSums(across(starts_with("q")), na.rm = T)) %>% 
   group_by(income_quintile, sum) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100,
          cumperc = cumsum(perc)) %>% 
   filter(!is.na(income_quintile)) %>%
   mutate(label = ifelse(income_quintile == 1, "Bottom 20%",
                         ifelse(income_quintile == 5, "Top 20%", NA))) %>% 
   select(-count) %>% 
   pivot_wider(names_from = sum, values_from = perc)  

write_csv(table, outputfiles$total_burden)

#so, we aren't seeing an increase in number of hardships by income bracket.
plot <- ggplot(table, 
               aes(x = sum,
                   y = perc)) +
   geom_bar(stat="identity", position = "dodge", width=.5) + 
   geom_text(aes(label = scales::percent(perc, accuracy = 1)),
             hjust = -0.1, size = 3,
             position = position_dodge(width = .5)) +
   #scale_fill_manual(values = c("#a059a0", "#ce6693", "#eb7f86",
   #                            "#f8a07e", "#fac484")) +
   theme_agile_meta() +
   coord_flip() + 
   facet_wrap(reorder(income_group, income_rank) ~ .) +
   expand_limits(y = .68) +
   scale_y_continuous(name = "Percentage of Adults in Each Income Group",
                      labels = percent_format(accuracy = 1)) +
   scale_x_discrete(labels = wrap_format(30)) + 
   theme(strip.text.y = element_text(size = 8)) +
   #theme(axis.text.x = element_text(angle=45, vjust = .5, hjust = .5)) +
   theme(legend.position = "bottom") +
   theme(legend.title = element_blank()) +
   #guides(fill = guide_legend(reverse = TRUE, nrow = 3))  +
   labs(x = "Household Income (2019)", 
        title = "Income Disparities in Multiple Stressors", 
        subtitle = "As of 1/18/2021",
        caption = caption)
plot
ggsave(plot, file = outputfiles$exposure_facet_plot, 
       units="in", width = 10, height = 8, dpi = 600)
embed_fonts(outputfiles$exposure_facet_plot)

#percentage of people that had x number of tasks affected
table <- df %>% 
   select(q24a, q24b, q24e, q24f, q24g, q24h, q24i) %>% 
   mutate(across(starts_with("q"), ~ ifelse(.x != 1, NA, .x))) %>% 
   mutate(sum = rowSums(across(starts_with("q")), na.rm = T)) %>% 
   group_by(sum) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100,
          cumperc = cumsum(perc),
          inverse_perc = 100 - cumperc) 

#food insecurity
ci <- svy %>%
   group_by(q36) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

t <- df %>% 
   group_by(income_quintile, q36) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) %>% 
   select(-count) %>% 
   pivot_wider(names_from = q36, values_from = perc)

ci <- svy %>%
   group_by(q37.a) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q37.b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(q37.c) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

t <- df %>% 
   filter(!is.na(income_quintile)) %>% 
   group_by(q37.b, income_quintile) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) 

t1 <- df %>% 
   filter(!is.na(income_quintile)) %>% 
   group_by(income_quintile, q37.a) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) %>% 
   filter(q37.a == 1 ) %>% 
   mutate(cat = "You, or any other adult in your household, had to skip a meal because there was not enough money or other resources to get food")

t2 <- df %>% 
   filter(!is.na(income_quintile)) %>% 
   group_by(income_quintile, q37.b) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) %>% 
   filter(q37.b == 1 ) %>% 
   mutate(cat = "You, or any other adult in your household, went without eating for a whole day because of a lack of money or other resources")

t3 <- df %>% 
   filter(!is.na(income_quintile)) %>% 
   group_by(income_quintile, q37.c) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) %>% 
   filter(q37.c == 1 ) %>% 
   mutate(cat = "Your household ran out of food because of a lack of money or other resources")

table <- bind_rows(t1, t2, t3) %>% 
   select(cat, income_quintile, perc)
rm(t1, t2, t3)

#ran out of food total
ci <- svy %>%
   group_by(q37.c) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#running out of food with children, older people, women-head
ci <- svy %>%
   group_by(children_in_home, q37.b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(older_people_in_home, q37.b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

ci <- svy %>%
   group_by(female_head_household, q37.b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#health concerns related to generator
ci <- svy %>%
   group_by(q13) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

##Mortgage and rent
#ownership status
ci <- svy %>%
   group_by(q30) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#rent and mortgage behind on payment
ci <- svy %>%
   filter(q30 == 2 | q30 == 3) %>%
   group_by(q31) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#rent and mortgage behind on payment
ci <- svy %>%
   filter(q30 == 2 | q30 == 3) %>%
   group_by(q32) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#inability to pay rent
#rent and mortgage behind on payment
ci <- svy %>%
   filter(q30 == 2 | q30 == 3) %>%
   group_by(q35.h) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(q35.h) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

#food security
ci <- svy %>%
   group_by(not_enough_eat_sometimes_often) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(children_in_home, not_enough_eat_sometimes_often) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(female_head_household, not_enough_eat_sometimes_often) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(Governorate, not_enough_eat_sometimes_often) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(not_enough_eat_sometimes_often, received_any_gov_assistance) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(not_enough_eat_sometimes_often, received_any_nongov_assistance) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(not_enough_eat_sometimes_often, received_any_gov_assistance,
            received_any_nongov_assistance) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(q37.a) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(q37.b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(q37.c) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

#lose employment
ci <- svy %>%
   group_by(q33) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

#loss of employment and hours per day of electricity
ci <-  svy %>%
   group_by(q33) %>% 
   summarize(hrs_pr_day = survey_mean(q11j, vartype = c( "ci")))

#cash assistance
ci <- svy %>%
   group_by(any_gov_assistance) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(any_nongov_assistance) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(any_gov_assistance, any_nongov_assistance) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))
t <- df %>%
   group_by(any_gov_assistance,
            any_nongov_assistance) %>%
   summarize(count = n()) %>%
   ungroup() %>%
   mutate(perc = count/sum(count))

t <- df %>%
   group_by(any_gov_assistance, q39a) %>%
   summarize(count = n()) %>%
   mutate(perc = count/sum(count))

t <- df %>%
   filter(q39a == 2 | q39a == 3) %>% 
   group_by( q40a) %>%
   summarize(count = n()) %>%
   mutate(perc = count/sum(count))

t <- df %>%
   filter(q39a == 2 | q39a == 3) %>% 
   group_by( q40a) %>%
   summarize(count = n()) %>%
   mutate(perc = count/sum(count))

ci <- svy %>%
   group_by(received_assistance_FOR_electricity) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))


#what deciles were the households that received gov assistance in?
t<- df %>% 
   group_by(any_gov_assistance, income_decile) %>% 
   summarize(count = n()) %>%
   group_by(income_decile) %>% 
   mutate(perc = count/sum(count)) 

write_csv(t, outputfiles$distribution_gov_assistance)

#government aid and geography
t <- df %>% 
   group_by(any_gov_assistance, Governorate) %>% 
   summarize(count = n()) %>%
   group_by(Governorate) %>% 
   mutate(perc = count/sum(count)) 

#economic standing
ci <- svy %>%
   group_by(q29) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

t <- df %>%
   group_by(income_quintile, q29) %>%
   summarize(count = n()) %>%
   mutate(perc = count/sum(count) * 100) %>% 
   select(income_quintile, q29, perc) %>% 
   pivot_wider(names_from = q29, values_from = perc)

ci <- svy %>%
   group_by(difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

#economic standing but crosstabbed 
ci <- svy %>%
   group_by(Governorate, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci"))) %>% 
   filter(difficulty_making_ends_meet_or_paying_basic_expenses == "X")

ci <- svy %>%
   group_by(children_in_home, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(female_head_household, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

ci <- svy %>%
   group_by(income_quintile, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci"))) %>% 
   filter(!is.na(income_quintile) & 
             difficulty_making_ends_meet_or_paying_basic_expenses == "X")

ci <- svy %>%
   group_by(income_quintile, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci"))) %>% 
   filter(!is.na(income_quintile) & 
             difficulty_making_ends_meet_or_paying_basic_expenses == "X")

ci <- svy %>%
   group_by(lost_work_remain_unemployed, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci"))) 

ci <- svy %>%
   group_by(difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(hrs_pr_day = survey_mean(q11j, vartype = c( "ci"))) 

#difficulty in paying for things.
#heating
t1 <- svy %>%
   filter( q35.j != 3) %>% 
   group_by(q35.j) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Heating") %>% 
   filter(q35.j == 1)

t2 <- svy %>%
   filter( q35.k != 3) %>% 
   group_by(q35.k) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Adequate clothing") %>% 
   filter(q35.k == 1)

t3 <- svy %>%
   filter( q35.a != 3) %>% 
   group_by(q35.a) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Medicine, medical care or health services") %>% 
   filter(q35.a == 1)

t4 <- svy %>%
   filter( q35.e != 3) %>% 
   group_by(q35.e) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Transport (to school, to work)") %>% 
   filter(q35.e == 1)

t5 <- svy %>%
   filter( q35.f != 3) %>% 
   group_by(q35.f) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Education tuition and school materials") %>% 
   filter(q35.f == 1)

t6 <- svy %>%
   filter( q35.i != 3) %>% 
   group_by(q35.i) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Internet") %>% 
   filter(q35.i == 1)

t7 <- svy %>%
   filter( q35.b != 3) %>% 
   group_by(q35.b) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Childcare") %>% 
   filter(q35.b == 1)

t8 <- svy %>%
   filter( q35.g != 3) %>% 
   group_by(q35.g) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "School meals") %>% 
   filter(q35.g == 1)

t9 <- svy %>%
   filter( q35.h != 3) %>% 
   group_by(q35.h) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Rent or mortgage") %>% 
   filter(q35.h == 1)

table <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8, t9) %>% 
   select(cat, pct)
rm(t1, t2, t3, t4, t5, t6, t7, t8, t9)

write_csv(table, outputfiles$difficulty_paying)

#difficulty paying for medical by income
t3 <- svy %>%
   filter( q35.a != 3) %>% 
   group_by(income_quintile, q35.a) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T)) %>% 
   mutate(cat = "Medicine, medical care or health services") %>% 
   filter(q35.a == 1)


#inability pay older people
t <- svy %>% 
   filter(older_people_in_home == "X") %>% 
   group_by(q35.c) %>% 
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#inability pay disabled
t <- svy %>% 
   filter(disability_in_home == "X") %>% 
   group_by(q35.d) %>% 
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T))

#disability
ci <- svy %>%
   group_by(disability_in_home) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))

#disability and difficulty paying for needs
ci <- svy %>%
   group_by(disability_in_home, difficulty_making_ends_meet_or_paying_basic_expenses) %>%
   summarize(pct = survey_prop(vartype = c( "ci"), proportion = T),
             total = survey_total(vartype = c( "ci")))


#multiple difficulty
table <- df %>% 
   select(income_quintile, q35.a, q35.e, q35.h, q35.i, q35.j, q35.k) %>% 
   mutate(across(starts_with("q"), ~ ifelse(.x != 1, NA, .x))) %>% 
   mutate(sum = rowSums(across(starts_with("q")), na.rm = T)) %>% 
   group_by(income_quintile, sum) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) %>% 
   filter(!is.na(income_quintile)) %>%
   mutate(label = ifelse(income_quintile == 1, "Bottom 20%",
                         ifelse(income_quintile == 5, "Top 20%", NA))) %>% 
   select(-count) %>% 
   pivot_wider(names_from = sum, values_from = perc)  

#health and inability to pay essentials
t <- df %>% 
   group_by(q35.a, q22) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100)

#school meals and inability to pay
t <- df %>% 
   group_by(q35.g, q22) %>% 
   summarise(count = n()) %>% 
   mutate(perc = round(count/sum(count), 3) * 100)


