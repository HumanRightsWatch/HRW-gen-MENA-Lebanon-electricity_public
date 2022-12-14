#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive - Human Rights Watch/HRW/HRW-gen-MENA-Lebabon-electricity/descriptives/src/lebanon_electricity_respondent_followup.r
library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, rstatix, #get summary table function#
       survey, srvyr, haven #for importing spss#
       )
options(scipen=999)

here <- here::here

source("../plot_themes.R")

ntile_na <- function(x,n)
{
   notna <- !is.na(x)
   out <- rep(NA_real_,length(x))
   out[notna] <- ntile(x[notna],n)
   return(out)
}

########### input and output files ##############
# input files:
inputfiles <- list(
   survey = "import/input/anonymized_data.csv",
   xchange = "import/input/usd-to-lbp-buy-exchange.csv"
   
   
) %>% map(here)

#output files. 
outputfiles <- list(
   respondent_database = "descriptives/output/respondent_followup.csv",
   bill_table = "processing/output/bill_table.csv",
   bills_per_ampereHR = "processing/output/bill_ampere.csv",
   building_amper_HR_example = "processing/output/building_example.csv",
   df = "processing/output/df.rds"
 
) %>% map(here)

#read in
df <- read_csv(inputfiles$survey)

#convert some dates
df <- df %>%
   mutate(today = as_date(today, format = "%m/%d/%y"),
          q8 = as_date(q8, format = "%m/%d/%y"),
          q15 = as_date(q15, format = "%m/%d/%y")) %>%
   rename(Governorate = q45, Caza = q46)

#surveys attempted
df <- df %>%
   filter(!is.na(log_sheet)) 

t <- df %>%
   group_by(log_sheet) %>%
   summarise(count = n()) %>%
   mutate(perc = count/sum(count))

#filter only those living there since 2019
df <- df %>%
   filter(q1 == 1 & q2 == 1)

#rename and create variables of interest for contacting
df <- df %>%
   rename(monthly_income_thousandLBP = q28a,
          monthly_income_USD = q28b,
          monthly_income_lollars = q28c) %>%
   mutate(older_people_in_home = ifelse(q53 > 0, "X", NA),
          children_in_home = ifelse(q54 > 0, "X", NA),
          number_of_children = q54,
          shutoff_bc_unablepay = ifelse(
             q21 != 4, "X", NA),
          female_head_household = ifelse(
             q51 == 1 & q50 == 1, "X", NA),
          no_generator = ifelse(
             q11a_n != 1 & q11a_b != 1 & q11a_o !=1, "X", NA),
         affect_safe_temperature_neareveryday = ifelse(
             q24h == 1 & q24h_1 == 1, "X", NA),
         affect_med_equip_or_assistive_device = 
            ifelse(q24c == 1 | q24d == 1, "X", NA),
         affect_ability_pay_other_essentials = ifelse(
            q22 != 1, "X", NA),
         affect_education_near_everyday = ifelse(
            q24e == 1 & q24e_1 == 1, "X", NA),
         affect_work_near_everyday = ifelse(
            q24f == 1 & q24f_1 == 1, "X", NA),
         difficulty_pay_electric_bill = ifelse(q19 == 2, "X", NA),
         not_enough_eat_sometimes_often = ifelse(
            q36 == 3 | q36 == 4, "X", NA),
         difficulty_making_ends_meet_or_paying_basic_expenses = ifelse(
            q29 == 3 | q29 == 4, "X", NA),
         received_assistance_FOR_electricity = ifelse(
            q44a == 1 | q44b == 1, "X", NA),
         received_any_gov_assistance = ifelse(
            !is.na(q26_f), "X", NA),
         received_any_nongov_assistance = ifelse(
            !is.na(q26_g), "X", NA),
         not_lebanese = ifelse(
            q49 == 2, "X", NA),
         disability_in_home = ifelse(
          q55 == 1, "X", NA),
         generator_too_expensive = 
            ifelse(
               q11i1 == 1, "X", NA),
         affect_ability_access_home = 
            ifelse(
               q24i == 1, "X", NA),
         affect_cooking = 
            ifelse(
               q24a == 1, "X", NA),
         affect_food_refridge = 
            ifelse(
               q24b == 1, "X", NA),
         affect_employment_status = 
            ifelse(
               q24f_2 == 1, "X", NA),
         blackouts_tolerable = 
            ifelse(
               q11i5 == 1, "X", NA),
         generators_not_avail = 
            ifelse(
               q11i3 == 1, "X", NA),
         health_concerns_due_to_generator = 
            ifelse(
               q13 == 1, "X", NA),
         lost_work_remain_unemployed = 
            ifelse(
               q33 == 1, "X", NA),
         use_solar = 
            ifelse(
               q14_a2 == 1, "X", NA),
         ration_card = 
            ifelse(
               q38a2 == 1, "X", NA),
         national_poverty_program = 
            ifelse(
               q38a2 == 1, "X", NA),
         municipality_assistance = 
            ifelse(
               q38a3 == 1, "X", NA),
         political_party_assistance = 
            ifelse(
               q38b5 == 1, "X", NA))


###Monthly Income
xchange <- read_csv(inputfiles$xchange) %>%
   mutate(date = as_datetime(DateTime, 
                             format = "%m/%d/%y")) %>%
   select(-DateTime) %>%
   distinct() %>%
   group_by(date) %>%
   slice(1)

df <- left_join(df, xchange, by = c("today" = "date"))

df <- df %>%
   mutate_at(vars(starts_with("monthly_")), ~replace_na(., 0)) %>%
   mutate(monthly_income_pounds_in_dollars = 
             (monthly_income_thousandLBP * 1000)/`USD to LBP`,
          monthly_lollars_in_dollars = monthly_income_lollars/8000,
          total_monthly_USD = 
             round(monthly_income_pounds_in_dollars + monthly_lollars_in_dollars +
             monthly_income_USD, 0)) %>%
   mutate_at(vars(starts_with("monthly_")), ~na_if(., 0)) %>%
   mutate(total_monthly_USD = na_if(total_monthly_USD, 0)) %>%
   mutate(did_not_report_income = ifelse(is.na(total_monthly_USD), "X", NA))

table(df$did_not_report_income)

#convert summer 2019 bills to USD
df <- df %>%
   mutate(summer19_EDL_USD = ifelse(q7 == 999, NA, 
                                    (q7*1000)/1500),
          summer19_neighbor_gen_USD = ifelse(q12e_n == 999, NA, 
                                             (q12e_n*1000)/1500),
          summer19_building_gen_USD = ifelse(q12e_b == 999, NA, 
                                             (q12e_b*1000)/1500),
          summer19_own_gen_USD = ifelse(q12e_o == 999, NA, 
                                        (q12e_o*1000)/1500))

t <- df %>%
   group_by(q7, summer19_EDL_USD) %>%
   summarise(count = n())

#most recent bill for people with a bill since august 21
#get monthly average exchange - 999 is no answer
monthly_exchange = xchange %>%
   mutate(floor = floor_date(date, "monthly")) %>%
   group_by(floor) %>%
   summarise(monthly_avg_xchange = mean(`USD to LBP`))

df <- left_join(df, monthly_exchange, by = c("q8" = "floor"))

df <- df %>%
   mutate(latest_edlbill_usd = ifelse(q9 == 999, NA, 
      (q9*1000)/monthly_avg_xchange))



#previous month generator bills (so average from Nov 2021) in USD
nov_xchange <- 22679

df <- df %>%
   mutate(latest_neighbor_gen_USD = ifelse(q11e_n == 999, NA, 
                                           (q11e_n*1000)/nov_xchange),
          latest_building_gen_USD = ifelse(q11e_b == 999, NA, 
                                           (q11e_b*1000)/nov_xchange),
          latest_own_gen_USD = ifelse(q11e_o == 999, NA, 
                                      (q11e_o*1000)/nov_xchange)) %>% 
   rowwise() %>% 
   mutate(total_gen_USD = sum(c_across(latest_neighbor_gen_USD:latest_own_gen_USD),
                              na.rm = T)) %>% 
   ungroup()

#generator bills as percentage of income
df <- df %>% 
   mutate(gen_bills_as_perc_of_income = total_gen_USD/total_monthly_USD)


#alternative source installation and bill - 999 is NA?
df <- df %>%
   mutate(alternative_source_installation_USD = ifelse(
      q16 == 999, NA, q16
   ))
df <- df %>%
   mutate(alternative_source_lastbill_USD =
             ifelse(q17 == 999, NA, (q17*1000)/nov_xchange))


#calculate ampere-hours per day
df <- df %>%
   mutate(neighborhood_ampere_hours = q11b_n*q11c_n,
          building_ampere_hours = q11b_b*q11c_b,
          own_ampere_hours = q11b_o*q11c_o, 
          precrisis_neighborhood_ampereHRs = q12b_n * q12c_n,
          precrisis_building_ampereHRS = q12b_b * q12c_b,
          precrisis_own_ampereHRS = q12b_o * q12c_o)


##bills pre and post by ampere hours
#USD cost per ampere hours
df <- df %>%
   mutate(USD_per_building_gen_current = 
             latest_building_gen_USD/building_ampere_hours, 
          USD_per_neighborhood_gen_current = 
             latest_neighbor_gen_USD/neighborhood_ampere_hours,
          USD_per_own_gen_current = 
             latest_own_gen_USD/own_ampere_hours,
          USD_per_building_gen_precrisis = 
             summer19_building_gen_USD/precrisis_building_ampereHRS,
          USD_per_neighborhood_gen_precrisis = 
             ifelse(precrisis_neighborhood_ampereHRs != 0, 
                    summer19_neighbor_gen_USD/precrisis_neighborhood_ampereHRs,
                    NA),
          USD_per_own_gen_precrisis = 
             summer19_own_gen_USD/precrisis_own_ampereHRS)


#function  for counting NAs across columns
ntile_na <- function(x,n)
{
   notna <- !is.na(x)
   out <- rep(NA_real_,length(x))
   out[notna] <- ntile(x[notna],n)
   return(out)
}

ntile_na <- function(x,n)
{
   notna <- !is.na(x)
   out <- rep(NA_real_,length(x))
   out[notna] <- ntile(x[notna],n)
   return(out)
}



#code households into USD income decile, quintile, quartile
df <- df %>% 
   mutate(income_decile = ntile_na(total_monthly_USD, 10),
          income_quartile = ntile_na(total_monthly_USD, 4),
          income_quintile = ntile_na(total_monthly_USD, 5)) 

#Edl shutoff, generator shutoff
df <- df %>%
   mutate(shutoff_EDL = ifelse(
      q21 == 1 | q21 == 3, 1, NA),
      generator_shutoff = ifelse(
         q21 == 2 | q21 == 3, 1, NA))

#children binary variable
df <- df %>%
   mutate(children_present = ifelse(
      is.na(children_in_home), 0, 1))

#gov and nongov assistance - binary variable
df <- df %>%
   mutate(any_gov_assistance = ifelse(received_any_gov_assistance == "X",
                                      1, 0),
          any_nongov_assistance = ifelse(received_any_nongov_assistance
                                         == "X", 1, 0))


#"assistance" in other form of income
df <- df %>%
   mutate(other_income_source = case_when(
      str_detect(q26_h_o, "Assistance") ~ "Assistance",
      is.na(q26_h) ~ "NA",
      TRUE ~ "Other"
      ))

#income_sources
df <- df %>% 
   mutate(income_source = case_when(
      q26_a == 100 ~ "Business/self employment",
      q26_b == 100 ~ "Wage employment",
      q26_c == 100 ~ "Asset earnings",
      q26_d == 100 | (q26_h == 100 & other_income_source == "Assistance") 
                      ~ "Remittances/assistance from family/others", 
      q26_e == 100 ~ "Pension",
      q26_f == 100 ~ "Government assistance",
      TRUE ~ "Multiple sources"
   ),
   any_assistance_remittances = ifelse(
      other_income_source == "Assistance" | q26_d > 0,
      1, 0
   ))
          

#save data
write_rds(df, outputfiles$df)
