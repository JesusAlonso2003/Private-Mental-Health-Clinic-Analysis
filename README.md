# Private-Mental-Health-Clinic-Analysis
# Clinical Business Analytics Model

## Task

The objective is to understand:

- Revenue drivers
- Marketing efficiency
- Professional-level margin
- Rolling quarterly performance
- Break-even dynamics

## How they work

Revenue:
- €30 first session
- €60 recurring sessions

Marketing:
- €1200 monthly fixed acquisition budget
- Allocated proportionally per new patient

Profit:
Net Profit = Revenue - Fixed Cost - Allocated Marketing

## Basic Insights

- New patient acquisition reduces short-term margin.
- Long-term profitability depends on retention.
- Hour volume is the primary profit driver.
- Marketing sensitivity significantly impacts net margin.

## Other Components

- Rolling 3-month financial model
- LTV estimation
- Marketing scenario simulation
- Regression modeling

## Author
Jesús Alonso

```r
# ==========================================================
# TERESA CALVO HEALTH CARE
# ==========================================================

library(tidyverse)
library(lubridate)
library(zoo)

# ----------------------------
# LOAD DATA 
# ----------------------------

if(!exists("Ingresos.CTC")){
  Ingresos.CTC <- read.csv(file.choose(), header = TRUE)
}

df <- Ingresos.CTC
names(df) <- make.names(names(df))

# ----------------------------
# DATE PROCESSING
# ----------------------------

date_col <- names(df)[1]

df <- df %>%
  mutate(
    raw_date = as.character(.data[[date_col]]),
    month_text = str_extract(raw_date, "[A-Za-z]+"),
    year = str_extract(raw_date, "\\d{4}"),
    month_num = match(month_text,
                      c("Enero","Febrero","Marzo","Abril","Mayo",
                        "Junio","Julio","Agosto","Septiembre",
                        "Octubre","Noviembre","Diciembre")),
    date = as.Date(paste0(year, "-", sprintf("%02d", month_num), "-01"))
  )

# ----------------------------
# NUMERIC CLEANING
# ----------------------------

df <- df %>%
  mutate(across(starts_with("P"), ~as.numeric(.)),
         across(starts_with("N"), ~as.numeric(.)),
         across(starts_with("Cost"), ~as.numeric(.)))

# ----------------------------
# GIVING IT SOME SHAPE
# ----------------------------

sessions <- df %>%
  select(date, starts_with("P")) %>%
  pivot_longer(-date,
               names_to = "professional",
               values_to = "hours")

new_patients <- df %>%
  select(date, starts_with("N")) %>%
  pivot_longer(-date,
               names_to = "np_col",
               values_to = "new_patients") %>%
  mutate(professional = str_replace(np_col, "N", "P")) %>%
  select(-np_col)

fixed_costs <- df %>%
  select(date, starts_with("Cost"), -Cost) %>%
  pivot_longer(-date,
               names_to = "cost_col",
               values_to = "fixed_cost") %>%
  mutate(professional = str_replace(cost_col, "Cost", "P")) %>%
  select(-cost_col)

data <- sessions %>%
  left_join(new_patients, by = c("date","professional")) %>%
  left_join(fixed_costs, by = c("date","professional")) %>%
  mutate(
    hours = replace_na(hours, 0),
    new_patients = replace_na(new_patients, 0),
    fixed_cost = replace_na(fixed_cost, 0)
  )

# ----------------------------
# ECONOMIC MODEL
# ----------------------------

data <- data %>%
  mutate(
    additional_sessions = pmax(hours - new_patients, 0),
    income = new_patients * 30 + additional_sessions * 60
  )

data <- data %>%
  group_by(date) %>%
  mutate(
    total_np = sum(new_patients),
    marketing_cost = ifelse(total_np > 0,
                            new_patients * (1200 / total_np),
                            0)
  ) %>%
  ungroup()

data <- data %>%
  mutate(
    net_monthly = income - fixed_cost - marketing_cost,
    margin = ifelse(income > 0,
                    net_monthly / income,
                    NA)
  )

# ----------------------------
# ROLLING QUARTER
# ----------------------------

data <- data %>%
  arrange(professional, date) %>%
  group_by(professional) %>%
  mutate(
    profit_q = rollapply(net_monthly, 3, sum,
                         align = "right", fill = NA),
    income_q = rollapply(income, 3, sum,
                         align = "right", fill = NA)
  ) %>%
  ungroup()

# ----------------------------
# LTV
# ----------------------------

ltv <- data %>%
  group_by(professional) %>%
  summarise(
    avg_sessions_per_patient = mean(hours / pmax(new_patients,1)),
    est_ltv = avg_sessions_per_patient * 60 - 120,
    .groups = "drop"
  )

print(ltv)

# ----------------------------
# MARKETING SIMULATION
# ----------------------------

simulate_marketing <- function(marketing_budget){
  
  data %>%
    group_by(date) %>%
    mutate(
      total_np = sum(new_patients),
      new_marketing_cost =
        ifelse(total_np > 0,
               new_patients * (marketing_budget / total_np),
               0),
      new_profit = income - fixed_cost - new_marketing_cost
    ) %>%
    ungroup() %>%
    summarise(total_profit = sum(new_profit, na.rm = TRUE))
}

print(simulate_marketing(900))
print(simulate_marketing(1500))
´´´
