library(tidyverse)
library(lubridate)
library(readxl)
setwd("c:/Users/emanurn1/Documents/Work/COVID")
COVID_18MAY <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_18MAY.csv")
COVID_21MAY <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_21MAY.csv")
COVID_26MAY <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_26MAY.csv")
COVID_1JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_1JUN.csv")
COVID_8JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_8JUN.csv")
COVID_19JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_19JUN.csv")
COVID_29JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_29JUN.csv")
COVID_6JUL <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_6JUL.csv")
COVID_16JUL <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_16JUL.csv")
COVID_21JUL <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_21JUL.csv")
COVID_31JUL <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_31JUL.csv")
COVID_12AUG <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_12AUG.csv")
COVID_27AUG <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_27AUG.csv")
COVID_28SEP <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_28SEP.csv")
COVID_15OCT <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_15OCT.csv")
COVID_25JAN21 <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_25JAN21.csv", ) %>% mutate(`End Week` = `End Date`)

COVID_25JAN21 <- COVID_25JAN21[,colnames(COVID_25JAN21) %in% colnames(COVID_16JUL)]

COVID_CASE_DEATH <- read_csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time_26JAN.csv")
#COVID_CASE_DEATH_25JAN <- read_csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time_25JAN21.csv")

USpop <- read_xlsx("2019gender_table1.xlsx", skip = 5) %>%
    mutate(type = "Pop")

USpop <- USpop[2:19, 1:3]



cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

COVID <- bind_rows(## COVID_18MAY,
                   ## COVID_21MAY,
                   ## COVID_26MAY,
                   ## COVID_1JUN,
                   ## COVID_8JUN,
                   ## COVID_19JUN,
                   ## COVID_29JUN,
                   ## COVID_6JUL,
    ##COVID_16JUL,
    ##COVID_21JUL,
    ##COVID_31JUL,
    ##COVID_12AUG,
    COVID_27AUG,
    COVID_28SEP,
    COVID_15OCT,
    COVID_25JAN21)

COVID <-
    COVID %>%
    mutate(`End Week` = str_replace(`End Week`, pattern = "0020", replacement = "2020"))
## # To use for fills, add
##   scale_fill_manual(values=cbPalette)

## # To use for line and point colors, add
##   scale_colour_manual(values=cbPalette)

GAcovid <-
  COVID %>%
  filter(str_detect(State, "Georgia")) %>%
  dplyr::select(-`Start week`,
         -`Footnote`,
         -`Group`,
         -State,
         -Indicator) %>%
  gather(Reason, Deaths, -`End Week`, -`Data as of`) %>%
  filter(Reason != "Percent of Expected Deaths") %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths),
         date = mdy(`End Week`))

UScovid <-
  COVID %>%
  filter(str_detect(State, "United States")) %>%
  dplyr::select(-`Start week`,
         -`Footnote`,
         -`Group`,
         -State,
         -Indicator) %>%
  gather(Reason, Deaths, -`End Week`, -`Data as of`) %>%
  filter(Reason != "Percent of Expected Deaths") %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths),
         date = mdy(`End Week`))

GAcovid_percent <-
    COVID %>%
    filter(str_detect(State, "Georgia")) %>%
    mutate(date = mdy(`End Week`)) %>%
    dplyr::select(-`Start week`,
           -`Footnote`,
           -`Group`,
           -State,
           -Indicator) %>%
    dplyr::select(-`End Week`, date, `COVID-19 Deaths`, `Total Deaths`,
           `Pneumonia Deaths`,
           `Influenza Deaths`, `Pneumonia and COVID-19 Deaths`,
           `Pneumonia, Influenza, or COVID-19 Deaths`) %>%
    mutate(COVIDperc = `COVID-19 Deaths` / `Total Deaths`,
           PneumPerc = `Pneumonia Deaths` / `Total Deaths`,
           InfPerc = `Influenza Deaths` /  `Total Deaths`,
           PneuCOVperc = `Pneumonia and COVID-19 Deaths` /  `Total Deaths`,
           PICperc = `Pneumonia, Influenza, or COVID-19 Deaths` /
               `Total Deaths`) %>%
    dplyr::select(-2, -3, -4, -5, -6, -7, -8) %>%
    gather(Reason, PercentOfDeaths, -date, -`Data as of`)


ggplot(GAcovid, aes(date, Deaths, color = Reason, shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_bw()

ggplot(UScovid, aes(date, Deaths, color = Reason, shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_bw()

ggplot(GAcovid_percent,
       aes(date, PercentOfDeaths, color = Reason, shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_bw()


UScovid_percent <-
    COVID %>%
    mutate(date = mdy(`End Week`)) %>%
    dplyr::select(-`Start week`,
           -`Footnote`,
           -`Group`,
           -Indicator) %>%
    dplyr::select(-`End Week`, date, `COVID-19 Deaths`, `Total Deaths`,
           `Pneumonia Deaths`,
           `Influenza Deaths`, `Pneumonia and COVID-19 Deaths`,
           `Pneumonia, Influenza, or COVID-19 Deaths`) %>%
    mutate(`COVID-19\nPercentage` = `COVID-19 Deaths` / `Total Deaths`,
           `Pneumonia\nPercentage` = `Pneumonia Deaths` / `Total Deaths`,
           `Influenza\nPercentage` = `Influenza Deaths` /  `Total Deaths`,
           `Pneumonia & COVID-19\nPercentage` = `Pneumonia and COVID-19 Deaths` /  `Total Deaths`,
           `Pneumonia or Influenze or COVID-19\nPercentage` =
               `Pneumonia, Influenza, or COVID-19 Deaths` /
               `Total Deaths`) %>%
    dplyr::select(-3, -4, -5, -6, -7, -8, -9) %>%
    gather(Reason, `Fraction of Deaths`, -date, -State, -`Data as of`) %>%
    mutate(`Fraction of Deaths` = ifelse(is.na(`Fraction of Deaths`), 0,
                                         `Fraction of Deaths`))

ggplot(UScovid_percent,
       aes(date, `Fraction of Deaths`, color = Reason, shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    ylim(0, 0.7) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "top") +
    facet_wrap(~ State, ncol = 4) +
    scale_colour_manual(values=cbbPalette)  +
    theme_bw()

US_percent_CIP <-
    UScovid_percent %>%
    filter(Reason == unique(UScovid_percent$Reason)[5])

ggplot(US_percent_CIP,
       aes(date, `Fraction of Deaths`, color = `Data as of`,
           shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    ylim(0, 0.7) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "top") +
    facet_wrap(~ State, ncol = 4) +
    scale_colour_manual(values=cbbPalette)  +
    theme_bw()


cVSd <-
    COVID_CASE_DEATH %>%
    mutate(CaseDeathRatio = tot_cases / tot_death) %>%
    pivot_longer(cols = c(3:12),
                 names_to = "Observation_Type",
                 values_to = "Number_Observed") %>%
    mutate(submission_date = mdy(submission_date))

ggplot(cVSd,
       aes(submission_date, Number_Observed, color = Observation_Type)) +
    geom_line() +
    facet_wrap(~ state, ncol = 5)

newCaseVsDeath <-
    cVSd %>%
    filter(Observation_Type == "new_case" |
           Observation_Type == "new_death")

ggplot(newCaseVsDeath,
       aes(submission_date, Number_Observed, color = Observation_Type)) +
    geom_line() +
    facet_wrap(~ state, ncol = 5)

cumCaseVsDeath <-
    cVSd %>%
    filter(Observation_Type == "tot_cases" | Observation_Type == "tot_death")

ggplot(cumCaseVsDeath,
       aes(submission_date, Number_Observed, color = Observation_Type)) +
    geom_line() +
    facet_wrap(~ state, ncol = 5)

entireUS <-
    cVSd %>%
    group_by(submission_date, Observation_Type) %>%
    summarise(Total = sum(Number_Observed))

ggplot(filter(entireUS, Observation_Type == "new_case" | Observation_Type == "new_death"),
       aes(submission_date, Total, color = Observation_Type)) +
    geom_line()

ggplot(filter(entireUS, Observation_Type == "tot_cases" | Observation_Type == "tot_death"),
       aes(submission_date, Total, color = Observation_Type)) +
    geom_line()

entireUS <-
    entireUS %>%
    mutate(Log_Total = log10(Total))

ggplot(filter(entireUS, Observation_Type == "tot_cases" | Observation_Type == "tot_death"),
       aes(submission_date, Log_Total, color = Observation_Type)) +
    geom_line()

demData <- read_csv("Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State _25JAN21.csv")

US_now <-
    demData %>%
    filter(str_detect(`End Date`,"01/16/2021"),
           str_detect(State, "Unit")) %>%
    mutate(Death_Percent = 100 * `COVID-19 Deaths` / max(`COVID-19 Deaths`)) %>%
    mutate(Age_Group = factor(`Age group`)) %>%
    filter(!str_detect(Age_Group, "0-17"),
           !str_detect(Age_Group, "18-29"),
           !str_detect(Age_Group, "30-49"),
           !str_detect(Age_Group, "50-64"),
           !str_detect(Age_Group, "All"))

US_now$Age_Group <-fct_drop(US_now$Age_Group)
US_now$Age_Group <- fct_relevel(US_now$Age_Group,
                                c("Under 1 year",
                                  "1-4 years",
                                  "5-14 years",
                                  "15-24 years",
                                  "25-34 years",
                                  "35-44 years",
                                  "45-54 years",
                                  "55-64 years",
                                  "65-74 years",
                                  "75-84 years",
                                  "85 years and over"))





US_now_allSex <-
    US_now %>%
    filter(str_detect(Sex, "All"))

ggplot(US_now_allSex, aes(Age_Group, Death_Percent, fill = Sex)) +
    geom_col(position = "dodge") +
    geom_text(data = US_now_allSex,
              aes(y = Death_Percent, x = Age_Group,
                  label = paste(round(Death_Percent, digits = 2)))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .3))

## California data
CA <- read_csv("CaliforniaNumbers.csv")
CA$`Age Group`[2] <- "5-17"

CA <-
    CA %>%
    mutate(`Cases per Death` = `No. Cases` / `No. Deaths`) %>%
    filter(!str_detect(`Age Group`, "Total")) %>%
    pivot_longer(!`Age Group`, names_to = "count_type", values_to = "Value")

CA$`Age Group` <- factor(CA$`Age Group`)

CA$`Age Group` <- fct_relevel(
    CA$`Age Group`,
    c("<5",
      "5-17",
      "18-34",
      "35-49",
      "50-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80+",
      "missing"))
                          

CA_percent <-
    CA %>%
    filter(str_detect(count_type, "Percent")) 

CA_cases <-
    CA %>%
    filter(str_detect(count_type, "per"))

CA_count <-
    CA %>%
    filter(str_detect(count_type, "No."))


ggplot(CA_percent,
       aes(`Age Group`, Value, color = count_type, fill = count_type)) +
    geom_col(position = "dodge") +
    theme_bw()

ggplot(CA_count,
       aes(`Age Group`, Value, color = count_type, fill = count_type)) +
    geom_col(position = "dodge") +
    theme_bw()


ggplot(CA_cases,
       aes(`Age Group`, Value)) +
    geom_col() +
    geom_text(data = CA_cases,
              aes(y = Value, x = `Age Group`, vjust = -1,
                  label = paste(round(Value, digits = 2))), ) +   
    theme_bw() +
    labs(title = "Number of Positive Cases for Every Death")

