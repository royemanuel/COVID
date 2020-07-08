
library(tidyverse)
library(lubridate)
COVID_18MAY <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_18MAY.csv")
COVID_21MAY <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_21MAY.csv")
COVID_26MAY <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_26MAY.csv")
COVID_1JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_1JUN.csv")
COVID_8JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_8JUN.csv")
COVID_19JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_19JUN.csv")
COVID_29JUN <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_29JUN.csv")
COVID_6JUL <- read_csv("Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State_6JUL.csv")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

COVID <- bind_rows(COVID_18MAY, COVID_21MAY, COVID_26MAY, COVID_1JUN, COVID_8JUN, COVID_19JUN, COVID_29JUN, COVID_6JUL)

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
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(UScovid, aes(date, Deaths, color = Reason, shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(GAcovid_percent,
       aes(date, PercentOfDeaths, color = Reason, shape = `Data as of`,
                    linetype = `Data as of`)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


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
    scale_colour_manual(values=cbbPalette) 

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
    scale_colour_manual(values=cbbPalette) 
