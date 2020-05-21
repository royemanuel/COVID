library(tidyverse)
library(lubridate)
COVID <- read_csv("c:/Users/emanurn1/Downloads/Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## # To use for fills, add
##   scale_fill_manual(values=cbPalette)

## # To use for line and point colors, add
##   scale_colour_manual(values=cbPalette)

GAcovid <-
  COVID %>%
  filter(str_detect(State, "Georgia")) %>%
  select(-`Data as of`,
         -`Start week`,
         -`Footnote`,
         -`Group`,
         -State,
         -Indicator) %>%
  gather(Reason, Deaths, -`End Week`) %>%
  filter(Reason != "Percent of Expected Deaths") %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths),
         date = mdy(`End Week`))

GAcovid_percent <-
    COVID %>%
    filter(str_detect(State, "Georgia")) %>%
    mutate(date = mdy(`End Week`)) %>%
    select(-`Data as of`,
           -`Start week`,
           -`Footnote`,
           -`Group`,
           -State,
           -Indicator) %>%
    select(-`End Week`, date, `COVID-19 Deaths`, `Total Deaths`,
           `Pneumonia Deaths`,
           `Influenza Deaths`, `Pneumonia and COVID-19 Deaths`,
           `Pneumonia, Influenza, or COVID-19 Deaths`) %>%
    mutate(COVIDperc = `COVID-19 Deaths` / `Total Deaths`,
           PneumPerc = `Pneumonia Deaths` / `Total Deaths`,
           InfPerc = `Influenza Deaths` /  `Total Deaths`,
           PneuCOVperc = `Pneumonia and COVID-19 Deaths` /  `Total Deaths`,
           PICperc = `Pneumonia, Influenza, or COVID-19 Deaths` /
               `Total Deaths`) %>%
    select(-1, -2, -3, -4, -5, -6, -7) %>%
    gather(Reason, PercentOfDeaths, -date)


ggplot(GAcovid, aes(date, Deaths, color = Reason)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(GAcovid_percent,
       aes(date, PercentOfDeaths, color = Reason)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


UScovid_percent <-
    COVID %>%
    mutate(date = mdy(`End Week`)) %>%
    select(-`Data as of`,
           -`Start week`,
           -`Footnote`,
           -`Group`,
           -Indicator) %>%
    select(-`End Week`, date, `COVID-19 Deaths`, `Total Deaths`,
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
    select(-2, -3, -4, -5, -6, -7, -8) %>%
    gather(Reason, `Fraction of Deaths`, -date, -State)

ggplot(UScovid_percent,
       aes(date, `Fraction of Deaths`, color = Reason)) +
    geom_point() +
    geom_line() +
    ylim(0, 0.7) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "top") +
    facet_wrap(~ State, ncol = 4) +
    scale_colour_manual(values=cbbPalette) 

