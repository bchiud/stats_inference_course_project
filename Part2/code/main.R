#======================================================================
# Title: Analysis of Simulation Distribution
# Author: Brady Chiu
# Date: May 2, 2016
#======================================================================
## @knitr notes

## @knitr setup
library(data.table, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)
library(knitr, warn.conflicts=F)
library(lubridate, warn.conflicts=F)
library(tidyr, warn.conflicts=F)

# setwd("/Users/bradychiu/Dropbox (Uber Technologies)/R/Coursera/06_Statistical_Inference/stats_inference_course_project/Part2/")

## @knitr data_load
data(ToothGrowth)

## @knitr data_boxplot
ggplot(
  ToothGrowth
  ,aes(x=factor(dose),y=len,fill=factor(dose))
  )+
  geom_boxplot(aes(group=dose))+
  facet_grid(.~supp)+
  scale_x_discrete(name="Dose")+
  scale_y_continuous(name="Length")+
  scale_fill_discrete(name="Dose")+
  theme(
    axis.title=element_text(face="bold",size=12)
    ,axis.text=element_text(size=12)
    )

## @knitr data_summary
summary(ToothGrowth)

## @knitr ttest_supp
t.test(len~supp,data=ToothGrowth,var.equal=T,alternative="two.sided")

## @knitr ttest_dose_1
t.test(len~dose,data=ToothGrowth %>% filter(dose %in% c(0.5,1.0)),var.equal=T,alternative="two.sided")

## @knitr ttest_dose_2
t.test(len~dose,data=ToothGrowth %>% filter(dose %in% c(0.5,2.0)),var.equal=T,alternative="two.sided")

## @knitr ttest_dose_3
t.test(len~dose,data=ToothGrowth %>% filter(dose %in% c(1.0,2.0)),var.equal=T,alternative="two.sided")
