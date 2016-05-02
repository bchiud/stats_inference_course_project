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

# setwd("/Users/bradychiu/Dropbox (Uber Technologies)/R/Coursera/06_Statistical_Inference/stats_inference_course_project")

## @knitr functions
simulator<-function(n,lambda,simulation_count,seed=1337){
  set.seed(seed)
  simulation_means<-data.frame(mean=numeric(simulation_count))
  for(i in 1:simulation_count){
    simulation_means[i,]<-mean(rexp(n,lambda))
  }
  simulation_means
}

## @knitr parameters
exponentials<-40
lambda<-0.2
simulation_count<-1000

## @knitr simulation_result
simulation_result<-simulator(exponentials,lambda,simulation_count)
head(simulation_result,10)

## @knitr mean
theoretical_mean<-1/lambda
sample_mean<-mean(simulation_result$mean)
paste("Theoretical Mean:",theoretical_mean)
paste("Sample Mean:",sample_mean)

## @knitr variance
theoretical_variance<-((1/lambda)^2)/simulation_count
sample_variance<-var(simulation_result$mean)
paste("Theoretical Variance:",theoretical_variance)
paste("Sample Variance:",sample_variance)

## @knitr plot_1
ggplot(
  data=simulation_result
  ,aes(x=mean)
  )+
  geom_histogram(binwidth=0.25,color="black",fill="yellow")+
  geom_vline(aes(xintercept=theoretical_mean),color="red",show.legend=TRUE)+
  geom_label(aes(label="Theoretical Mean",x=theoretical_mean,y=130,hjust=1.1),color="red")+
  geom_vline(aes(xintercept=sample_mean),color="blue",show.legend=TRUE)+
  geom_label(aes(label="Sample Mean",x=theoretical_mean,y=130,hjust=-0.2),color="blue")+
  ggtitle("Simulation Mean")+
  scale_x_continuous(name="Simulation Means")+
  scale_y_continuous(name="Simulations")+
  theme(
    axis.title=element_text(face="bold",size=12)
    ,axis.text=element_text(size=12)
    )

## @knitr plot_2
ggplot(
  data=simulation_result
  ,aes(x=mean)
  )+
  geom_histogram(binwidth=0.25,color="black",fill="yellow")+
  stat_density(aes(y=0.25*..count..,alpha=0.9,size=0.5),color="blue")+
  ggtitle("Simulation Mean Distribution")+
  scale_x_continuous(name="Simulation Means Distribution")+
  scale_y_continuous(name="Simulations")+
  theme(
    axis.title=element_text(face="bold",size=12)
    ,axis.text=element_text(size=12)
    ,legend.position="none"
    )

## @knitr plot_3
ggplot(
  data=simulation_result
  ,aes(sample=mean)
  )+
  geom_qq(color="yellow")+
  geom_abline(slope=1,intercept=5)+
  ggtitle("Simulation Mean Quantile-Quantile")+
  scale_x_continuous(name="Theoretical Quantiles")+
  scale_y_continuous(name="Sample Quantiles")+
  theme(
    axis.title=element_text(face="bold",size=12)
    ,axis.text=element_text(size=12)
    ,legend.position="none"
  )