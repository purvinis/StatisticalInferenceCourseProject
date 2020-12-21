##comments
library(ggplot2)
library(dplyr)
library(RColorBrewer)
data(ToothGrowth)

p1 <- ggplot(ToothGrowth, aes(supp,len,color = supp,fill=supp))+
      geom_boxplot()+
      facet_grid(.~dose)+
      scale_color_manual(values = c("#000000","#000000"))+
      scale_fill_manual(values = c("#FF9900","#9900CC"))
      
print(p1)



d1plot <- ggplot(ToothGrowth,aes(len,color = supp,fill = supp))+
      facet_grid(.~dose)+
      geom_histogram(aes(y = ..density..),color = "black",binwidth = 1)+
      geom_density(size =2)+
      scale_color_manual(values = c("#FF4466","#9900CC"))+
      scale_fill_manual(values = c(alpha("#FF9900",0.3),alpha("#9900CC",.3)))
print(d1plot)

#Ho: The source of the vit C makes no difference;ie. no difference in means
#H1: 
overall <- t.test(ToothGrowth$len ~ ToothGrowth$supp,alt = "two.sided")

d1 <- filter(ToothGrowth,dose == 0.5)
d2 <- filter(ToothGrowth,dose == 1.0)
d3 <- filter(ToothGrowth,dose == 2.0)

dose1 <- t.test(d1$len ~ d1$supp,alt = "two.sided")
dose2 <- t.test(d2$len ~ d2$supp,alt = "two.sided")
dose3 <- t.test(d3$len ~ d3$supp,alt = "two.sided")

print(overall)
print(dose1)
print(dose2)
print(dose3)



