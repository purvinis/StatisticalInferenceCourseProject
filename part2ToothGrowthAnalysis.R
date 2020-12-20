##comments
library(ggplot2)
library(dplyr)
data(ToothGrowth)

p1 <- ggplot(ToothGrowth, aes(dose,len,color = supp))+
      geom_boxplot()+
      facet_grid(dose ~ supp)
print(p1)
