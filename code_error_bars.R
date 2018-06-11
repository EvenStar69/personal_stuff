bact <- read.csv("bacteria_days.csv", sep = ";", dec = ".", header = T)
bact$Sample <- as.character(bact$Sample)
bact_stat <- rbind(bact, c("Mean", colMeans(bact[,-1])))
bact_stat <- rbind(bact_stat, c("SD", apply(X = bact[,-1], 2,sd)))

bact_melt <- melt(bact, id.var = "Sample")
bact_melt$value <- as.numeric(bact_melt$value)

ggplot(data=bact_melt, aes(x=variable, y=value, color = Sample)) + geom_point(size = 2.5) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  ggtitle("Evolution of the quantity of Phage in feces") + xlab("Time") + ylab("Phage (PFU/g of feces)") + theme(plot.title = element_text(hjust = 0.5)) + expand_limits(y = c(10^3, 10^8))
  
ggplot(data=bact_melt, aes(x=variable, y=value, color = Sample)) + geom_point(size = 2.5) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  ggtitle("My_Title") + xlab("My_X") + ylab("My_Axis") + theme(plot.title = element_text(hjust = 0.5)) + expand_limits(y = c(10^3, 10^8))

library(dplyr)    
bact_melt <- bact_melt %>% group_by(variable) %>% mutate(upper =  quantile(value, 0.75), lower = quantile(value, 0.25), mean = mean(value))

ggplot(data=bact_melt, aes(x=variable, y=value, color = Sample)) + geom_point(size = 2.5) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  ggtitle("My_Title") + xlab("My_X") + ylab("My_Axis") + theme(plot.title = element_text(hjust = 0.5)) + expand_limits(y = c(10^3, 10^8)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper),col = "red", width =  0.25) + geom_point(aes(x = variable, y = mean), size = 3, col = "red")
