library(ggplot2) # pour les graphes
library(reshape2) # pour melt les df

setwd("Z:/Quentin Lamy/Meta3C/")
coverage <- read.csv(file = "coverage_normalized.csv", header = T, sep = ";", dec = ",")

selection <- coverage[-c(13,14,15,16,17),c(1,2,4,6,8,9,10,11)] # sans phages & mt1b1 et un seul ech par jour
colnames(selection) <- c("Species", "-7", "-1", "0", "1", "4", "5", "7")
melt <- melt(selection, id.vars = "Species")
melt$value <- as.numeric(melt$value)

plot_cov <- ggplot(data=melt, aes(x=variable, y=value, group = Species, color = Species, shape = Species)) + geom_point(size = 3) + geom_line(size = 1) +
  ggtitle("") + xlab("Time (Days)") + ylab("Abundance of bacteria (%)") + theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) + expand_limits(y = c(0, 100)) +
  scale_color_manual(name = "Bacterial strains", values = c("#CC79A7", "#56B4E9", "#999999", "#009E73", "#56B4E9", "#56B4E9", "#56B4E9", "#56B4E9", "#56B4E9", "#56B4E9", "#009E73", "#E69F00"), breaks = c("A_muciniphila","B_caecimuris","M_intestinale", "T_muris", "B_animalis", "Clostridioforme", "C_innocuum", "L_reuteri", "B_coccoides", "E_faecalis", "F_plautii", "A_muris")) +
  scale_shape_manual(name = "Bacterial strains", values = c(16,3,16,16,4,15,16,17,18,25,15,16), breaks = c("A_muciniphila","B_caecimuris","M_intestinale", "T_muris", "B_animalis", "Clostridioforme", "C_innocuum", "L_reuteri", "B_coccoides", "E_faecalis", "F_plautii", "A_muris"))
plot_cov

plot_cov + coord_cartesian(ylim=c(0, 20)) # Zoom


new <- coverage[c(13,17),c(1,2,4,6,8,9,10,11)] # phage cocktail et mt1b1
new[2,4] = 0 # on supprime le pic chiant
colnames(new) <- c("Species", "-7", "-1", "0", "1", "4", "5", "7")
melt_new <- melt(new, id.vars = "Species")
melt_new$value <- as.numeric(melt_new$value)
plot_new <- ggplot(data=melt_new, aes(x=variable, y=value, group = Species, color = Species)) + geom_point(size = 4) + geom_line(size = 1.5) +
  ggtitle("") + xlab("Time (Days)") + ylab("Abundance (%)") + theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) + expand_limits(y = c(0, 100)) + coord_cartesian(ylim=c(0, 0.20)) + scale_colour_manual(values = c("#0072B2", "#F0E442"), name = "")
plot_new
