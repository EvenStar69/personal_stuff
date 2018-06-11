setwd("Z:\\Quentin Lamy\\Results/Bacteria_Phage_counts/")
counts <- read.csv("Exp_Q1_counts.csv", dec = ".", sep = ";", header = FALSE)
counts_bf <- counts[2:5, 1:6]; colnames(counts_bf) = c("Sample", "0", "1", "4", "5", "7")
counts_bt <- counts[8:11, 1:7]; colnames(counts_bt) = c("Sample", "Ileum", "Ileum content", "Caecum", "Colon", "Colon content", "Feces")
counts_pf <- counts[14:17, 1:4]; colnames(counts_pf) = c("Sample", "4", "5", "7")
counts_pt <- counts[20:23, 1:7]; colnames(counts_pt) = c("Sample", "Ileum", "Ileum content", "Caecum", "Colon", "Colon content", "Feces")

library(ggplot2) # pour les graphes
library(reshape2) # pour melt les df
library(scales) # pour les log scale

counts_bfm <- melt(counts_bf, id.var = "Sample")
counts_btm <- melt(counts_bt, id.var = "Sample")
counts_pfm <- melt(counts_pf, id.var = "Sample")
counts_ptm <- melt(counts_pt, id.var = "Sample")
  
counts_bfm$value <- as.numeric(counts_bfm$value)
counts_btm$value <- as.numeric(counts_btm$value)
counts_pfm$value <- as.numeric(counts_pfm$value)
counts_ptm$value <- as.numeric(counts_ptm$value)

plot_bf <- ggplot(data=counts_bfm, aes(x=variable, y=value, color = Sample)) + geom_point(size = 5, position=position_jitter(width=0.2, height=0)) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("") + xlab("Time (Days)") + ylab("Mt1b1 (CFU/g of feces)") + theme(plot.title = element_text(hjust = 0.5, size = 25), axis.text=element_text(size=20), axis.title=element_text(size=15), panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = "white",size=0.75)) + expand_limits(y = c(10^3, 10^8)) 
plot_bf
plot_pf <- ggplot(data=counts_pfm, aes(x=variable, y=value, color = Sample)) + geom_point(size = 5, position=position_jitter(width=0.15, height=0)) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  ggtitle("") + xlab("Time (Days)") + ylab("Phage (PFU/g of feces)") + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=20), axis.title=element_text(size=15), panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = "white",size=0.75)) + expand_limits(y = c(10^3, 10^8))
plot_pf
plot_bt <- ggplot(data=counts_btm, aes(x=variable, y=value, color = Sample)) + geom_point(size = 5, position=position_jitter(width=0.2, height=0)) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("") + xlab("Organs") + ylab("Mt1b1 (CFU/g of tissue)") + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=15), axis.title=element_text(size=15), panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = "white",size=0.75)) + expand_limits(y = c(10^3, 10^8))
plot_bt
plot_pt <- ggplot(data=counts_ptm, aes(x=variable, y=value, color = Sample)) + geom_point(size = 5, position=position_jitter(width=0.15, height=0)) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("") + xlab("Organs") + ylab("Phage (PFU/g of tissue)") + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=15), axis.title=element_text(size=15), panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = "white",size=0.75)) + expand_limits(y = c(10^3, 10^8))
plot_pt

ggsave(filename = "graphs\\bacteria_over_time.png", plot = plot_bf)
ggsave(filename = "graphs\\phage_over_time.png", plot = plot_pf)
ggsave(filename = "graphs\\bacteria_organs.png", plot = plot_bt)
ggsave(filename = "graphs\\phage_organs.png", plot = plot_pt)


Evolution of the quantity of phage cocktail in feces
Quantity of Mt1B1 in the different organs
Quantity of phage cocktail in the different organs