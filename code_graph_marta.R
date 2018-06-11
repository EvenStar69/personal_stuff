# works best with 1 csv per table, and table is like :
# Sample   Day1 Day2 Day3 Day4... (or organs instead or days or whatever variable)
# NM       5e6  1e6  ...
# OG
# ...

counts <- read.csv("my_path/my_data.csv", dec = ".", sep = ";", header = FALSE) # get data

library(ggplot2) # for beautiful graphs
library(reshape2) # for the "melt" function to prepare data for ggplots
library(scales) # for log scales 

counts_melt <- melt(counts_bf, id.var = "Sample") # Sample = name of the column with the names of the Samples (e.g. NM, OG, ...)
# getting a warning after the melt is normal
counts_melt$value <- as.numeric(counts_bfm$value) # make value column numeric (in case it's not in the good format)

plot_counts <- ggplot(data=counts_melt, aes(x=variable, y=value, color = Sample, group = Sample)) + geom_point(size = 2.5) + scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Evolution of the quantity of Mt1B1 in feces") + xlab("Time") + ylab("Mt1b1 (CFU/g of feces)") + expand_limits(y = c(10^3, 10^8))
# feel free to change title (ggtitle) and name of the axes (xlab, ylab) on the second line of ggplot (or the y-axis limits (with expand_limits))

ggsave(filename = "name_plot.png", plot= plot_counts) # save the plot in current folder
