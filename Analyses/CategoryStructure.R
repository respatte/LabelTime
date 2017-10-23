# LIBRARY IMPORTS
library(ggplot2)
library(Hmisc)
library(plyr)

# DATA HANDLING
cat <- read.csv("../Results/CategoryStructure.csv", head=TRUE)

# COMPUTE PCA
cat.pca <- prcomp(cat, retx=T)
# Replace previous values with PCA values
cat[,2:11] <- cat.pca$x

# PLOT
cat.plot <- ggplot(data=cat, aes(x=dim0, y=dim1, colour=factor(category), shape=factor(category))) +
  xlab("PCA1 - 83.59% of variance explained") +
  ylab("PCA2 - 5.955% of variance explained") +
  theme_bw(base_size = 10) +
  scale_colour_brewer(palette="Dark2", guide=F) +
  scale_shape_manual(values=c(16,18), guide=F) +
  geom_point(size=6)
ggsave("../Results/CategoryStructure.pdf", plot=cat.plot,
       width=3.5, height=3)
