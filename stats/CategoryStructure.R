# LIBRARY IMPORTS
library(ggplot2)
library(Hmisc)
library(plyr)

# DATA HANDLING
cat <- read.csv("../results/CategoryStructure.csv", head=TRUE)
cat$category <- factor(cat$category)
cat$cat_phase <- factor(paste(cat$phase,cat$category))

# COMPUTE PCA
cat.pca <- prcomp(cat[,3:12], retx=T)
# Replace previous values with PCA values
cat[,3:12] <- cat.pca$x

# PLOT
cat.plot <- ggplot(data=cat, aes(x=dim0, y=dim1, colour=category, shape=cat_phase)) +
  xlab("PCA1 - 83.59% of variance explained") +
  ylab("PCA2 - 5.955% of \n variance explained") +
  coord_cartesian(ylim = c(-.625,1.025)) +
  theme_bw(base_size = 10, base_family = "serif") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  scale_colour_brewer(palette="Dark2", guide=F) +
  scale_shape_manual(values=c(0,2,15,17), guide=F) +
  geom_point(size=2.5)
ggsave("../results/CategoryStructure.tiff", plot=cat.plot,
       width=3.5, height=1.5, dpi = 600)
