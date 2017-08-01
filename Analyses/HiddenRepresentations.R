# LIBRARY IMPORTS
library(ggplot2)

# DATA HANDLING
# Import data, from both Category and SingleObject
hidden_rep.LTM <- read.csv("../Results/Category_hidden_LTM.csv", head=TRUE)
hidden_rep.STM <- read.csv("../Results/Category_hidden_STM.csv", head=TRUE)

# PRINCIPAL COMPONENT ANALYSIS
# Add empty pca columns on the left
hidden_rep.LTM <- cbind(pca1=0, pca2=0, hidden_rep.LTM)
hidden_rep.STM <- cbind(pca1=0, pca2=0, hidden_rep.STM)
# Extract first two dimensions after PCA, for each subject, for each step
for (subject in 0:160){
	for (step in
		 levels(factor(hidden_rep.LTM$step[hidden_rep.LTM$subject==subject]))){
		print(c(subject, step))
		# First LTM
		index <- hidden_rep.LTM$subject==subject & hidden_rep.LTM$step==step
		pca.LTM <- prcomp(hidden_rep.LTM[index, -(1:7)], retx=T)$x[, 1:2]
		hidden_rep.LTM$pca1[index] <- pca.LTM[, 1]
		hidden_rep.LTM$pca2[index] <- pca.LTM[, 2]
		# Then STM
		index <- hidden_rep.STM$subject==subject & hidden_rep.STM$step==step
		pca.STM <- prcomp(hidden_rep.STM[index, -(1:7)], retx=T)$x[, 1:2]
		hidden_rep.STM$pca1[index] <- pca.STM[, 1]
		hidden_rep.STM$pca2[index] <- pca.STM[, 2]
	}
}
