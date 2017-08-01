# LIBRARY IMPORTS
library(ggplot2)

# DATA HANDLING
# Import data, from both Category and SingleObject
h_rep.LTM <- read.csv("../Results/Category_hidden_LTM.csv", head=TRUE)
h_rep.STM <- read.csv("../Results/Category_hidden_STM.csv", head=TRUE)
# Add empty pca columns on the left
h_rep.LTM <- cbind(pca1=0, pca2=0, h_rep.LTM)
h_rep.STM <- cbind(pca1=0, pca2=0, h_rep.STM)
d <- data.frame(subject=numeric(480), theory=0, step=0,
				dist_type=0, m_LTM=0, sd_LTM=0, m_STM=0, sd_STM=0)

# Loop over subject and step
for (subject in 0:160){
	for (step in
		 levels(factor(h_rep.LTM$step[h_rep.LTM$subject==subject]))){
		print(c(subject, step))
		# PRINCIPAL COMPONENT ANALYSIS
		# Extract first two dimensions after PCA
		# First LTM
		index <- h_rep.LTM$subject==subject & h_rep.LTM$step==step
		pca.LTM <- prcomp(h_rep.LTM[index, -(1:7)], retx=T)$x[, 1:2]
		h_rep.LTM$pca1[index] <- pca.LTM[, 1]
		h_rep.LTM$pca2[index] <- pca.LTM[, 2]
		# Then STM
		index <- h_rep.STM$subject==subject & h_rep.STM$step==step
		pca.STM <- prcomp(h_rep.STM[index, -(1:7)], retx=T)$x[, 1:2]
		h_rep.STM$pca1[index] <- pca.STM[, 1]
		h_rep.STM$pca2[index] <- pca.STM[, 2]
		# WITHIN CATEGORY MEAN DISTANCE
		dl.LTM <- c()  # Labelled category (coded 0 in df)
		dl.STM <- c()  # Labelled category (coded 0 in df)
		dnl.LTM <- c() # Unlabelled category (coded 1 in df)
		dnl.STM <- c() # Unlabelled category (coded 1 in df)
		# Create index for category
		index.l <- index & h_rep.LTM$category==0
		index.nl <- index & h_rep.LTM$category==1
		for (e1 in 0:2){
			# Create index for first exemplar
			index1.l <- index.l & h_rep.LTM$exemplar==e1
			index1.nl <- index.nl & h_rep.LTM$exemplar==e1
			for (e2 in e1+1:3){
				# Create index for second exemplar
				index2.l <- index.l & h_rep.LTM$exemplar==e2
				index2.nl <- index.nl & h_rep.LTM$exemplar==e2
				# Append distances
				dl.LTM <- c(dl.LTM,
							dist(rbind(c(h_rep.LTM$pca1[index1.l],
										 h_rep.LTM$pca1[index2.l]),
									   c(h_rep.LTM$pca2[index1.l],
										 h_rep.LTM$pca2[index2.l])))[1])
				dl.STM <- c(dl.STM,
							dist(rbind(c(h_rep.STM$pca1[index1.l],
										 h_rep.STM$pca1[index2.l]),
									   c(h_rep.STM$pca2[index1.l],
										 h_rep.STM$pca2[index2.l])))[1])
				dnl.LTM <- c(dl.LTM,
							 dist(rbind(c(h_rep.LTM$pca1[index1.nl],
										  h_rep.LTM$pca1[index2.nl]),
										c(h_rep.LTM$pca2[index1.nl],
										  h_rep.LTM$pca2[index2.nl])))[1])
				dnl.STM <- c(dl.STM,
							 dist(rbind(c(h_rep.STM$pca1[index1.nl],
										  h_rep.STM$pca1[index2.nl]),
										c(h_rep.STM$pca2[index1.nl],
										  h_rep.STM$pca2[index2.nl])))[1])
			}
		}
		# Save mean and sd of distance for labelled category with all info
		d[3*subject,] <- c(h_rep.LTM[index1.l, 3:5], "labelled",
						   mean(dl.LTM), sd(dl.LTM),
						   mean(dl.STM), sd(dl.STM))
		# Save mean and sd of distance for unlabelled category with all info
		d[3*subject + 1,] <- c(h_rep.LTM[index1.nl, 3:5], "unlabelled",
							   mean(dnl.LTM), sd(dnl.LTM),
							   mean(dnl.STM), sd(dnl.STM))
		# Compute centre ("prototype") of each category
		proto_l.LTM <- c(mean(h_rep.LTM$pca1[index.l]),
						 mean(h_rep.LTM$pca2[index.l]))
		proto_l.STM <- c(mean(h_rep.STM$pca1[index.l]),
						 mean(h_rep.STM$pca2[index.l]))
		proto_nl.LTM <- c(mean(h_rep.LTM$pca1[index.nl]),
						  mean(h_rep.LTM$pca2[index.nl]))
		proto_nl.STM <- c(mean(h_rep.STM$pca1[index.nl]),
						  mean(h_rep.STM$pca2[index.nl]))
		# Save distances between categories (sd=0 here)
		d[3*subject + 2,] <- c(h_rep.LTM[index1.nl, 3:5], "between",
							   dist(rbind(proto_l.LTM, proto_nl.LTM))[1], 0,
							   dist(rbind(proto_l.STM, proto_nl.STM))[1], 0)
	}
}

write.csv(d, filename="../Results/Category_hidden_distances.csv", row.names=F)
