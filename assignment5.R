knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)

setwd("doktori/statistics in R/beadandok/")
dataset <- read.csv("data/assignment_5_dataset.csv")

str(dataset)
head(dataset)
summary(dataset)
str(dataset)
head(dataset)
summary(dataset)
library(psych)
library(moments)
cor_matrix <- cor(dataset)
KMO <- KMO(cor_matrix)
KMO
skewness_values <- skewness(dataset)
kurtosis_values <- kurtosis(dataset)

# Print the results
cat("Skewness values:\n")
print(skewness_values)

cat("Kurtosis values:\n")
print(kurtosis_values)
efa_result <- fa(dataset, nfactors = 3, rotate = "varimax")
print(efa_result$loadings, digits = 2)
# Extract communality scores
communality_scores <- efa_result$communality

if (length(communality_scores) == 0) {
  print("No communality scores available.")
} else {
  # Create a data frame with variable names and communality scores
  communality_df <- data.frame(variable = names(communality_scores), communality = communality_scores)
  
  # Sort the data frame by communality scores in decreasing order
  communality_df <- communality_df[order(-communality_df$communality), ]
  
  # Print the sorted communality scores
  print(communality_df)
}
  
  if (length(communality_scores) == 0) {
    print("No communality scores available.")
  } else {
    # Calculate the mean communality score
    mean_communality <- mean(communality_scores)
    
    # Print the mean communality score
    print(paste("Mean Communality Score: ", round(mean_communality, 4)))
  }
factor_loadings <- efa_result$loadings

# Print factor loadings
print(factor_loadings)
ggplot(factor_loadings) + barplot(abs(factor_loadings), beside = TRUE, col = "skyblue", main = "Factor Loadings", 
        xlab = "Items", ylab = "Loading", names.arg = colnames(factor_loadings), 
        ylim = c(0, 1), legend.text = rownames(factor_loadings), args.legend = list(x = "topright", bty = "n"))

# Add factor names to the plot
text(1:ncol(factor_loadings), par("usr")[3] - 0.1, labels = rownames(factor_loadings), srt = 45, adj = c(1, 1), xpd = TRUE)

# Add a legend
legend("topright", legend = colnames(factor_loadings), fill = "skyblue")


plot(efa_result$values, type = "b", main = "Scree Plot")
liberal_variable <- dataset$liberal
factor_scores <- as.data.frame(efa_result$scores)
write.csv(factor_scores, "factor_scores.csv", row.names = FALSE)
str(factor_scores)
model <- glm(liberal_variable ~ MR1 + MR2 + MR3, data = factor_scores)
summary(model)



