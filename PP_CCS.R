# Load appropriate libraries for the project. 
library(Matrix)
library(arules)
library(arulesViz)
library(tidyverse)
# Create the pp.ccs data frame by reading the comma separated values file.
pp.ccs <- read.csv("CatalogCrossSell.csv", header = TRUE)
# Display the first 6 rows of records and all columns in the data frame.
head(pp.ccs)
# Display the structure of the data frame which provide the number of variables and rows as well
# as the variable types.
str(pp.ccs)
# Convert data frame to a matrix format, and remove columns one and four.
pp.ccs <- as.matrix(pp.ccs[,-c(1,4)])
# Convert variable types to transaction data.
pp.ccs.trans <- as(pp.ccs,"transactions")
# List column (variable) names to verify integrity.
colnames(pp.ccs.trans)
# Display summary information to verify itemMatrix in sparse format
# Also displays most frequent items used.
summary(pp.ccs.trans)
# Run structure of data once again to verify data is transaction data.
str(pp.ccs.trans)
# Run association rules model Apriori
pprules <- apriori(pp.ccs.trans,parameter = list(supp = 0.05, conf = 0.5,target = "rules"))
# Limit significant digits to three with options function.
options(digits = 3)
# List the top five rules identified from the model by descending lift.
inspect(head(sort(pprules, by = "lift"), n = 5))
plot(pprules, jitter = 0, main = "Scatterplot of Created Rules")
plot(pprules, method = "graph")
plot(pprules, method = "paracoord", control = list(reorder = TRUE))
plot(pprules, method = "matrix")
# Use k parameter to limit number of antecedent groups.
plot(pprules, method = "grouped", k = 10)
