#installing and loading basic packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)

##uploading xls file
ur_db <- read_excel("C:/Users/vella/Documents/Datasets/AWB Project/DaveList2022_Complete.xlsx")
glimpse(ur_db)

## data wrangling

#factorising vars
ur_db <- ur_db %>% mutate(
  Sex = as.factor(Sex),
  Locality = as.factor(Locality),
  Venue = as.factor(Venue),
  `1st Timer` = as.factor(`1st Timer`),
  `Deferred Donor` = as.factor(`Deferred Donor`),
  `Deferral Reason` = as.factor(`Deferral Reason`),
)
glimpse(ur_db)


#recode localities 
locality_to_postcode <- c(
  "ATTARD - MALTA" = "ATD",
  "BALZAN - MALTA" = "BZN",
  "BIRGU - MALTA" = "BRG",
  "BIRKIRKARA - MALTA" = "BKR",
  "BIRZEBBUGA - MALTA" = "BBG",
  "BORMLA - MALTA" = "BML",
  "DINGLI - MALTA" = "DGL",
  "FGURA - MALTA" = "FGR",
  "FLORIANA - MALTA" = "FLR",
  "FONTANA - GOZO" = "FNT",
  "GHAJNSIELEM - GOZO" = "GSM",
  "GHARB - GOZO" = "GRB",
  "GHARGHUR - MALTA" = "GHR",
  "GHASRI - GOZO" = "GSR",
  "GHAXAQ - MALTA" = "GXQ",
  "GUDJA - MALTA" = "GDJ",
  "GZIRA - MALTA" = "GZR", 
  "HAMRUN - MALTA" = "HMR",
  "IKLIN - MALTA" = "IKL",
  "ISLA - MALTA" = "ISL", 
  "KALKARA - MALTA" = "KKR",
  "KERCEM - GOZO" = "KCM",
  "KIRKOP - MALTA" = "KKP",
  "LIJA - MALTA" = "LJA",
  "LUQA - MALTA" = "LQA", 
  "MARSA - MALTA" = "MRS",
  "MARSALFORN - GOZO" = "MFN",
  "MARSASKALA - MALTA" = "MSK",
  "MARSAXLOKK - MALTA" = "MXK",
  "MDINA - MALTA" = "MDN",
  "MELLIEHA - MALTA" = "MLH",
  "MGARR - MALTA" = "MGR",
  "MOSTA - MALTA" = "MST",
  "MQABBA - MALTA" = "MQB",
  "MSIDA - MALTA" = "MSD",
  "MTARFA - MALTA" = "MTF",
  "MUNXAR - GOZO" = "MXR",
  "NADUR - GOZO" = "NDR",
  "NAXXAR - MALTA" = "NXR",
  "PAOLA - MALTA" = "PLA",
  "PEMBROKE - MALTA" = "PBK", 
  "PIETA' - MALTA" = "PTA",
  "QALA - GOZO" = "QLA",
  "QORMI - MALTA" = "QRM",
  "QRENDI - MALTA" = "QRD",
  "RABAT - GOZO" = "VCT",
  "RABAT - MALTA" = "RBT",
  "SAFI - MALTA" = "SFI",
  "SAN GILJAN - MALTA" = "STJ",
  "SAN GWANN - MALTA" = "SGN",
  "SAN LAWRENZ - GOZO" = "SLZ",
  "SAN PAWL IL-BAHAR - MALTA" = "SPB",
  "SANNAT - GOZO" = "SNT",
  "SANTA LUCIJA - MALTA" = "SLC",
  "SANTA VENERA - MALTA" = "SVR",
  "SIGGIEWI - MALTA" = "SGW",
  "SLIEMA - MALTA" = "SLM",
  "SWIEQI - MALTA" = "SWQ",
  "TA' XBIEX - MALTA" = "XBX",
  "TARXIEN - MALTA" = "TXN",
  "VALLETTA - MALTA" = "VLT",
  "XAGHRA - GOZO" = "XRA",
  "XEWKIJA - GOZO" = "XWK",
  "XGHAJRA - MALTA" = "XJR",
  "XLENDI - GOZO" = "XLN",
  "ZABBAR - MALTA" = "ZBR",
  "ZEBBUG - GOZO" = "ZBB",
  "ZEBBUG - MALTA" = "ZBG",
  "ZEJTUN - MALTA" = "ZJT",
  "ZURRIEQ - MALTA" = "ZRQ"
)


locality_to_district <- c(
  "ATTARD - MALTA" = "Western",
  "BALZAN - MALTA" = "Western",
  "BIRGU - MALTA" = "Southern Harbour",
  "BIRKIRKARA - MALTA" = "Northern Harbour",
  "BIRZEBBUGA - MALTA" = "South Eastern",
  "BORMLA - MALTA" = "Southern Harbour",
  "DINGLI - MALTA" = "Western",
  "FGURA - MALTA" = "Southern Harbour",
  "FLORIANA - MALTA" = "Southern Harbour",
  "FONTANA - GOZO" = "Gozo & Comino",
  "GHAJNSIELEM - GOZO" = "Gozo & Comino",
  "GHARB - GOZO" = "Gozo & Comino",
  "GHARGHUR - MALTA" = "Northern",
  "GHASRI - GOZO" = "Gozo & Comino",
  "GHAXAQ - MALTA" = "South Eastern",
  "GUDJA - MALTA" = "South Eastern",
  "GZIRA - MALTA" = "Northern Harbour", 
  "HAMRUN - MALTA" = "Northern Harbour",
  "IKLIN - MALTA" = "Western",
  "ISLA - MALTA" = "Southern Harbour", 
  "KALKARA - MALTA" = "Southern Harbour",
  "KERCEM - GOZO" = "Gozo & Comino",
  "KIRKOP - MALTA" = "South Eastern",
  "LIJA - MALTA" = "Western",
  "LUQA - MALTA" = "Southern Harbour", 
  "MARSA - MALTA" = "Southern Harbour",
  "MARSALFORN - GOZO" = "Gozo & Comino",
  "MARSASKALA - MALTA" = "South Eastern",
  "MARSAXLOKK - MALTA" = "South Eastern",
  "MDINA - MALTA" = "Western",
  "MELLIEHA - MALTA" = "Northern",
  "MGARR - MALTA" = "Northern",
  "MOSTA - MALTA" = "Northern",
  "MQABBA - MALTA" = "South Eastern",
  "MSIDA - MALTA" = "Northern Harbour",
  "MTARFA - MALTA" = "Western",
  "MUNXAR - GOZO" = "Gozo & Comino",
  "NADUR - GOZO" = "Gozo & Comino",
  "NAXXAR - MALTA" = "Northern",
  "PAOLA - MALTA" = "Southern Harbour",
  "PEMBROKE - MALTA" = "Northern Harbour", 
  "PIETA' - MALTA" = "Northern Harbour",
  "QALA - GOZO" = "Gozo & Comino",
  "QORMI - MALTA" = "Northern Harbour",
  "QRENDI - MALTA" = "South Eastern",
  "RABAT - GOZO" = "Gozo & Comino",
  "RABAT - MALTA" = "Northern",
  "SAFI - MALTA" = "South Eastern",
  "SAN GILJAN - MALTA" = "Northern Harbour",
  "SAN GWANN - MALTA" = "Northern Harbour",
  "SAN LAWRENZ - GOZO" = "Gozo & Comino",
  "SAN PAWL IL-BAHAR - MALTA" = "Northern",
  "SANNAT - GOZO" = "Gozo & Comino",
  "SANTA LUCIJA - MALTA" = "Southern Harbour",
  "SANTA VENERA - MALTA" = "Northern Harbour",
  "SIGGIEWI - MALTA" = "Western",
  "SLIEMA - MALTA" = "Northern Harbour",
  "SWIEQI - MALTA" = "Northern Harbour",
  "TA' XBIEX - MALTA" = "Northern Harbour",
  "TARXIEN - MALTA" = "Southern Harbour",
  "VALLETTA - MALTA" = "Southern Harbour",
  "XAGHRA - GOZO" = "Gozo & Comino",
  "XEWKIJA - GOZO" = "Gozo & Comino",
  "XGHAJRA - MALTA" = "Southern Harbour",
  "XLENDI - GOZO" = "Gozo & Comino",
  "ZABBAR - MALTA" = "Southern Harbour",
  "ZEBBUG - GOZO" = "Gozo & Comino",
  "ZEBBUG - MALTA" = "Western",
  "ZEJTUN - MALTA" = "South Eastern",
  "ZURRIEQ - MALTA" = "South Eastern"
)

#setting up current date
current_date <- Sys.Date()

#creating the target var
target_db <- ur_db %>% mutate(
  Donor_Status = case_when(
    difftime(current_date, `Last Visit`, units = "days") > 365 ~ "Donor Lost",
    `1st Timer` == "First Timer" & difftime(current_date, `Last Visit`, units = "days") <365 ~ "Donor Retained",
    `1st Timer` == "Regular" & `Donation Frequency` <= 6 & difftime(current_date, `Last Visit`, units = "days") <365 ~ "Donor Retained",
    TRUE ~ "Donor Lost"
  )
)

target_db <- target_db %>% mutate(Donor_Status =
                                    as.factor(Donor_Status)
)


#target_db <- target_db %>%
#  mutate(Locality = case_when(
#    Locality %in% names(locality_to_postcode) ~ locality_to_postcode[Locality],
#    TRUE ~ Locality  # Keep original if not in mapping
#  )) %>%
#  mutate(Locality = as.factor(Locality)) 


target_db <- target_db %>%
  mutate(Locality = case_when(
    Locality %in% names(locality_to_district) ~ locality_to_district[Locality],
    TRUE ~ Locality  # Keep original if not in mapping
  )) %>%
  mutate(Locality = as.factor(Locality)) 


str(target_db)  
glimpse(target_db)
View(target_db)

#################################################################################
##visualisation

summary(target_db)

# quick review of donor status

ggplot(target_db, aes(x = Donor_Status, fill = Donor_Status))+
  geom_bar()+
  labs(title = "Distribution of Donor_Status")

#  visualise locality
install.packages("treemap")
library(treemap)

# Summarize to get counts per Locality
locality_data <- target_db %>%
  group_by(Locality) %>%
  summarise(count = n())

treemap(locality_data, 
        index = "Locality", 
        vSize = "count", 
        title = "Treemap of Localities 2022 - 2023")

ggplot(locality_data, aes(x = count, y = reorder(Locality, count))) +
  geom_point() +
  labs(title = "Dot Plot of Locality", x = "Count", y = "Locality")

# Predominant localities per venue

library(purrr)

locality_venue_data <- target_db %>%
  group_by(Venue, Locality) %>%
  summarise(count = n()) %>%
  ungroup()


library(treemap)

locality_venue_data %>%
  group_split(Venue) %>%
  purrr::walk(function(data_subset) {
    ven <- unique(data_subset$Venue)
    treemap(data_subset,
            index = "Locality",
            vSize = "count",
            title = paste("Treemap of Localities for Venue", ven))
  })


# Box plot grid for categoricals

ggplot(target_db, aes(x = Donor_Status, y = Age, fill = Donor_Status)) + 
  geom_boxplot() +
  facet_wrap(~ Sex + `1st Timer` + Venue, scales = "free_x") +
  scale_y_continuous(limits = c(16, 70)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Age Distribution by Donor Status, Sex, 1st Timer Status, and Venue")



ggplot(target_db, aes(x = Donor_Status, y = `Donation Frequency`, fill = Donor_Status)) + 
  geom_boxplot() +
  facet_wrap(~ Sex + `1st Timer` + Venue, scales = "free_x") +
  scale_y_discrete(limits = factor(0:10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Donation Frequency by Donor Status, Sex, 1st Timer Status, and Venue")

ggplot(target_db, aes(x = Donor_Status, y = `Total Donation Attempts.`, fill = Donor_Status)) + 
  geom_boxplot() +
  facet_wrap(~ Sex + `1st Timer` + Venue, scales = "free_x") +
  scale_y_continuous(limits = c(0,157))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Donation Attempt by Donor Status, Sex, 1st Timer Status, and Venue")


# Violin plot grid for categoricals 

ggplot(target_db, aes(x = Donor_Status, y = Age, fill = Donor_Status)) + 
  geom_violin() +
  facet_wrap(~ Sex + `1st Timer` + Venue, scales = "free_x") +
  scale_y_continuous(limits = c(16, 70)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Age Distribution by Donor Status, Sex, 1st Timer Status, and Venue")



ggplot(target_db, aes(x = Donor_Status, y = `Donation Frequency`, fill = Donor_Status)) + 
  geom_violin() +
  facet_wrap(~ Sex + `1st Timer` + Venue, scales = "free_x") +
  scale_y_discrete(limits = factor(0:10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Donation Frequency by Donor Status, Sex, 1st Timer Status, and Venue")

ggplot(target_db, aes(x = Donor_Status, y = `Total Donation Attempts.`, fill = Donor_Status)) + 
  geom_violin() +
  facet_wrap(~ Sex + `1st Timer` + Venue, scales = "free_x") +
  scale_y_continuous(limits = c(0,157))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Donation Attempt by Donor Status, Sex, 1st Timer Status, and Venue")


# heatmap for continuous 

library(reshape2)

# Calculate correlation matrix for numeric variables
corr_matrix <- cor(target_db %>% select_if(is.numeric))
glimpse(corr_matrix)

# Plot Heatmap
ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Numeric Variables", x = "", y = "")


library(plotly)

# Interactive scatter plot for Age vs. Donation Frequency
plot_ly(target_db, x = ~Age, y = ~`Donation Frequency`, color = ~ Donor_Status, type = 'scatter', mode = 'markers') %>%
  layout(title = "Interactive Scatter Plot of Age vs. Donation Frequency")




## k-means clustering on training

# preparation of training db

target_numeric <- target_db %>%
  select(-`Donor No.`) %>% # Drop the 'Donor No.' column
  select_if(is.numeric) %>%
  scale()


target_numeric <- as.data.frame(target_numeric)
glimpse(target_numeric)

# Ensure dataframe

target_numeric <- as.data.frame(target_numeric)


# elbow method to determine no of clusters
install.packages("factoextra")
library(factoextra)

fviz_nbclust(target_numeric, kmeans, method = "wss")

set.seed(123)
wss <- (nrow(target_numeric)-1)* sum(apply(target_numeric,2, var))
for( i in 2:15) wss[i] <- sum(kmeans(target_numeric, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


### 4 clusters optimum ###



# Perform K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(target_numeric, centers = 4, iter.max = 100)

# Add cluster result to dataframe
target_db$Cluster <- as.factor(kmeans_result$cluster)


glimpse(target_numeric)
# Visualize Clusters
ggplot(target_db, aes(x = Cluster, y = Age , color = Donor_Status))+
  geom_point()+
  labs(title = "K-Means Clustering of Donors")+
  theme_minimal()


install.packages("factoextra")
library(factoextra)

fviz_cluster(kmeans_result, data = target_numeric)

##Hierarchical Clustering:

# Calculate the distance matrix
dist_matrix <- dist(target_numeric)

# Apply hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", cex = 0.9)


#MCA

# Install and load the ca package
install.packages("ca")
library(ca)

# Install and load the FactoMineR package
install.packages("FactoMineR")
library(FactoMineR)

colnames(target_db)


mca_result <- MCA(target_db[, c("Sex", "Locality", "Venue", "1st Timer",  "Deferred Donor", "Deferral Reason", "Donor_Status" )])

# Print the results
print(summary(mca_result))

# Plot the results
plot(mca_result, main = "MCA Plot")



#rtsne

install.packages("Rtsne")
library(Rtsne)
library(ggplot2)

# Prepare numeric data
training_rtsne <- target_db %>%
  #select(-`Donor No.`)%>%
  select_if(is.numeric) %>% 
  scale()  # Standardizing the data



set.seed(123)  # For reproducibility
tsne_result <- Rtsne(training_rtsne, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

tsne_data <- as.data.frame(tsne_result$Y)
tsne_data$Cluster <- as.factor(target_db$Cluster) # Add cluster information
tsne_data$Donor_Status <-target_db$Donor_Status
glimpse(tsne_data)

ggplot(tsne_data, aes(x = V1, y = V2, color = Donor_Status)) +
  geom_point() +
  labs(title = "t-SNE Visualization of Training Data", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal()

#PCA

install.packages("factoextra")
library(factoextra)

# Prepare numeric data
training_pca <- target_db %>%
  select(-`Donor No.`) %>%
  select_if(is.numeric) %>%
  scale()  # Standardizing the data

pca_result <- prcomp(training_pca, scale = TRUE)

# Scree Plot to visualize explained variance
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Biplot to visualize PCA components
fviz_pca_biplot(pca_result, 
                geom.ind = "point", 
                pointshape = 21, 
                pointsize = 2,
                fill.ind = as.factor(target_db$Donor_Status), 
                col.ind = "black",
                palette = "jco",
                addEllipses = TRUE, 
                label = "var",
                col.var = "red",
                repel = TRUE)




# Facet Grid Plot by Clusters
ggplot(target_db, aes(x = Sex, fill = Donor_Status)) +
  geom_bar() +
  facet_wrap(~ Cluster) +
  labs(title = "Donor Status Distribution across Clusters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()

#Association Mining Rules

install.packages("arules",
                 repos = c("https://mhahsler.r-universe.dev",
                           "https://cloud.r-project.org/"))

library(arules)

# Convert relevant columns to factors
training_arules <- target_db %>%
  mutate(across(everything(), as.factor))

# Convert to transactions
transactions <- as(training_arules, "transactions")

rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.8))

inspect(sort(rules, by = "lift")[1:10])


install.packages("arulesViz")
library(arulesViz)

plot(rules, method = "graph", engine = "htmlwidget")

######################################################################

## Random Forest Section

#Installation & loading
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)




target_db <- target_db %>%
  mutate(
    Age = as.numeric(Age),
    `Donation Frequency` = as.numeric(`Donation Frequency`),
    `Total Donation Attempts.` = as.numeric(`Total Donation Attempts.`)
    #First_Visit_Year = as.numeric(format(`First Visit`, "%Y")),
    #First_Visit_Month = as.numeric(format(`First Visit`, "%m")),
    #Last_Visit_Year = as.numeric(format(`Last Visit`, "%Y")),
    #Last_Visit_Month = as.numeric(format(`Last Visit`, "%m")),
    #Days_Since_First_Visit = as.numeric(difftime(Sys.Date(), `First Visit`, units = "days")),
    #Days_Since_Last_Visit = as.numeric(difftime(Sys.Date(), `Last Visit`, units = "days"))
  ) %>%
  select(-`Donor No.`, -`First Visit`, -`Last Visit`, -`Date of Birth`) %>%
  mutate(across(where(is.factor), as.factor))

str(target_db)


names(target_db) <- make.names(names(target_db))


# Splitting target_db
trainIndex <- createDataPartition(target_db$Donor_Status, p = .75, list = FALSE, times = 1)
trainingData <- target_db[trainIndex,]
testingData  <- target_db[-trainIndex,]


# Train the Random Forest Model
set.seed(123)
rf_model <- randomForest(Donor_Status ~ ., data = trainingData, ntree = 500, mtry = 3, importance = TRUE)

# Generate Predictions for Training Data
rf_train_predictions <- predict(rf_model, newdata = trainingData)

# Create Confusion Matrix for Training Data
conf_matrix_train <- confusionMatrix(rf_train_predictions, trainingData$Donor_Status)

# Print the Confusion Matrix
print(conf_matrix_train)



# Evaluation 
rf_predictions <- predict(rf_model, newdata = testingData)
confusionMatrix(rf_predictions, testingData$Donor_Status)

# Define the name of the log file
log_file <- "urtrainandtest.txt"

# Start saving console output to the log file
sink(log_file, append = TRUE, type = "output")

# Your R code here
# Example: print a summary of your model
print(conf_matrix_train)
confusionMatrix(rf_predictions, testingData$Donor_Status)

# Stop saving console output
sink(NULL)


varImpPlot(rf_model)

##Tree visualisation
install.packages("randomForestExplainer")
library(randomForestExplainer)

# Extract and plot a single tree
tree <- getTree(rf_model, k = 1, labelVar = TRUE)
plot(tree)

## Variable Importance Plot

varImpPlot(rf_model, n.var = 10, main = "Ranking of Variables")


## Visualisation of confusion matrix

conf_matrix <- confusionMatrix(rf_predictions, testingData$Donor_Status)


conf_matrix_df <- as.data.frame(as.table(conf_matrix$table))

ggplot(conf_matrix_df, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted")

#Hyperparameter 

library(caret)

# Set up train control
control <- trainControl(method = "cv", number = 5, search = "grid")

tuneGrid <- expand.grid(
  .mtry = c(2, 3, 4, 5),
  .splitrule = "gini",
  .min.node.size = c(1, 5, 10)
)

set.seed(123)
rf_model_tuned <- train(
  Donor_Status ~ ., 
  data = trainingData, 
  method = "ranger", 
  trControl = control, 
  tuneGrid = tuneGrid,
  importance = 'impurity'
)

print(rf_model_tuned)
best_model <- rf_model_tuned$finalModel
rf_predictions_tuned <- predict(rf_model_tuned, newdata = testingData)
confusionMatrix(rf_predictions_tuned, testingData$Donor_Status)

#ROSE

install.packages("ROSE")
library(ROSE)


# Determine current sample size and set N accordingly
current_size <- nrow(trainingData)
desired_size <- current_size * 1.5  # For example, 150% of the current size

# Apply ROSE
trainingData_rose <- ovun.sample(Donor_Status ~ ., data = trainingData, method = "over", N = desired_size)$data

# Check the new class distribution
table(trainingData_rose$Donor_Status)

# Retrain Random Forest Model with ROSE data
set.seed(123)
rf_model_rose <- randomForest(Donor_Status ~ ., data = trainingData_rose, ntree = 500, mtry = 3, importance = TRUE)

# Evaluate the ROSE Model
rf_predictions_rosetrain <- predict(rf_model_rose, newdata = trainingData)
conf_matrix_rosetrain <- confusionMatrix(rf_predictions_rosetrain, trainingData$Donor_Status)

rf_predictions_rose <- predict(rf_model_rose, newdata = testingData)
conf_matrix_rose <- confusionMatrix(rf_predictions_rose, testingData$Donor_Status)

# Print the Confusion Matrix
print(conf_matrix_rose)
print(conf_matrix_rosetrain)

# Define the name of the log file
log_file <- "rose.txt"

# Start saving console output to the log file
sink(log_file, append = TRUE, type = "output")

# Your R code here
# Example: print a summary of your model
print(conf_matrix_rosetrain)
print(conf_matrix_rose)

# Stop saving console output
sink(NULL)



#Gradient Boosting
install.packages("gbm")
library(gbm)

control <- trainControl(method = "cv", number = 5, search = "grid")

tuneGrid <- expand.grid(
  .n.trees = c(50, 100, 150),
  .interaction.depth = c(1, 3, 5),
  .shrinkage = c(0.01, 0.1),
  .n.minobsinnode = c(10, 20)
)

set.seed(123)
gbm_model <- train(
  Donor_Status ~ ., 
  data = trainingData, 
  method = "gbm", 
  trControl = control, 
  tuneGrid = tuneGrid, 
  verbose = FALSE
)

gbm_predictions <- predict(gbm_model, newdata = testingData)
confusionMatrix(gbm_predictions, testingData$Donor_Status)

#######################
##pre-split


# Load necessary library
library(dplyr)
# Assume your dataset is called 'data' and your target column is 'target'
# Split the data by target class
class0 <- target_db %>% filter(Donor_Status == "Donor Lost")
class1 <- target_db %>% filter(Donor_Status == "Donor Retained")
# Calculate the minimum count of samples between the two classes
n_min <- min(nrow(class0), nrow(class1))
# Balance each class separately
class0_balanced <- class0 %>% sample_n(n_min)
class1_balanced <- class1 %>% sample_n(n_min)
# Combine the balanced classes
data_balanced <- bind_rows(class0_balanced, class1_balanced)
# Shuffle the balanced dataset
set.seed(123)  # for reproducibility
data_balanced <- data_balanced %>% sample_frac(1)
# Split your data like you already did in train and test, but now based on data_balanced


#Random Forest on data_balanced

data_balanced <- data_balanced %>%
  mutate(
    Age = as.numeric(Age),
    `Donation Frequency` = as.numeric(`Donation Frequency`),
    `Total Donation Attempts.` = as.numeric(`Total Donation Attempts.`)
    #First_Visit_Year = as.numeric(format(`First Visit`, "%Y")),
    #First_Visit_Month = as.numeric(format(`First Visit`, "%m")),
    #Last_Visit_Year = as.numeric(format(`Last Visit`, "%Y")),
    #Last_Visit_Month = as.numeric(format(`Last Visit`, "%m")),
    #Days_Since_First_Visit = as.numeric(difftime(Sys.Date(), `First Visit`, units = "days")),
    #Days_Since_Last_Visit = as.numeric(difftime(Sys.Date(), `Last Visit`, units = "days"))
  ) %>%
  select(-`Donor No.`, -`First Visit`, -`Last Visit`, -`Date of Birth`) %>%
  mutate(across(where(is.factor), as.factor))




names(data_balanced) <- make.names(names(data_balanced))


# Splitting target_db
trainIndexbalanced <- createDataPartition(data_balanced$Donor_Status, p = .75, list = FALSE, times = 1)
trainingDatabalanced <- data_balanced[trainIndexbalanced,]
testingDatabalanced  <- data_balanced[-trainIndexbalanced,]


# Train the Random Forest Model
set.seed(123)
rf_model <- randomForest(Donor_Status ~ ., data = trainingDatabalanced, ntree = 500, mtry = 3, importance = TRUE)

# Generate Predictions for Training Data
rf_train_predictions <- predict(rf_model, newdata = trainingDatabalanced)

# Create Confusion Matrix for Training Data
conf_matrix_train <- confusionMatrix(rf_train_predictions, trainingDatabalanced$Donor_Status)

# Print the Confusion Matrix
print(conf_matrix_train)



# Evaluation 
rf_predictions <- predict(rf_model, newdata = testingDatabalanced)
confusionMatrix(rf_predictions, testingDatabalanced$Donor_Status)



#Ensemble

library(caret)

# Set up train control for cross-validation
control <- trainControl(method = "cv", number = 5, savePredictions = "final", classProbs = TRUE)

# Train Random Forest
# Convert factor levels to valid variable names
trainingDatabalanced$Donor_Status <- factor(make.names(trainingDatabalanced$Donor_Status))
testingDatabalanced$Donor_Status <- factor(make.names(testingDatabalanced$Donor_Status))

set.seed(123)
rf_model <- train(Donor_Status ~ ., data = trainingDatabalanced, method = "rf", trControl = control)

# Train Gradient Boosting Machine
set.seed(123)
gbm_model <- train(Donor_Status ~ ., data = trainingDatabalanced, method = "gbm", trControl = control, verbose = FALSE)

# Train XGBoost
set.seed(123)
xgb_model <- train(Donor_Status ~ ., data = trainingDatabalanced, method = "xgbTree", trControl = control)

# Create a dataframe of predictions from each model
predictions <- data.frame(
  rf = predict(rf_model, testingDatabalanced, type = "prob")[,2],
  gbm = predict(gbm_model, testingDatabalanced, type = "prob")[,2],
  xgb = predict(xgb_model, testingDatabalanced, type = "prob")[,2],
  Donor_Status = testingDatabalanced$Donor_Status
)

# Train a meta-model (logistic regression) using these predictions
meta_model <- train(Donor_Status ~ ., data = predictions, method = "glm", family = "binomial")

# Make predictions with the meta-model
ensemble_predictions <- predict(meta_model, newdata = predictions)

# Evaluate the ensemble model
ensemble_conf_matrix <- confusionMatrix(ensemble_predictions, predictions$Donor_Status)
print(ensemble_conf_matrix)

# Define the name of the log file
log_file <- "ensemble.txt"

# Start saving console output to the log file
sink(log_file, append = TRUE, type = "output")

# Your R code here
# Example: print a summary of your model
print(ensemble_conf_matrix)


# Stop saving console output
sink(NULL)



#hyperparameter tuning
library(gbm)

install.packages("xgboost")
library(xgboost)



control <- trainControl(method = "cv", number = 10, search = "grid")

tuneGrid <- expand.grid(
  .n.trees = seq(50, 200, by = 50),          # Number of boosting iterations
  .interaction.depth = seq(1, 10, by = 2),   # Max tree depth
  .shrinkage = c(0.01, 0.05, 0.1, 0.2),      # Learning rate
  .n.minobsinnode = c(5, 10, 15, 20)         # Min. observations in terminal node
)

set.seed(123)
gbm_model <- train(
  Donor_Status ~ ., 
  data = trainingDatabalanced, 
  method = "gbm", 
  trControl = control, 
  tuneGrid = tuneGrid, 
  verbose = FALSE
)

gbm_predictions <- predict(gbm_model, newdata = testingDatabalanced)
confusionMatrix(gbm_predictions, testingDatabalanced$Donor_Status)

train_matrix <- xgb.DMatrix(data = as.matrix(trainingDatabalanced[, -which(names(trainingDatabalanced) == "Donor_Status")]), label = as.numeric(trainingDatabalanced$Donor_Status) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(testingDatabalanced[, -which(names(testingDatabalanced) == "Donor_Status")]), label = as.numeric(testingDatabalanced$Donor_Status) - 1)

grid <- expand.grid(
  nrounds = seq(50, 200, by = 50),
  eta = c(0.01, 0.05, 0.1, 0.2),
  max_depth = seq(3, 10, by = 2),
  gamma = c(0, 1, 3, 5),
  colsample_bytree = c(0.5, 0.7, 0.9, 1),
  min_child_weight = c(1, 3, 5, 7)
)

control <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
xgb_model <- train(
  x = train_matrix,
  y = trainingDatabalanced$Donor_Status,
  method = "xgbTree",
  trControl = control,
  tuneGrid = grid
)

xgb_predictions <- predict(xgb_model, newdata = test_matrix)
xgb_confusion_matrix <- confusionMatrix(factor(xgb_predictions, levels = c(0, 1)), testingDatabalanced$Donor_Status)

# Print the Confusion Matrix
print(xgb_confusion_matrix)

# Convert all character columns to factors, then to numeric
trainingData_numeric <- trainingDatabalanced %>% mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))
testingData_numeric <- testingDatabalanced %>% mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

train_matrix <- xgb.DMatrix(data = as.matrix(trainingData_numeric[, -which(names(trainingData_numeric) == "Donor_Status")]), label = as.numeric(trainingData_numeric$Donor_Status) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(testingData_numeric[, -which(names(testingData_numeric) == "Donor_Status")]), label = as.numeric(testingData_numeric$Donor_Status) - 1)

grid <- expand.grid(
  nrounds = seq(50, 200, by = 50),
  eta = c(0.01, 0.05, 0.1, 0.2),
  max_depth = seq(3, 10, by = 2),
  gamma = c(0, 1, 3, 5),
  colsample_bytree = c(0.5, 0.7, 0.9, 1),
  min_child_weight = c(1, 3, 5, 7)
)

control <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(123)
xgb_model <- train(
  x = train_matrix,
  y = trainingData_numeric$Donor_Status,
  method = "xgbTree",
  trControl = control,
  tuneGrid = grid
)

xgb_predictions <- predict(xgb_model, newdata = test_matrix)
xgb_confusion_matrix <- confusionMatrix(factor(xgb_predictions, levels = c(0, 1)), testingData_numeric$Donor_Status)

# Print the Confusion Matrix
print(xgb_confusion_matrix)

###############################################################################################
install.packages("DiagrammeR")
install.packages("DiagrammeRsvg")

library(DiagrammeR)
library(DiagrammeRsvg)

grViz("

digraph algorithm_flowchart {

  # Add a title
  labelloc = 't'
  label = 'Target Variable: Donor Status:  Donor Lost/ Donor Retained'
  fontsize = 20
  
  # Define node styles
  node [shape = box, style = filled, color = lightblue]
  
  # Define nodes
  Lastdon1       [label = 'Time since last donation > 12 months?', shape = diamond]
  Firsttimer [label = '1st Timer?', shape = diamond]
  Avginterval  [label = 'Regular average no of intervals < 6?', shape = diamond]
  Lastdon2    [label = 'Time since last donation < 6 months?', shape = diamond]
  Retained    [label = 'Donor Retained']
  Lost        [label = 'Donor Lost'] 
  
  # Define edges
  Lastdon1 -> Lost[label = 'True']
  Lastdon1 -> Firsttimer[label = 'False']
  Firsttimer -> Avginterval[label = 'False']
  Firsttimer -> Lastdon2[label = 'True']
  Avginterval -> Lastdon2[label = 'False']
  Lastdon2 -> Retained[label ='True']
  Lastdon2 -> Lost[label = 'False']
}
")

