#TRAVEL REVIEW ANALYSIS FOR DESTINATION PROFILING AND RATING BASED CLUSTERING
#DATASCIENCE FINAL PROJECT
#DATASET USED : TRAVEL REVIEW RATINGS

library(dplyr)
library(tidyverse)

#GET DIRECTORY
getwd()

# READ THE CSV
Ratings <- read.csv('./tarvel+review+ratings/google_review_ratings.csv')

#PRINT FIRST 10 ROWS
print(head(Ratings , 10))

# MAP THE REAL VARIABLES WITH THE CATEGORY
real_variables <- c(
  "churches", "resorts", "beaches", "parks", "theatres",
  "museums", "malls", "zoos", "restaurants", "pubs/bars",
  "local services", "burger/pizza shops", "hotels/other lodgings",
  "juice bars", "art galleries", "dance clubs", "swimming pools",
  "gyms", "bakeries", "beauty & spas", "cafes", "view points",
  "monuments", "gardens"
)

#RENAME CATEGORY 1 TO CATEGORY 24 ACCORDINGLY
  
category_columns <- paste0("Category.", 1:24)
names(Ratings )[which(names(Ratings ) %in% category_columns)] <- real_variables

#PRINT THE RENAMED COLUMNS
print(head(Ratings , 10))

#NAMES OF COLUMNS
names(Ratings )

#GET DIMENSIONS
cat("\nData Dimensions :",dim(Ratings),"\n")

#GET STRUCTURE INFORMATION
cat("\nData Structure:\n")

str(Ratings )

#GET SUMMARY STATISTICS 
cat("\nSummary Statistics\n")

# Loop through each variable and print its summary with title formatting
      for (col in names(Ratings)) {
                  cat("\n\033[1m", col, "\033[0m\n")  # Makes variable name bold in most terminals
                  print(summary(Ratings[[col]]))
      }

#CHECK THE DATATYPES (CLASS) OF ALL COLUMNS
col_classes <- sapply(Ratings , class)
  
        for (i in names(col_classes)) {
                  cat("\033[1m", i, ":\033[0m", col_classes[[i]], "\n")
        }

#CHANGE THE DATATYPE OF COL "LOCAL SERVICES"
Ratings $`local services` <- as.numeric(Ratings $`local services`)

#VERIFY THE DATATYPE OF COLUMN "LOCAL SERVICE"
str(Ratings $`local services`)

#CHECK FOR THE MISSING VALUES
Missing_Summary <- colSums(is.na(Ratings ))
print(Missing_Summary)

#BETTER REPRESENTATION OF MISSING VALUES USING A DATA FRAME
Missing_Summary <- data.frame(
                 Missing_Count = colSums(is.na(Ratings )),
                 Total_Values = nrow(Ratings ))

      print(Missing_Summary)
      
#REMOVE THE COLUMN NAMED X
Ratings  <- Ratings [, names(Ratings ) != "X"]

#PRINT CLEANED COLUMNS
cat("CLeaned Data Columns:",dim(Ratings ),"\n")

# REPLACING MISSING VALUES WITH THE METHOD OF IMPUTATION (USING MEAN TO REPLACE)
Ratings $`local services`[is.na(Ratings $`local services`)] <- mean(Ratings $`local services`, na.rm = TRUE)
Ratings $`burger/pizza shops`[is.na(Ratings $`burger/pizza shops`)] <- mean(Ratings $`burger/pizza shops`, na.rm = TRUE)
Ratings $`gardens`[is.na(Ratings $`gardens`)] <- mean(Ratings $`gardens`, na.rm = TRUE)

#VERIFY IF ANY MISSING VALUES
colSums(is.na(Ratings ))

#FUNCTION TO DETECT OUTLIERS USING IQR METHOD:
detect_outliers_iqr <- function(Ratings , column, factor = 2.0) {
      Q1 <- quantile(Ratings[[column]], 0.25, na.rm = TRUE)
      Q3 <- quantile(Ratings[[column]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - factor * IQR
      upper_bound <- Q3 + factor * IQR
                
       # Detect outliers (values outside the IQR bounds)
       outliers <- which(Ratings[[column]] < lower_bound | Ratings[[column]] > upper_bound)
       return(outliers)
          }
  
        # Get all numeric column names
        numeric_features <- Ratings %>%
        select(where(is.numeric)) %>%
        names()
         
          # Loop through each feature and count outliers
          for (feature in numeric_features) {
               outliers <- detect_outliers_iqr(Ratings, feature)
               cat(sprintf("Total outliers in '%s': %d\n", feature, length(outliers)))
             }

#WINSORIZE NUMERIC FEATURES USING THE IQR METHOD
 for (feature in numeric_features) {
       Q1 <- quantile(Ratings[[feature]], 0.25, na.rm = TRUE)
       Q3 <- quantile(Ratings[[feature]], 0.75, na.rm = TRUE)
       IQR <- Q3 - Q1
       lower_bound <- Q1 - 2 * IQR
       upper_bound <- Q3 + 2 * IQR
                                        
       # Cap values outside the bounds
       Ratings[[feature]][Ratings[[feature]] < lower_bound] <- lower_bound
       Ratings[[feature]][Ratings[[feature]] > upper_bound] <- upper_bound
        }
                 
        for (feature in numeric_features) {
        outliers <- detect_outliers_iqr(Ratings, feature)
        cat(sprintf("Remaining outliers in '%s': %d\n", feature, length(outliers)))
        }

                 
#PLOT THE NUMERIC FEATURES
library(ggplot2)
library(reshape2)
# MELT THE DATA TO LONG FORMAT
Ratings_melted <- melt(Ratings, measure.vars = first_10_features)

#CREATE A FACETED HISTOGRAM + DENSITY PLOT
ggplot(Ratings_melted, aes(x = value)) +
      geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.6) +
      geom_density(color = "red", size = 1) +
      facet_wrap(~variable, scales = "free_y", ncol = 3) +  # adjust y-axis per plot
      theme_minimal() +
      labs(title = "Distribution of First 10 Numerical Features",
                                            x = "Value", y = "Density")

#PLOT HISTOGRAM FOR NEXT FEATURES
remaining_features <- numeric_features[11:24]

#Melt the data to long format
Ratings_melted_remain <- melt(Ratings, measure.vars = remaining_features)

#Create faceted histogram + density plot
ggplot(Ratings_melted_remain, aes(x = value)) +
           geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.6) +
           geom_density(color = "red", size = 1) +
           facet_wrap(~variable, scales = "free_y", ncol = 3) +
           theme_minimal() +
           labs(title = "Distribution of Remaining Numerical Features (11–24)",
                                            x = "Value", y = "Density")

#QUESTION 1 : Which review categories most strongly influence overall destination ratings?

# Create overall rating as row-wise mean
Ratings$overall_rating <- rowMeans(Ratings[, numeric_features], na.rm = TRUE)

#SUMMARY OF OVERALL RATING COLUMN
summary(Ratings$overall_rating)

# CORRELATION ANALYSIS: IDENTIFYING FEATURE INFLUENCE
# This block performs correlation analysis to identify which individual review categories most strongly 
# influence the overall user satisfaction score.
hist(Ratings$overall_rating, breaks = 20, col = "skyblue", main = "Overall Rating Distribution")
  
        numeric_features <- names(Ratings)[
                  sapply(Ratings, is.numeric) & names(df) != "overall_rating"
              ]
            correlations <- cor(Ratings[, numeric_features], Ratings$overall_rating, use = "complete.obs")
          correlation_Ratings <- data.frame(
                    Feature = numeric_features,
                    Correlation = round(as.vector(correlations), 3)
                )
           head(correlation_Ratings)
           
#VIS 1
library(ggplot2)
           
ggplot(correlation_Ratings, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Correlation)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(
           title = "Correlation of Review Categories with Overall Rating",
           x = "Review Category",
           y = "Correlation Coefficient"
          ) +
          theme_minimal()

#TOP 5
top5 <- head(correlation_Ratings, 5)
ggplot(top5, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Correlation)) +
       geom_bar(stat = "identity", width = 0.5) +  # <- This controls bar width (smaller = thinner bars)
       coord_flip() +
       scale_fill_gradient2(
                 low = "lightcoral", mid = "orange", high = "red", 
                 midpoint = 0   # <- Optional: centers the color gradient at this correlation value
                 ) +
          labs(
                 title = "Top 5 Features Influencing Overall Rating",
                 x = "Review Feature",
                 y = "Correlation with Overall Rating"
                 ) +
           theme_minimal(base_size = 14) +
           theme(
                 plot.title = element_text(face = "bold", hjust = 0.5),
                 axis.title.y = element_text(face = "bold"),
                 axis.title.x = element_text(face = "bold")
                 )

#QUESTION 2 :  Can destinations be classified into distinct quality tiers based on review patterns?
            #1. Scale the data
            Ratings_scaled <- scale(Ratings[, numeric_features])
            # 2. Elbow Method (Optional for determining optimal clusters)
            set.seed(42)
            wss <- sapply(1:10, function(k){
              kmeans(Ratings_scaled, centers = k, nstart = 10)$tot.withinss
            })
            
            # 3. Apply KMeans (choose optimal number of clusters, e.g., 3)
            set.seed(42)
            kmeans_result <- kmeans(Ratings_scaled, centers = 3, nstart = 25)
            
            # 4. PCA for Visualization
            pca_result <- prcomp(Ratings_scaled)
            pca_Ratings <- as.data.frame(pca_result$x[, 1:2])  # First two principal components
            colnames(pca_Ratings) <- c("PC1", "PC2")
            
            # 5. Add cluster labels to PCA dataframe
            pca_Ratings$cluster <- as.factor(kmeans_result$cluster)
            library(ggplot2)

            ggplot(pca_Ratings, aes(x = PC1, y = PC2, color = cluster)) +
              geom_point(alpha = 0.7, size = 2.5) +
              labs(
                title = "Destination Clusters Visualized on PCA Axes",
                x = "Principal Component 1",
                y = "Principal Component 2"
              ) +
              theme_minimal(base_size = 14) +
              scale_color_brewer(palette = "Set1")

#Average Overall Rating per Cluster
Ratings$cluster <- pca_Ratings$cluster

cluster_avg <- aggregate(Ratings$overall_rating, by = list(Cluster = Ratings$cluster), FUN = mean)
colnames(cluster_avg) <- c("Cluster", "Average_Rating")

ggplot(cluster_avg, aes(x = as.factor(Cluster), y = Average_Rating, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Average_Rating, 2)), vjust = -0.5) +
  labs(
    title = "Average Overall Rating per Cluster",
    x = "Cluster",
    y = "Average Overall Rating"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

# TABULAR FORMAT OF CLUSTER AND CATEGORIES

    # Get only numeric features (excluding overall_rating if needed)
    numeric_features <- names(Ratings)[sapply(Ratings, is.numeric) & names(Ratings) != "overall_rating"]

    # Compute cluster-wise average for each feature
    cluster_feature_means <- aggregate(Ratings[, numeric_features], by = list(Cluster = Ratings$cluster), FUN = mean)

    # View result
    View(cluster_feature_means)

# PRINTING CATEGORIES ACCORDING TO RESPECTIVE CLUSTERS
      # Step 1: Get average ratings for each numeric feature by cluster
      cluster_feature_means <- aggregate(Ratings[, numeric_features], by = list(Cluster = Ratings$cluster), FUN = mean)

      # Step 2: Remove Cluster column for processing, and set row names
      feature_scores <- cluster_feature_means[, -1]
      rownames(feature_scores) <- paste("Cluster", cluster_feature_means$Cluster)

      # Step 3: Initialize containers
      top_features <- list()
      used_features <- c()
      
      # Step 4: Loop through each cluster and get top 8 unique features
      for (i in 1:nrow(feature_scores)) {
        cluster_name <- rownames(feature_scores)[i]
        
        # Convert the row to a numeric named vector
        cluster_scores <- as.numeric(feature_scores[i, ])
        names(cluster_scores) <- colnames(feature_scores)
        
        # Filter out already used features
        cluster_scores <- cluster_scores[!(names(cluster_scores) %in% used_features)]
        
        # Get top 8 unique features
        top_feats <- names(sort(cluster_scores, decreasing = TRUE))[1:min(8, length(cluster_scores))]
        
        # Save to list and mark as used
        top_features[[cluster_name]] <- top_feats
        used_features <- c(used_features, top_feats)
      }
      
      # Step 5: Convert result to a nice data frame for viewing
      top_features_Ratings <- as.data.frame(do.call(cbind, top_features), stringsAsFactors = FALSE)
      print(top_features_Ratings)
                       


                 