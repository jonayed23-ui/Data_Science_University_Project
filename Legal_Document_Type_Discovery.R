install.packages("NLP")
install.packages("stringr")
install.packages("tm")
install.packages("textstem")
install.packages("Matrix")
install.packages("uwot")
install.packages("dbscan")
library(NLP)
library(readr)
library(stringr)
library(tm)
library(textstem)
library(Matrix)
library(uwot)
library(dbscan)
library(cluster)
library(aricode)

df <- read.csv("E:/10 sem/final/ds project/IDB Project/IDB Project/legal_dataset.csv", header = TRUE, sep = ",")

set.seed(42)
df <- df[sample(nrow(df)), ]
rownames(df) <- NULL
df$number <- seq_len(nrow(df))
df <- df[, !names(df) %in% c("label")]

cat(substr(df$text[1], 1, 300), "\n")
df$text <- str_to_lower(df$text)
cat(substr(df$text[1], 1, 300), "\n")

df$text <- str_replace_all(df$text, "[[:punct:]]", " ")
df$text <- str_replace_all(df$text, "\\s+", " ")
df$text <- str_trim(df$text)
cat(substr(df$text[1], 1, 300), "\n")

legal_stopwords <- c(
  stopwords("en"),
  "shall", "may", "party", "parties", "pursuant", "accordance",
  "section", "herein", "thereof", "hereby", "provided", "including",
  "without", "upon", "within", "under", "terms", "conditions",
  "rights", "obligations", "agreement", "law", "date", "time",
  "made", "set", "forth", "following", "per", "also", "one", "two"
)
df$text <- sapply(df$text, function(x) {
  words <- str_split(x, " ")[[1]]
  words <- words[!words %in% legal_stopwords]
  paste(words, collapse = " ")
})
cat(substr(df$text[1], 1, 300), "\n")

df$text <- lemmatize_strings(df$text)
cat(substr(df$text[1], 1, 300), "\n")

dim(df)

corpus <- VCorpus(VectorSource(df$text))
tfidf_matrix <- DocumentTermMatrix(corpus, control = list(
  weighting = weightTfIdf,
  minDocFreq = 2,
  bounds = list(global = c(2, Inf))
))
tfidf_matrix <- removeSparseTerms(tfidf_matrix, 0.95)
tfidf_dense <- as.matrix(tfidf_matrix)
dim(tfidf_dense)
sort(colMeans(tfidf_dense), decreasing = TRUE)[1:20]
tfidf_dense[1:5, 1:5]

set.seed(42)
umap_result <- umap(tfidf_dense,
                    n_components = 2,
                    n_neighbors = 30,
                    min_dist = 0.01,
                    metric = "cosine")

dim(umap_result)
head(umap_result, 5)
range(umap_result[, 1])
range(umap_result[, 2])

set.seed(42)
wss <- sapply(1:10, function(k) {
  kmeans(umap_result, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19)

optimal_k <- max(3, which(diff(diff(wss)) == max(diff(diff(wss)))) + 1)
optimal_k

kmeans_result <- kmeans(umap_result, centers = optimal_k, nstart = 25, iter.max = 100)
kmeans_clusters <- kmeans_result$cluster

table(kmeans_clusters)
kmeans_result$centers

hdbscan_result <- hdbscan(umap_result, minPts = 10)
hdbscan_clusters <- hdbscan_result$cluster

table(hdbscan_clusters)
hdbscan_result$membership_prob[1:10]

dist_matrix <- dist(umap_result, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "ward.D2")
hclust_clusters <- cutree(hclust_result, k = optimal_k)

table(hclust_clusters)

par(mar = c(2, 4, 2, 1))
plot(hclust_result,
     labels = FALSE,
     hang = -1,
     xlab = "",
     sub = "",
     main = "Cluster Dendrogram")
rect.hclust(hclust_result, k = optimal_k, border = c("red", "blue", "green"))

kmeans_dist  <- table(kmeans_clusters)
hdbscan_dist <- table(hdbscan_clusters)
hclust_dist  <- table(hclust_clusters)

kmeans_dist
hdbscan_dist
hclust_dist

kmeans_df  <- as.data.frame(kmeans_dist)
hdbscan_df <- as.data.frame(hdbscan_dist)
hclust_df  <- as.data.frame(hclust_dist)

names(kmeans_df)  <- c("Cluster", "Count")
names(hdbscan_df) <- c("Cluster", "Count")
names(hclust_df)  <- c("Cluster", "Count")

kmeans_df$Percentage  <- round(kmeans_df$Count  / sum(kmeans_df$Count)  * 100, 2)
hdbscan_df$Percentage <- round(hdbscan_df$Count / sum(hdbscan_df$Count) * 100, 2)
hclust_df$Percentage  <- round(hclust_df$Count  / sum(hclust_df$Count)  * 100, 2)

kmeans_df
hdbscan_df
hclust_df

barplot(kmeans_dist,
        main = "KMeans Cluster Distribution",
        xlab = "Cluster",
        ylab = "Number of Documents",
        col = 1:optimal_k)

barplot(hdbscan_dist,
        main = "HDBSCAN Cluster Distribution",
        xlab = "Cluster",
        ylab = "Number of Documents",
        col = seq_along(hdbscan_dist))

barplot(hclust_dist,
        main = "Hierarchical Cluster Distribution",
        xlab = "Cluster",
        ylab = "Number of Documents",
        col = 1:optimal_k)

true_labels <- read.csv("E:/10 sem/final/ds project/IDB Project/IDB Project/legal_dataset.csv", header = TRUE, sep = ",")
set.seed(42)
true_labels <- true_labels[sample(nrow(true_labels)), ]
rownames(true_labels) <- NULL
true_labels <- true_labels$label

sil_kmeans  <- silhouette(kmeans_clusters, dist_matrix)
sil_hclust  <- silhouette(hclust_clusters, dist_matrix)
sil_hdbscan <- silhouette(hdbscan_clusters[hdbscan_clusters != 0],
                          dist(umap_result[hdbscan_clusters != 0, ], method = "euclidean"))

mean(sil_kmeans[, 3])
mean(sil_hclust[, 3])
mean(sil_hdbscan[, 3])

NMI(true_labels, kmeans_clusters)
NMI(true_labels, hclust_clusters)
NMI(true_labels[hdbscan_clusters != 0], hdbscan_clusters[hdbscan_clusters != 0])

ARI(true_labels, kmeans_clusters)
ARI(true_labels, hclust_clusters)
ARI(true_labels[hdbscan_clusters != 0], hdbscan_clusters[hdbscan_clusters != 0])

tfidf_df <- as.data.frame(tfidf_dense)

tfidf_df$kmeans_cluster  <- kmeans_clusters
tfidf_df$hclust_cluster  <- hclust_clusters
tfidf_df$hdbscan_cluster <- hdbscan_clusters

for (k in 1:optimal_k) {
  cluster_docs <- tfidf_df[tfidf_df$kmeans_cluster == k, !names(tfidf_df) %in% c("kmeans_cluster", "hclust_cluster", "hdbscan_cluster")]
  term_means <- sort(colMeans(cluster_docs), decreasing = TRUE)
  print(paste("KMeans Cluster", k))
  print(head(term_means, 10))
}

for (k in 1:optimal_k) {
  cluster_docs <- tfidf_df[tfidf_df$hclust_cluster == k, !names(tfidf_df) %in% c("kmeans_cluster", "hclust_cluster", "hdbscan_cluster")]
  term_means <- sort(colMeans(cluster_docs), decreasing = TRUE)
  print(paste("Hierarchical Cluster", k))
  print(head(term_means, 10))
}

unique_hdbscan <- sort(unique(hdbscan_clusters[hdbscan_clusters != 0]))
for (k in unique_hdbscan) {
  cluster_docs <- tfidf_df[tfidf_df$hdbscan_cluster == k, !names(tfidf_df) %in% c("kmeans_cluster", "hclust_cluster", "hdbscan_cluster")]
  term_means <- sort(colMeans(cluster_docs), decreasing = TRUE)
  print(paste("HDBSCAN Cluster", k))
  print(head(term_means, 10))
}

true_df <- read.csv("E:/10 sem/final/ds project/IDB Project/IDB Project/legal_dataset.csv", header = TRUE, sep = ",")
set.seed(42)
true_df <- true_df[sample(nrow(true_df)), ]
rownames(true_df) <- NULL

label_map <- c("1" = "Court", "2" = "Contract", "3" = "Policy")

kmeans_identity <- tapply(true_df$label, kmeans_clusters, function(x) {
  names(which.max(table(x)))
})
kmeans_identity <- label_map[kmeans_identity]
names(kmeans_identity) <- paste("Cluster", 1:optimal_k)
kmeans_identity

hclust_identity <- tapply(true_df$label, hclust_clusters, function(x) {
  names(which.max(table(x)))
})
hclust_identity <- label_map[hclust_identity]
names(hclust_identity) <- paste("Cluster", 1:optimal_k)
hclust_identity

hdbscan_nonoise     <- hdbscan_clusters[hdbscan_clusters != 0]
true_labels_nonoise <- true_df$label[hdbscan_clusters != 0]
hdbscan_identity <- tapply(true_labels_nonoise, hdbscan_nonoise, function(x) {
  names(which.max(table(x)))
})
hdbscan_identity <- label_map[hdbscan_identity]
names(hdbscan_identity) <- paste("Cluster", names(hdbscan_identity))
hdbscan_identity

x_range <- range(umap_result[, 1])
y_range <- range(umap_result[, 2])

x_pad <- (x_range[2] - x_range[1]) * 0.15
y_pad <- (y_range[2] - y_range[1]) * 0.15

plot(umap_result[, 1], umap_result[, 2],
     col = kmeans_clusters,
     pch = 19,
     cex = 0.6,
     main = "KMeans Clusters",
     xlab = "UMAP 1",
     ylab = "UMAP 2",
     xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
     ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))
legend("topright",
       legend = paste("Cluster", 1:optimal_k, "-", kmeans_identity),
       col = 1:optimal_k,
       pch = 19,
       cex = 0.8)

plot(umap_result[, 1], umap_result[, 2],
     col = hclust_clusters,
     pch = 19,
     cex = 0.6,
     main = "Hierarchical Clusters",
     xlab = "UMAP 1",
     ylab = "UMAP 2",
     xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
     ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))
legend("topright",
       legend = paste("Cluster", 1:optimal_k, "-", hclust_identity),
       col = 1:optimal_k,
       pch = 19,
       cex = 0.8)

hdbscan_colors <- hdbscan_clusters + 1
plot(umap_result[, 1], umap_result[, 2],
     col = hdbscan_colors,
     pch = 19,
     cex = 0.6,
     main = "HDBSCAN Clusters",
     xlab = "UMAP 1",
     ylab = "UMAP 2",
     xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
     ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))
legend("topleft",
       legend = c("Noise", paste("Cluster", names(hdbscan_identity), "-", hdbscan_identity)),
       col = c(1, as.integer(names(hdbscan_identity)) + 1),
       pch = 19,
       cex = 0.8)