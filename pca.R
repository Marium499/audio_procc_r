library(tuneR)
library(audio)
library(seewave)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(stringr)
library(matrixStats)
library(melfcc)
library(caTools)
library(class)
library(e1071)
library(caret)
library(factoextra)
library("FactoMineR")
library("ggcorrplot")
library("corrr")



df <- read.csv("output/df_final_combined_6_mfcc.csv")
pca_test <- df_final
rownames(pca_test) <- NULL
colSums(is.na(pca_test))
pca_test <- drop_na(pca_test)
summary(pca_test)

n <- 35 # number of cols
#numerical_data <- pca_test[,9:n]
numerical_data <- pca_test[, !(names(pca_test) %in% c('path', 'filename', 'dataset', 'duration', 'age', 'gender', 'sample_rate'))]

X = numerical_data %>% select(-emotion)
Y = numerical_data %>% select(emotion)

data_normalized <- scale(X)
head(data_normalized)

dev.off()

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$scores
data.pca$loadings[, 1:3]


fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

fviz_cos2(data.pca, choice = "var", axes = 1:2)

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


###### predictions using PCA

pca4 <- PCA(data_normalized, ncp = 4)

x_4d_rescaled <- as.data.frame(predict.PCA(pca4, data_normalized))
#x_4d_rescaled_no_crema <- as.data.frame(predict(pca4, X_wo_cremad_rescaled))

target_names <- c('angry','disgust','fear','happy','neutral','sad','surprise')


plot_pca <- function(rescaled_data, target, component1, component2, component1_name, component2_name, title){
  
  
  #your code here 
  
  plt <- fviz_cluster(list(data = rescaled_data[,c(component1, component2)], cluster = target), 
                      geom = "point", 
                      frame.type = "norm", 
                      frame.color = "orange", 
                      frame.linetype = "dashed", 
                      pointsize = 1.5, 
                      pointshape = 21, 
                      pointcolor = "lightblue", 
                      title = title, 
                      ellipse = FALSE,
                      xlab = component1_name, 
                      ylab = component2_name, 
                      legend.title = "Target",
                      
                      ggtheme = theme_minimal())
  
  print(plt)
  
  
}

plot_pca(x_4d_rescaled, Y$emotion, 1,2,'1st component','2nd component', '2D-plot of the first two PCA components for SAVEE and CREMA-D dataset')






