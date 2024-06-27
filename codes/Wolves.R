#### Pacotes ####

library(candisc)
library(dplyr)
library(ggplot2)
library(cluster)
library(gridExtra)
library(factoextra)

####Base de dados  ####
data(Wolves)
str(Wolves)

naniar::miss_var_summary(Wolves)

#### Análise exploratória (2A): ####

#

table(Wolves$sex)
table(Wolves$location)

Wolves |> ggplot(aes(x = group))+
  geom_bar(fill = "#5A639C", width = .5)+
  labs( y = "Frequência", x = "Grupo")+
  theme_minimal()


#Histogramas - quantitativas
par(mfrow = c(3,3))
str(Wolves)
for(i in 1:ncol(Wolves)){
  name = names(Wolves)[i]
  if(is.numeric(Wolves[[i]])){
    label = attr(Wolves[[i]], "label")
    main_title = ifelse(!is.null(label), label, name)
    
    hist(Wolves[[i]], xlab= "", main = main_title)
  }
}


#Boxplot por grupo

plots <- list()

for (i in 1:ncol(Wolves)) {
  var_name <- names(Wolves)[i]
  
  if (is.numeric(Wolves[[var_name]])) {
    label <- attr(Wolves[[var_name]], "label")
    
    main_title <- ifelse(!is.null(label), label, var_name)
  
    p <- ggplot(data = Wolves, aes_string(x = "group", y = var_name)) +
      geom_boxplot() +
      labs(title = main_title, x = "", y = "") +
      theme_minimal()
    
    plots[[length(plots) + 1]] <- p
  }
}
do.call("grid.arrange", c(plots, ncol = 3))

# Análise de correlação

par(mfrow = c(1,1))
pairs(Wolves[,4:12], 
      pch=21)

corrplot::corrplot(cor(Wolves[,4:12]), method = "square" , order = "hclust", tl.col='black', tl.cex=.75) 

cor(Wolves[,4:12])

#### Análise Discriminante (2B) ####

head(Wolves)

N_W <- Wolves[,4:length(Wolves)]


mvn(data = N_W, mvnTest = "mardia", multivariatePlot = "qq") #Conjuntamente segue normalmultivariada, ou seja, os grupos seguem normal multivarida.

X<-as.matrix(Wolves[,c(-1,-2,-3)])
X_d <- dist(X)
X <- betadisper(X_d,Wolves$group)
permutest(X) # Matriz de covariancias são homogeneas
plot(X)


Manova <- aov(x1+x2+x3+x4+x5+x6+x7+x8+x9 ~ group, data = Wolves)
summary(Manova) # As medias não são homogeneas

#Análise discriminante
Wolves_n <- Wolves[,c(1,4:12)]
spe.lda <- lda(group ~., data=Wolves_n)

round(predict(spe.lda)$posterior,2)

ta <- table(Wolves[,1],predict(spe.lda)$class)

diag(prop.table(ta,1))

confusionmatrix(predict(spe.lda)$class, Wolves$group)


#### K-means (2C) ####
Wolves
X = Wolves[,-c(1:3)]

#X = X |> 
  #mutate(across(c(location,sex), as.factor))

X = as.matrix(X)
str(X)
# Determinar o número ótimo de clusters usando o método WSS (Within-Cluster Sum of Squares)
fviz_nbclust(X, kmeans, method = "wss")

# Definir o número de clusters
K = 5

# Realizar o algoritmo K-means nos dados
set.seed(123)  # Para garantir a reprodutibilidade
km_res <- kmeans(X, centers = K)

# Plotar os clusters fornecidos pelo K-means
plot(X, col = km_res$cluster, pch = 16, main = "Clusters K-means")

# Plotar os centróides dos grupos
points(km_res$centers, col = 1:K, pch = 8, cex = 2)

# Plotar o gráfico de agrupamento usando fviz_cluster
fviz_cluster(km_res, data = X,
             ggtheme = theme_minimal(),
             main = "Gráfico de Agrupamento")

#### Hierarquico (2D) ####
###Mantendo a variável location e sex 

##como numeric
Xh = Wolves[,-1]
Xh$location = ifelse(Xh$location == "rm", 1,0)
Xh$sex = ifelse(Xh$sex == "m", 1,0)

Xh = Xh |> 
  mutate(across(c(location,sex), as.numeric))

Xh = scale(Xh)

dist_matrix <- dist(Xh)

hc_complete <- hclust(dist_matrix, method = "complete")

plot(hc_complete, main = "Complete Linkage Hierarchical Clustering", sub = "", xlab = "")

