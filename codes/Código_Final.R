#### Importando bibliotecas ####
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(caret)
library(tidyr)
library(sf)
library(gridExtra)
library(RColorBrewer)
library(factoextra)
library(e1071)
library(labelled)
library(candisc)

#### Questão 2 ####

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

#

str(Wolves[,4:12])
library(dplyr)
library(tidyr)
library(labelled)

# Função para remover rótulos e armazená-los em um vetor
remove_labels_and_store <- function(df) {
  labels <- sapply(df, function(x) attr(x, "label"))
  df[] <- lapply(df, function(x) {
    if (is.labelled(x)) {
      as.numeric(x)
    } else {
      as.numeric(x) # Garantir que as colunas sejam convertidas para numeric
    }
  })
  return(list(data = df, labels = labels))
}

# Função para calcular medidas estatísticas
medidas_resumo <- function(data) {
  result <- remove_labels_and_store(data)
  data <- result$data
  labels <- result$labels
  
  # Transformar os dados para o formato longo
  data_long <- data %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  
  # Calcular medidas estatísticas para cada variável
  medidas <- data_long %>%
    group_by(variable) %>%
    summarise(
      media = mean(value),
      dp = sd(value),
      coef = (mean(value) / sd(value)) * 100,
      mediana = median(value),
      IQQ = paste(quantile(value, 0.25), quantile(value, 0.75), sep = "-"),
      ASS = 3 * (mean(value) - median(value)) / sd(value)
    )
  
  # Adicionar os rótulos como uma nova coluna
  medidas$variable  <- labels[medidas$variable]
  
  return(medidas)
}

# Testar a função
medidas_resumo(Wolves[, 4:12])

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



#### Questão 3 ####
dados = read.table("https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/alimentos.txt", header = T)

row.names(dados)=dados$pais
#### Construindo hierarquia utilizando apenas leite e fruta/legumes
#
dados_LF = dplyr::select(dados, lech, fleg)
dados_LF = scale(dados_LF)
dist_matrix = dist(dados_LF)

hc_FL = hclust(dist_matrix, method = "complete")

plot(hc_FL, main = "Complete Linkage Hierarchical Clustering", sub = "", xlab = "")

#### Usando todas as variáveis // All 
dados_All = dplyr::select(dados, -pais)
dados_All = scale(dados_All)
dist_matrix_All = dist(dados_All)

hc_All = hclust(dist_matrix_All, method = "complete")

par(mfrow = c(2,1))
plot(hc_All, main = "Complete Linkage Hierarchical Clustering (ALL)", sub = "", xlab = "")
plot(hc_FL, main = "Complete Linkage Hierarchical Clustering (LL)", sub = "", xlab = "")

#### Todas as variaveis utilizando o average 

Xa = dplyr::select(dados, -pais)
Xa = scale(Xa)
dist_matrix_avg = dist(Xa)

hc_avg = hclust(dist_matrix_avg, method = "average")

plot(hc_avg, main = "Average Linkage Hierarchical Clustering", sub = "", xlab = "")

#### cortando a arvore // leites e legumes
#k = 2
clustersFL_2 <- cutree(hc_FL, k = 2)
cluster_fl2 = data.frame(clustersFL_2)
cluster_fl2$cluster <- clustersFL_2
cluster_fl2$pais = dados$pais
cluster_fl2 

#k = 3
clustersFL_3 <- cutree(hc_FL, k = 3)
cluster_fl3 = data.frame(clustersFL_3)
cluster_fl3$cluster <- clustersFL_3
cluster_fl3$pais = dados$pais
cluster_fl3

#### cortando a arvore // tudo = All
#k = 2
clustersAll_2 <- cutree(hc_All, k = 2)
cluster_All2 = data.frame(clustersAll_2)
cluster_All2$cluster <- clustersAll_2
cluster_All2$pais = dados$pais
cluster_All2

#k = 3
clustersAll_3 <- cutree(hc_All, k = 3)
cluster_All3 = data.frame(cluster_All3)
cluster_All3$cluster <- clustersAll_3
cluster_All3$pais = dados$pais
cluster_All3

par(mfrow = c(2,1))
plot(hc_All, main = "Complete Linkage Hierarchical Clustering (ALL)", sub = "", xlab = "")
plot(hc_avg, main = " AVG Linkage Hierarchical Clustering (ALL)", sub = "", xlab = "")


clusters_avg3 <- cutree(hc_avg, k = 3)
cluster_avg3 = data.frame(clusters_avg3)
cluster_avg3$cluster <- clustersavg_3
cluster_avg3$pais = dados$pais
cluster_avg3





#### Questão 4 ####
#### Lendo os dados ####

ips= read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/IPS.csv", sep = ';')

ips_anos = list(ips_2016 = dplyr::filter(ips, ano == 2016), 
                ips_2018 = dplyr::filter(ips, ano == 2018),
                ips_2020 = dplyr::filter(ips, ano == 2020), 
                ips_2022 = dplyr::filter(ips, ano == 2022))


#### PCA ####

# Remover colunas categóricas e escalar os dados
IPS_d <- scale(ips_anos$ips_2016[,-c(1,2,3)])
IPS_pca <- scale(ips_anos$ips_2016[,-c(1,2)])
rownames(IPS_d) <- ips_anos$ips_2016$regiao_administrativa
rownames(IPS_pca) <- ips_anos$ips_2016$regiao_administrativa

# Executar a PCA
pca_result <- PCA(IPS_pca, ncp = ncol(IPS_d),quanti.sup = c(1), graph = FALSE)
var_out <- get_pca_var(pca_result)
loadings <- var_out$coord
corelacao_pca <- var_out$cor
varia_pca <- row.names(corelacao_pca[abs(corelacao_pca[, "Dim.1"]) >= 0.8, ])

# Calcular scores manualmente multiplicando a matriz original escalada pelos loadings
scores_2016 <- as.data.frame(as.matrix(IPS_d) %*% as.matrix(loadings))

# Garantir que os nomes das linhas sejam consistentes para outros anos
rownames(ips_anos$ips_2018) <- ips_anos$ips_2016$regiao_administrativa
rownames(ips_anos$ips_2020) <- ips_anos$ips_2016$regiao_administrativa
rownames(ips_anos$ips_2022) <- ips_anos$ips_2016$regiao_administrativa

# Escalar os dados de outros anos
IPS_2018 <- scale(ips_anos$ips_2018[,-c(1,2,3)])
IPS_2020 <- scale(ips_anos$ips_2020[,-c(1,2,3)])
IPS_2022 <- scale(ips_anos$ips_2022[,-c(1,2,3)])

# Calcular scores para outros anos
scores_2018 <- as.data.frame(as.matrix(IPS_2018) %*% as.matrix(loadings))

scores_2020 <- as.data.frame(as.matrix(IPS_2020) %*% as.matrix(loadings))

scores_2022 <- as.data.frame(as.matrix(IPS_2022) %*% as.matrix(loadings))

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50),main = "") #gráficos

fviz_pca_var(pca_result, axes = c(1,2),col.var = "black",repel = TRUE) 

fviz_pca_ind(pca_result, axes = c(1,2)) 

fviz_pca_biplot(pca_result,addlabels = TRUE) 


#### Agrupamentos hierarquicos ####

#####Agrupando utilizando todas as variáveis #####

ips_c = ips_anos[["ips_2016"]][c(-1,-2,-3)]
ips_c = scale(ips_c)
row.names(ips_c) = ips_anos$ips_2016$regiao_administrativa
par(mfrow = c(1,1))

dist_ipsc = dist(scores_2016)

cluster_ips2 = hclust(dist_ipsc, method = "complete")

plot(cluster_ips2, main = "Modelo Hierárquico completo", xlab = "", ylab = "",sub = "", yaxt = "n", cex = 0.6)

# Adicionar retângulos ao redor dos clusters
rect.hclust(cluster_ips2, k = 4, border = 2:5) # Aqui, k define o número de clusters, ajuste conforme necessário
clusters_ips <- cutree(cluster_ips2, k = 4)


ips_cl = ips
ips_cl$cluster = rep(clusters_ips, times = length(unique(ips$ano)))

ips_clv = ips_cl |>  select(regiao_administrativa, ano, all_of(varia_pca), ips_geral, cluster)

ips_cl1 = filter(ips_cl, cluster == 1) |> select(regiao_administrativa, ano, all_of(varia_pca), ips_geral)

ips_cl1 |> 
  mutate(ano = factor(ano),
         ips_geral = round(ips_geral,1)) |> 
  newggslopegraph(Times = ano,
                  Measurement = ips_geral,
                  Grouping = regiao_administrativa) +
  labs(title = "IPS GERAL cluster 1", 
       subtitle = "Região administrativa")

ips_cl2 = filter(ips_cl, cluster == 2) |> select(regiao_administrativa, ano, all_of(varia_pca), ips_geral)

ips_cl2 |> 
  mutate(ano = factor(ano),
         ips_geral = round(ips_geral,1)) |> 
  newggslopegraph(Times = ano,
                  Measurement = ips_geral,
                  Grouping = regiao_administrativa) +
  labs(title = "IPS GERAL cluster 2", 
       subtitle = "Região administrativa")

ips_cl3 = filter(ips_cl, cluster == 3) |> select(regiao_administrativa, ano, all_of(varia_pca), ips_geral)

ips_cl3 |> 
  mutate(ano = factor(ano),
         ips_geral = round(ips_geral,1)) |> 
  newggslopegraph(Times = ano,
                  Measurement = ips_geral,
                  Grouping = regiao_administrativa) +
  labs(title = "IPS GERAL cluster 3", 
       subtitle = "Região administrativa")

ips_cl4 = filter(ips_cl, cluster == 4) |> select(regiao_administrativa, ano, all_of(varia_pca), ips_geral)

ips_cl4 |> 
  mutate(ano = factor(ano),
         ips_geral = round(ips_geral,1)) |> 
  newggslopegraph(Times = ano,
                  Measurement = ips_geral,
                  Grouping = regiao_administrativa) +
  labs(title = "IPS GERAL cluster 4", 
       subtitle = "Região administrativa")
colnames(ips_clv)
ips_cl |> 
  group_by(cluster,ano) |> 
  summarise(m_ipsgeral = round(mean(ips_geral),1)) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = m_ipsgeral,
                  Grouping = cluster) +
  labs(title = "Média do IPS Geral por Cluster", 
       subtitle = "Região administrativa")

par(mfrow = c(2,2))

a1 = ips_clv |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_vulnerabilidade_familiar,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_vulnerabilidade_familiar", 
       subtitle = "Região administrativa")

a2 = ips_clv |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_gravidez_adolescencia,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_gravidez_adolescencia", 
       subtitle = "Região administrativa")

a3 = ips_cl |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_adensamento_habitacional_excessivo,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_adensamento_habitacional_excessivo", 
       subtitle = "Região administrativa")

a4 = ips_cl |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_alfabetizacao,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_alfabetizacao", 
       subtitle = "Região administrativa")


gridExtra::grid.arrange(a1,a2,a3,a4)

g1 = ips_clv |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_frequencia_ensino_superior,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_frequencia_ensino_superior", 
       subtitle = "Região administrativa")

g2 = ips_clv |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_acesso_telefone_celular_fixo,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_acesso_telefone_celular_fixo", 
       subtitle = "Região administrativa")

g3 = ips_cl |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_pessoas_ensino_superior,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_pessoas_ensino_superior", 
       subtitle = "Região administrativa")

g4 = ips_cl |> 
  filter(regiao_administrativa %in% c("Rocinha", "Lagoa", "Maré", "Méier")) |> 
  mutate(ano = factor(ano)) |> 
  newggslopegraph(Times = ano,
                  Measurement = prop_acesso_internet,
                  Grouping = regiao_administrativa) +
  labs(title = "prop_acesso_internet", 
       subtitle = "Região administrativa")

gridExtra::grid.arrange(g1,g2,g3,g4)
