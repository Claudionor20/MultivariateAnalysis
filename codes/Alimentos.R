dados = read.table("https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/alimentos.txt", header = T)


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

plot(hc_All, main = "Complete Linkage Hierarchical Clustering", sub = "", xlab = "")

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


