#### Importando bibliotecas ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(factoextra)
library(FactoMineR)
library(CGPfunctions)

#### Lendo os dados ####

ips= read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/IPS.csv", sep = ';')

ips_anos = list(ips_2016 = dplyr::filter(ips, ano == 2016), 
                ips_2018 = dplyr::filter(ips, ano == 2018),
                ips_2020 = dplyr::filter(ips, ano == 2020), 
                ips_2022 = dplyr::filter(ips, ano == 2022))


#### PCA ####

# Remover colunas categóricas e escalar os dados
IPS_d <- scale(ips_anos$ips_2016[,-c(1,2,3)])
rownames(IPS_d) <- ips_anos$ips_2016$regiao_administrativa

# Executar a PCA
pca_result <- PCA(IPS_d, ncp = ncol(IPS_d), graph = FALSE)
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
