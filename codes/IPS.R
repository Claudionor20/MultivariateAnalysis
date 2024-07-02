# Importando pacotes

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
library(candisc)



###### Questão 4 ######

#### Base de dados ####

# Leitura da base de dados pelo github

ips= read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/IPS.csv", sep = ';')

ips_anos = list(ips_2016 = dplyr::filter(ips, ano == 2016), 
                ips_2018 = dplyr::filter(ips, ano == 2018),
                ips_2020 = dplyr::filter(ips, ano == 2020), 
                ips_2022 = dplyr::filter(ips, ano == 2022))

names(ips)


#### ANÁLISE DESCRITIVA ####

# Média de IPS ao longo dos anos por região administrativa

ips |>
  group_by(regiao_administrativa) |>
  summarise(media_ips = mean(ips_geral)) |>
  arrange(desc(media_ips)) %>%
  ggplot(aes(x = reorder(regiao_administrativa, media_ips), y = media_ips)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Média de IPS por Região Administrativa",
       x = "Região Administrativa",
       y = "Média de IPS")


# Fazendo histograma conjunto de todas as variáveis

data_long <- melt(ips, id.vars = c("ano", "regiao_administrativa"))

ggplot(data_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Histogramas Dos Indicadores de IPS",
       x = "Valor",
       y = "Frequência")

# Boxplot do IPS por ano

ips_geral <- ips|>
  select(ano,ips_geral)

ips_geral|>
  ggplot(aes(x = as.factor(ano), y = ips_geral)) +
  geom_boxplot(aes(fill = "Ano"), color = "gray50", outlier.color = "red", outlier.shape = 16, outlier.size = 3) +
  scale_fill_manual(values = "#66C2A5") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  
    panel.grid.minor = element_blank(),  
    legend.position = "none"  
  ) +
  labs(
    title = "Distribuição do IPS por Ano",
    x = "Ano",
    y = "IPS"
  )

# Evolução anual do IPS de cada regisão administrativa

ips |>
  ggplot(aes(x = factor(ano), y = ips_geral)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = regiao_administrativa), width = 0.7) +
  scale_fill_viridis_d() +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank()  
  ) +
  labs(
    title = "Evolução do IPS por Região Administrativa",
    x = "Ano",
    y = "IPS",
    fill = "Região Administrativa"
  ) +
  facet_wrap(~ regiao_administrativa, scales = "free_y", ncol = 6)  


# Comportamento das variáveis por ano

ips_long <- ips |>
  gather(key = "variavel", value = "valor", -ano, -regiao_administrativa)

ggplot(ips_long, aes(x = as.factor(ano), y = valor, group = interaction(ano, variavel), color = as.factor(ano))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores ao Longo do Tempo",
       x = "Ano", y = "Valor")

# Mapa de calor IPS
dados_geojson <- st_read("https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/mapa.geojson")
dados_ips_2022 <- ips[ips$ano == 2022, ]


dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Maré"] <- "Complexo da Maré"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Iraja"] <- "Irajá"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Portuaria"] <- "Portuária"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "São Cristovão"] <- "São Cristóvão"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Barra Da Tijuca"] <- "Barra da Tijuca"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Cidade De Deus"] <- "Cidade de Deus"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Complexo Do Alemão"] <- "Complexo do Alemão"
dados_ips_2022$regiao_administrativa[dados_ips_2022$regiao_administrativa == "Ilha Do Governador"] <- "Ilha do Governador"

regioes_ips_2020 <- merge(dados_geojson, dados_ips_2022, by.x = "nomera", by.y = "regiao_administrativa")

ggplot() +
  geom_sf(data = regioes_ips_2020, aes(fill = ips_geral)) +
  geom_sf_text(data = regioes_ips_2020, aes(label = nomera), 
               size = 3.5, color = "black", fontface = "bold", 
               check_overlap = TRUE) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "BuPu"),
    values = scales::rescale(c(min(regioes_ips_2020$ips_geral), max(regioes_ips_2020$ips_geral))),
    name = "IPS Geral"
  ) +
  theme_minimal() +
  labs(title = "Mapa de Calor do IPS Geral por Região Administrativa (2022)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

#### PCA ####
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)

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

# Adicionar os clusters aos scores
scores_2016$cluster <- as.factor(clusters_ips)
scores_2018$cluster <- as.factor(clusters_ips)
scores_2020$cluster <- as.factor(clusters_ips)
scores_2022$cluster <- as.factor(clusters_ips)

# Função para criar o contorno de um cluster
hull <- function(df) df[chull(df$Dim.1, df$Dim.2), ]

# Aplicar a função para cada cluster
hulls <- scores_2016 %>%
  group_by(cluster) %>%
  do(hull(.))

# Visualizar os clusters usando as componentes principais desejadas
# Exemplo: Usar PC1 e PC2 como eixos x e y
ggplot() +
  geom_polygon(data = hulls, aes(x = Dim.1, y = Dim.2, fill = cluster, group = cluster), alpha = 0.3) +
  geom_point(data = scores_2016, aes(x = Dim.1, y = Dim.2, color = cluster), size = 3) +
  geom_text(data = scores_2016, aes(x = Dim.1, y = Dim.2, label = rownames(scores_2016)), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Cluster IPS 2016", x = "PC1", y = "PC2") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-30,30,5))+
  scale_y_continuous(breaks = seq(-20,20,5), limits = c(-10,20))+
  scale_fill_brewer(palette  = "Dark2") + # Ajuste das cores dos clusters
  scale_color_brewer(palette = "Dark2")

hulls2018 <- scores_2018 %>%
  group_by(cluster) %>%
  do(hull(.))


ggplot() +
  geom_polygon(data = hulls2018, aes(x = Dim.1, y = Dim.2, fill = cluster, group = cluster), alpha = 0.3) +
  geom_point(data = scores_2018, aes(x = Dim.1, y = Dim.2, color = cluster), size = 3) +
  geom_text(data = scores_2018, aes(x = Dim.1, y = Dim.2, label = rownames(scores_2018)), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Cluster IPS 2018", x = "PC1", y = "PC2") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-30,30,5))+
  scale_y_continuous(breaks = seq(-20,20,5), limits = c(-10,20))+
  scale_fill_brewer(palette  = "Dark2") + # Ajuste das cores dos clusters
  scale_color_brewer(palette = "Dark2")


hulls2020 <- scores_2020 %>%
  group_by(cluster) %>%
  do(hull(.))

ggplot() +
  geom_polygon(data = hulls2020, aes(x = Dim.1, y = Dim.2, fill = cluster, group = cluster), alpha = 0.3) +
  geom_point(data = scores_2020, aes(x = Dim.1, y = Dim.2, color = cluster), size = 3) +
  geom_text(data = scores_2020, aes(x = Dim.1, y = Dim.2, label = rownames(scores_2020)), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Cluster IPS 2020", x = "PC1", y = "PC2") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-30,30,5))+
  scale_y_continuous(breaks = seq(-20,20,5), limits = c(-10,20))+
  scale_fill_brewer(palette  = "Dark2") + # Ajuste das cores dos clusters
  scale_color_brewer(palette = "Dark2")


hulls2022 <- scores_2022 %>%
  group_by(cluster) %>%
  do(hull(.))


ggplot() +
  geom_polygon(data = hulls2022, aes(x = Dim.1, y = Dim.2, fill = cluster, group = cluster), alpha = 0.3) +
  geom_point(data = scores_2022, aes(x = Dim.1, y = Dim.2, color = cluster), size = 3) +
  geom_text(data = scores_2022, aes(x = Dim.1, y = Dim.2, label = rownames(scores_2022)), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Cluster IPS 2022", x = "PC1", y = "PC2") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-30,30,5))+
  scale_y_continuous(breaks = seq(-20,20,5), limits = c(-10,20))+
  scale_fill_brewer(palette  = "Dark2") + # Ajuste das cores dos clusters
  scale_color_brewer(palette = "Dark2")


#####

tipo = c("SAU", "SAU", "SAU", "SAU", "SAN", "SAN", "SAN", "DIS", "INF", "DIS", 
         "SEG", "SEG", "EDU", "EDU", "EDU", "EDU", "INF", "INF", "SAU", "SAU", 
         "SAU", "SAN", "MEI", "INF", "SEG", "INF", "DIS", "SAU", "DIS", "DIS", 
         "SEG", "SEG", "DIS", "EDU", "EDU", "EDU")

ind_tipo = data.frame(variavel = names(ips)[-c(1,2,3)], tipo = tipo)
regioes = ips_anos$ips_2016$regiao_administrativa
ips_tipos2016_long = list()
ips_tipos2016 = list()

#
library(CGPfunctions)

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


