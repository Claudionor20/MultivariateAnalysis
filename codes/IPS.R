#### Base de dados ####

# Leitura da base de dados pelo github

ips= read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/IPS.csv", sep = ';')

ips_anos = list(ips_2016 = filter(ips, ano == 2016), 
                ips_2018 = filter(ips, ano == 2018),
                ips_2020 = filter(ips, ano == 2020), 
                ips_2022 = filter(ips, ano == 2022))

names(ips)
# Importando pacotes

library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(caret)
library(tidyr)
library(sf)

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
dados_geojson <- st_read("mapa.geojson")
dados_ips_2020 <- ips[ips$ano == 2022, ]
# Substituir Maré por Complexo da Maré
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Maré"] <- "Complexo da Maré"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Iraja"] <- "Irajá"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Portuaria"] <- "Portuária"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "São Cristovão"] <- "São Cristóvão"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Barra Da Tijuca"] <- "Barra da Tijuca"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Cidade De Deus"] <- "Cidade de Deus"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Complexo Do Alemão"] <- "Complexo do Alemão"
dados_ips_2020$regiao_administrativa[dados_ips_2020$regiao_administrativa == "Ilha Do Governador"] <- "Ilha do Governador"

# Achar um shapefile melhor se pá! Já que na base do shapefile tem Páqueta e não tem Rio de Janeiro


regioes_ips_2020 <- merge(dados_geojson, dados_ips_2020, by.x = "nomera", by.y = "regiao_administrativa")
library(RColorBrewer)

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



# Fazendo correlação entre as variáveis

correlacao = cor(ips[,-c(1,2)])
dev.new(width = 2000, height = 2000, res = 3000) # Largura e altura em polegadas
corrplot(correlacao, method = "circle", tl.cex = 0.6)

# Variaveis com alta correlação entre si (Sem contar o IPS)
base_sem_ips <- ips[,-c(1,2,3)]
correlacao_sem_ips <- cor(base_sem_ips)

variaveis_correlacionadas <- findCorrelation(correlacao_sem_ips, cutoff = 0.7)

nomes_variaveis_correlacionadas <- colnames(base_sem_ips)[variaveis_correlacionadas]
print(nomes_variaveis_correlacionadas)

#### PCA ####
library(dplyr)
library(tidyr)
library(ggplot2)

# Remover colunas categóricas e escalar os dados
IPS_d <- scale(ips_anos$ips_2016[,-c(1,2,3)])
rownames(IPS_d) <- ips_anos$ips_2016$regiao_administrativa

# Executar a PCA
pca_result <- PCA(IPS_d, ncp = ncol(IPS_d), graph = FALSE)
var_out <- get_pca_var(pca_result)
loadings <- var_out$coord

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


pca_result_2018 <- PCA(IPS_2018, ncp = ncol(IPS_2018), graph = FALSE)
var_out_2018 <- get_pca_var(pca_result_2018)
loadings_2018 <- var_out_2018$coord


fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50),main = "") #gráficos

fviz_pca_var(pca_result, axes = c(1,2),col.var = "black",repel = TRUE) 

fviz_pca_ind(pca_result, axes = c(1,2)) 

fviz_pca_biplot(pca_result,addlabels = TRUE) 


#### Agrupamento por k-means ####

# Guilherme

# Supondo que a base de ips se chama "ips" e já está carregada

# Filtrar os ips para os anos desejados
anos <- c(2016, 2018, 2020, 2022)
ips_filtrados <- ips %>% filter(ano %in% anos)

# Lista para armazenar os resultados do k-means por ano
clusters_por_ano <- list()

# Função para calcular os convex hulls
get_convex_hulls <- function(data) {
  hulls <- data %>%
    group_by(cluster) %>%
    slice(chull(ips_geral, prop_mobilidade_urbana)) %>%
    ungroup()
  return(hulls)
}

# Aplicar k-means para cada ano separadamente
for (ano_atual in anos) {
  ips_ano <- ips_filtrados %>% filter(ano == ano_atual)
  
  # Selecionar as variáveis para o k-means
  ips_kmeans <- ips_ano %>% select(ips_geral, prop_mobilidade_urbana)
  
  # Definir o número de clusters (k). Aqui usamos k = 3 como exemplo
  k <- 3
  
  # Executar o k-means
  set.seed(123) # Para reprodutibilidade
  kmeans_result <- kmeans(ips_kmeans, centers = k)
  
  # Adicionar os resultados do cluster aos ips
  ips_ano$cluster <- as.factor(kmeans_result$cluster)
  
  # Armazenar os ips do ano com os clusters
  clusters_por_ano[[as.character(ano_atual)]] <- ips_ano
}

# Função para criar o gráfico de dispersão com contornos e escalas fixas
plot_clusters <- function(ips, ano) {
  hulls <- get_convex_hulls(ips)
  ggplot(ips, aes(x = ips_geral, y = prop_mobilidade_urbana, color = cluster)) +
    geom_point() +
    geom_polygon(data = hulls, aes(fill = cluster, group = cluster), alpha = 0.2, color = NA) +
    geom_path(data = hulls, aes(group = cluster), color = "black", linetype = "dashed") + # Adiciona os contornos tracejados
    labs(title = paste("Clusters para o ano de", ano), x = "IPS Geral", y = "Proporção de Mobilidade Urbana") +
    xlim(30, 100) + # Ajustar a escala do eixo x
    ylim(0, 100) +    # Ajustar a escala do eixo y
    theme_minimal()
}

# Criar os gráficos para cada ano
plots <- lapply(names(clusters_por_ano), function(ano) {
  plot_clusters(clusters_por_ano[[ano]], ano)
})

# Mostrar os gráficos
for (plot in plots) {
  print(plot)
}

#### Agrupamentos hierarquicos ####

#####Agrupando utilizando todas as variáveis #####

ips_c = ips_anos[["ips_2016"]][c(-1,-2,-3)]
ips_c = scale(ips_c)
row.names(ips_c) = ips_anos$ips_2016$regiao_administrativa

dist_ipsc = dist(scores_2016)

cluster_ips = hclust(dist_ipsc, method = "single")
par(mfrow = c(1,1))
plot(cluster_ips, main = "single")

cluster_ips2 = hclust(dist_ipsc, method = "complete")
plot(cluster_ips2, main = "complete")

cluster_ips3 = hclust(dist_ipsc, method = "average")

plot(cluster_ips3, main = "avg")
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
####### SAUDE #######
ips_tipos2016_long$saude = inner_join(ips_long, ind_tipo, by = "variavel") |> 
  filter(tipo == "SAU" & ano == 2016) |> select(-tipo)

ips_tipos2016$saude = ips_tipos2016_long$saude  |> 
  tidyr::spread(key = variavel, value = valor) |> 
  select(-ano, -regiao_administrativa)

fviz_cluster(list(data = ips_tipos2016$saude, cluster = clusters_ips), 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = regioes), vjust = -0.5, hjust = 0.5)+
  labs(title = "SAUDE")

###### Saneamento Básico: SAN ######

ips_tipos2016_long$saneamento = inner_join(ips_long, ind_tipo, by = "variavel") |> 
  filter(tipo == "SAN" & ano == 2016) |> select(-tipo)

ips_tipos2016$saneamento = ips_tipos2016_long$saneamento  |> 
  tidyr::spread(key = variavel, value = valor) |> 
  select(-ano, -regiao_administrativa)

fviz_cluster(list(data = ips_tipos2016$saneamento, cluster = clusters_ips), 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = regioes), vjust = -0.5, hjust = 0.5)+
  labs(title = "Saneamento Básico")

###### Segurança: SEG ######

ips_tipos2016_long$seguranca = inner_join(ips_long, ind_tipo, by = "variavel") |> 
  filter(tipo == "SEG" & ano == 2016) |> select(-tipo)

ips_tipos2016$seguranca = ips_tipos2016_long$seguranca  |> 
  tidyr::spread(key = variavel, value = valor) |> 
  select(-ano, -regiao_administrativa)

fviz_cluster(list(data = ips_tipos2016$seguranca , cluster = clusters_ips), 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = regioes), vjust = -0.5, hjust = 0.5)+
  labs(title = "Segurança")
###### Educação: EDU ######

ips_tipos2016_long$educacao = inner_join(ips_long, ind_tipo, by = "variavel") |> 
  filter(tipo == "EDU" & ano == 2016) |> select(-tipo)

ips_tipos2016$educacao = ips_tipos2016_long$educacao  |> 
  tidyr::spread(key = variavel, value = valor) |> 
  select(-ano, -regiao_administrativa)

fviz_cluster(list(data = ips_tipos2016$educacao, cluster = clusters_ips), 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = regioes), vjust = -0.5, hjust = 0.5)+
  labs(title = "Educação")
###### Infraestrutura e Mobilidade: INF######

ips_tipos2016_long$infra = inner_join(ips_long, ind_tipo, by = "variavel") |> 
  filter(tipo == "INF" & ano == 2016) |> select(-tipo)

ips_tipos2016$infra = ips_tipos2016_long$infra  |> 
  tidyr::spread(key = variavel, value = valor) |> 
  select(-ano, -regiao_administrativa)


fviz_cluster(list(data = ips_tipos2016$infra, cluster = clusters_ips), 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = regioes), vjust = -0.5, hjust = 0.5)+
  labs(title = "Infraestrutura e Mobilidade")

###### Direitos e Inclusão Social: DIS ######

ips_tipos2016_long$direito = inner_join(ips_long, ind_tipo, by = "variavel") |> 
  filter(tipo == "DIS" & ano == 2016) |> select(-tipo)

ips_tipos2016$direito = ips_tipos2016_long$direito  |> 
  tidyr::spread(key = variavel, value = valor) |> 
  select(-ano, -regiao_administrativa)


fviz_cluster(list(data = ips_tipos2016$direito, cluster = clusters_ips), 
             geom = "point", 
             ellipse.type = "convex", 
             palette = "jco", 
             ggtheme = theme_minimal()) + 
  geom_text(aes(label = regioes), vjust = -0.5, hjust = 0.5)+
  labs(title = "Direitos e Inclusão Social")

#####

ips_tipos2016_long$saude$cluster = factor(clusters_ips)
ips_tipos2016_long$saneamento$cluster = factor(clusters_ips)
ips_tipos2016_long$seguranca$cluster = factor(clusters_ips)
ips_tipos2016_long$educacao$cluster = factor(clusters_ips)
ips_tipos2016_long$infra$cluster = factor(clusters_ips)
ips_tipos2016_long$direito$cluster = factor(clusters_ips)


ggplot(ips_tipos2016_long$saude, aes(x = cluster, y = valor, group = interaction(clusters_ips, variavel), color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores de saude 2016",
       x = "Cluster", y = "Valor")

ggplot(ips_tipos2016_long$saneamento, aes(x = cluster, y = valor, group = interaction(clusters_ips, variavel), color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores de saneamento 2016",
       x = "Cluster", y = "Valor")

ggplot(ips_tipos2016_long$seguranca, aes(x = cluster, y = valor, group = interaction(clusters_ips, variavel), color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores de seguranca 2016",
       x = "Cluster", y = "Valor")

ggplot(ips_tipos2016_long$educacao, aes(x = cluster, y = valor, group = interaction(clusters_ips, variavel), color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores de educacao 2016",
       x = "Cluster", y = "Valor")

ggplot(ips_tipos2016_long$infra, aes(x = cluster, y = valor, group = interaction(clusters_ips, variavel), color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores de infra 2016",
       x = "Cluster", y = "Valor")

ggplot(ips_tipos2016_long$direito, aes(x = cluster, y = valor, group = interaction(clusters_ips, variavel), color = cluster)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variavel, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Indicadores de direito 2016",
       x = "Cluster", y = "Valor")
 
