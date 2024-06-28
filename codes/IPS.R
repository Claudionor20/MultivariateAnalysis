




# Leitura da base de dados pelo github

ips= read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/IPS.csv", sep = ';')


# Importando pacotes

library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(caret)
library(tidyr)
library(sf)
######### ANÁLISE DESCRITIVA #########

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
dados_ips_2020 <- ips[ips$ano == 2020, ]
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

ggplot() +
  geom_sf(data = regioes_ips_2020, aes(fill = ips_geral)) +
  scale_fill_gradient(low = "white", high = "red", name = "IPS Geral") +
  theme_minimal() +
  labs(title = "Mapa de Calor do IPS Geral por Região Administrativa (2020)")



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
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Remover a coluna das espécies, pois é uma variável categórica
IPS_d <- ips[, c(-1,-2)]

# Executar a PCA
pca_result <- PCA(IPS_d, scale.unit = TRUE, ncp=36, quanti.sup=c(1) ,graph = FALSE)
pca_result$eig #81.85067% com 10 componentes # Claudio'nor: A gente teria que explicar quais variaveis explicam esses 10 compontentes? Se sim, acho muito difícil

var_out <- get_pca_var(pca_result)

loadings <- var_out$coord
View(loadings)

row.names(loadings)

#O primeiro Componente é a diferença entre qualidade, regiões com scores positivos tem maior qualidade de vida já os negativos pior qualidade, ou seja quanto maior score melhor qualidade de vida, quando menor o socre pior qualidade 
#Em suma o primeiro pca verifica as regios quem possuem mais recursos #Claudio'nor: Concordo

#o segundo representa um componente que relaciona segurança, criminalidade, liberdades individuais e acesso a serviços urbanos ( porra não consegui interpretar melhor n ) #Claudio'nor: Acho que só criminalidade já é bem explicativo
#o terceiro representa o acesso ao saneamento basico
#o quarto representa a vulnerabilidade social e direitos individuais????????????????????? #Claudio'nor: Eu achei que esse componente tem uma pegada de avaliar a vida urbana, já que ele tem variáveis de mobilidade urbana, crise respiratório (pode ser associado ao ambiente urbano),
                                                                                          # população vivendo em favela não urbanizada, etcc                                              
#o quinto representa saúde e qualidade ambiental??????????



fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50),main = "") #gráficos

fviz_pca_var(pca_result, axes = c(1,2),col.var = "black",repel = TRUE)  # Claudio'nor: Apesar de parecer uma teia de aranha, esse gráfico com a variável ips_geral destacada, mostra que ela é altamente influenciada
                                                                        # pelo primeiro componente, legal que são as mesma variaveis que tem maior correlação com ela, como foi visto na análise descritiva

fviz_pca_ind(pca_result, axes = c(1,2)) #

fviz_pca_biplot(pca_result,addlabels = TRUE) #Claudio'nor: Aqui se pá tem que escolher 1 ano e ver como a região se comporta em relação aos componentes
                                        # Se for interessante, cabe 3 gráficos e análisar a evolução (Bom que da prá ver a mudança da variável mais impactante em um estado ao longo do tempo)











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






