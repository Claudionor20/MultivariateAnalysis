# Leitura da base de dados pelo github

ips= read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/IPS.csv", sep = ';')


# Importando pacotes

library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)

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
IPS_d <- ips[, c(-1,-2,-3)]

# Padronizar as variáveis
ips_pad <- scale(IPS_d)

# Executar a PCA
pca_result <- PCA(ips_pad, graph = FALSE)
pca_result$eig #81.85067% com 10 componentes

var_out <- get_pca_var(pca_result)

loadings <- var_out$coord
View(loadings)

#O primeiro Componente é a diferença entre qualidade, regiões com scores positivos tem maior qualidade de vida já os negativos pior qualidade, ou seja quanto maior score melhor qualidade de vida, quando menor o socre pior qualidade
#Em suma o primeiro pca verifica as regios quem possuem mais recursos
#o segundo representa um componente que relaciona segurança, criminalidade, liberdades individuais e acesso a serviços urbanos ( porra não consegui interpretar melhor n )
#o terceiro representa o acesso ao saneamento basico
#o quarto representa a vulnerabilidade social e direitos individuais?????????????????????
#o quinto representa saúde e qualidade ambiental??????????

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50),main = "") #gráficos

fviz_pca_var(pca_result, axes = c(1,2),col.var = "black",repel = TRUE) 

fviz_pca_ind(pca_result, axes = c(1,2))

fviz_pca_biplot(pca_result,addlabels = TRUE)
