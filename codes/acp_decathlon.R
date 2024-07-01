#---- Librarias para ACP #----

library(FactoMineR)
library(factoextra)
library(corrplot)
library(moments)

#---- Carregando os dados decathlon2 #----
data("decathlon2")

#---- Dados para análise (10 vars ativas + 2 vars suplementares) #----
# removendo os 4 ultimos individuos suplementares e a ultima coluna suplementar qualitativa
dados_decathlon <- decathlon2[1:23,-13] 


# Faça uma analise descritiva do conjunto dados 
# considerando somente as variáveis ativas 

dados <- dados_decathlon[,1:10] # dados somente com as variáveis ativas

#---- Medidas resumo #----

media = apply(dados,MARGIN = 2,FUN = mean)
variancia = apply(dados,MARGIN = 2,FUN = var)
DP = apply(dados,MARGIN = 2,FUN = sd)
CV = DP/media * 100
minimo = apply(dados,MARGIN = 2,FUN = min)
mediana = apply(dados,MARGIN = 2,FUN = median)
maximo = apply(dados,MARGIN = 2,FUN = max)
assimetria = apply(dados,MARGIN = 2,FUN = skewness)
curtose = apply(dados,MARGIN = 2,FUN = kurtosis)

#---- Tabela medidas resumo #----
resumo <- round(rbind(media,variancia,DP,CV,minimo,mediana,maximo,assimetria,curtose),2)
View(resumo)

#---- Matriz de covariâncias #----
S <- cov(dados)
S

#---- Matriz e gráfico de correlações #----
R <- cor(dados_decathlon[,1:10])
R

corrplot(R, method = "square")

# Você faria um ACP não normado (na matriz S) ou um ACP normado (na matriz R)? Justifique


#---- ACP nos dados de decathlon (10 vars ativas + 2 vars suplementares) #----
acp_out <- PCA(dados_decathlon, scale.unit = TRUE, ncp = 10, 
               quanti.sup = c(11:12), graph = FALSE)

#---- Resultados para as variáveis #----
var_out <- get_pca_var(acp_out)

#---- Matriz de cargas (loadings) #----

loadings <- var_out$coord
loadings

# escalando pelo desvio padrão dos componentes para comparar c/ os 
# resultados vistos em sala
pesos_coord <- 1/sqrt(acp_out$eig[,1]) 
p <- length(pesos_coord)
m.cargas <- matrix(0,p,p)
for (i in 1:p) {
  m.cargas[,i] <- pesos_coord[i]*var_out$coord[,i]
}

# Questão 1)
# (a) Qual variável contribui mais à formação do primeiro componente principal?
# (b) Qual variável contribui menos à formação do primeiro componente principal?
# (c) Mesmas perguntas em (a) e (b) para o segundo componente principal?


#---- Escores: matriz Z #----
m.escores <- as.matrix(dados_decathlon[,1:10])%*%loadings
View(m.escores)

# Questão 2)
# (a) Interprete o primeiro, segundo e terceiro componente principal.
# (b) Faça um rankeamento dos atletas com base no primeiro componente principal? 
# (c) Faça um rankeamento dos atletas com base no segundo componente principal?

#---- Variância explicada por componente #----
acp_out$eig

#---- Screeplot #----
fviz_eig(acp_out, addlabels = TRUE, ylim = c(0, 50),main = "")

# Questão 3)
# (a) Qual a proporção da variância explicada pelo primeiro componente principal?
# (b) Quantos componentes principais você reteria? justifique 


#---- Matriz de rotação com Circulo de correlações #---- 
# plotando as variávesis no plano PC1-PC2
fviz_pca_var(acp_out, axes = c(1,2),col.var = "black",repel = TRUE) 

# Questão 4)
# (a) O que você pode dizer da variável X1500m em relação à contribuição ao segundo componente?
# (b) Qual das duas seguintes dos variáveis está melhor representada no plano PC1-PC2: Javeline ou Pole.vault?
# (c) Calcule as correlações entre a variável suplementar Rank e 
# as variáveis ativas, os valores são condizentes com o observado no gráfico ?
# (d) Refaça (c), porém, agora considerando a variável suplementar Points.

#---- Individuos no plano PC1-PC2 #----
fviz_pca_ind(acp_out, axes = c(1,2))

#---- Biplot #----
fviz_pca_biplot(acp_out,addlabels = TRUE)


# Bônus
# Consulte sobre a função PCA() do pacote MVAr.pt (em Português).
library(MVar.pt)

