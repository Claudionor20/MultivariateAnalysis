# Leitura da base de dados pelo github

ips_componentes = read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/ips_componentes1620.csv")
ips_componentes = ips_componentes[,-1]

ips_indicadores = read.csv(file = "https://raw.githubusercontent.com/Claudionor20/MultivariateAnalysis/main/bases/ips_indicadores1620.csv")
ips_indicadores = ips_indicadores[,-1]

#
library(dplyr)
#### IPS INDICADORES

naniar::miss_var_summary(ips_indicadores) |> arrange(desc(n_miss)) #sem dados faltantes 

str(ips_indicadores)
