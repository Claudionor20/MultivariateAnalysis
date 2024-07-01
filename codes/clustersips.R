#### Agrupamentos hierarquicos ####

#####Agrupando utilizando todas as variáveis #####

ips_c = filter(ips, ano == 2016)
ips_c = ips_c[-c(1,2,3)]
ips_c = scale(ips_c)
row.names(ips_c) = ips$regiao_administrativa[ips$ano == 2016]

dist_ipsc = dist(ips_c)

cluster_ips = hclust(dist_ipsc, method = "single")
par(mfrow = c(1,1))
plot(cluster_ips, main = "single")

cluster_ips2 = hclust(dist_ipsc, method = "complete")

plot(cluster_ips2, main = "complete")

cluster_ips3 = hclust(dist_ipsc, method = "average")

plot(cluster_ips3, main = "avg")


##### Agrupando por tipo de indicador #####
tipo = c("SAU", "SAU", "SAU", "SAU", "SAN", "SAN", "SAN", "DIS", "INF", "DIS", 
         "SEG", "SEG", "EDU", "EDU", "EDU", "EDU", "INF", "INF", "SAU", "SAU", 
         "SAU", "SAN", "MEI", "INF", "SEG", "INF", "DIS", "SAU", "DIS", "DIS", 
         "SEG", "SEG", "DIS", "EDU", "EDU", "EDU")

ind_tipo = data.frame(variavel = names(ips)[-c(1,2,3)], tipo = tipo)
######SAUDE ######
ips_saude = inner_join(ips_long, ind_tipo, by = "variavel")

ips_saude = filter(ips_saude, ano == 2016)
ips_saude = filter(ips_saude, tipo == "SAU")
ips_saude = ips_saude[,-5]

ips_saude_reorg = ips_saude |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_saude) = ips$regiao_administrativa[ips$ano == 2016]

ips_saudeM = scale(ips_saude_reorg[,-c(1,2)])
cluster_saude = hclust(dist(ips_saudeM), method = "single")
cluster_saude2 = hclust(dist(ips_saudeM), method = "complete")
cluster_saude3 = hclust(dist(ips_saudeM), method = "average")

plot(cluster_saude, labels = ips_saude_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_saude2, labels = ips_saude_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_saude3, labels = ips_saude_reorg$regiao_administrativa, main = "Método Average")

###### Saneamento Básico: SAN ######

ips_saneamento = inner_join(ips_long, ind_tipo, by = "variavel")

ips_saneamento = filter(ips_saneamento, ano == 2016)
ips_saneamento = filter(ips_saneamento, tipo == "SAN")
ips_saneamento = ips_saneamento[,-5]

ips_saneamento_reorg = ips_saneamento |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_saneamento) = ips$regiao_administrativa[ips$ano == 2016]

ips_saneamentoM = scale(ips_saneamento_reorg[,-c(1,2)])
cluster_saneamento = hclust(dist(ips_saneamentoM), method = "single")
cluster_saneamento2 = hclust(dist(ips_saneamentoM), method = "complete")
cluster_saneamento3 = hclust(dist(ips_saneamentoM), method = "average")

plot(cluster_saneamento, labels = ips_saneamento_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_saneamento2, labels = ips_saneamento_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_saneamento3, labels = ips_saneamento_reorg$regiao_administrativa, main = "Método Average")

###### Segurança: SEG ######

ips_seguranca = inner_join(ips_long, ind_tipo, by = "variavel")

ips_seguranca = filter(ips_seguranca, ano == 2016)
ips_seguranca = filter(ips_seguranca, tipo == "SEG")
ips_seguranca = ips_seguranca[,-5]

ips_seguranca_reorg = ips_seguranca |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_seguranca) = ips$regiao_administrativa[ips$ano == 2016]

ips_segurancaM = scale(ips_seguranca_reorg[,-c(1,2)])
cluster_seguranca = hclust(dist(ips_segurancaM), method = "single")
cluster_seguranca2 = hclust(dist(ips_segurancaM), method = "complete")
cluster_seguranca3 = hclust(dist(ips_segurancaM), method = "average")

plot(cluster_seguranca, labels = ips_seguranca_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_seguranca2, labels = ips_seguranca_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_seguranca3, labels = ips_seguranca_reorg$regiao_administrativa, main = "Método Average")

###### Educação: EDU ######

ips_educacao = inner_join(ips_long, ind_tipo, by = "variavel")

ips_educacao = filter(ips_educacao, ano == 2016)
ips_educacao = filter(ips_educacao, tipo == "EDU")
ips_educacao = ips_educacao[,-5]

ips_educacao_reorg = ips_educacao |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_educacao) = ips$regiao_administrativa[ips$ano == 2016]

ips_educacaoM = scale(ips_educacao_reorg[,-c(1,2)])
cluster_educacao = hclust(dist(ips_educacaoM), method = "single")
cluster_educacao2 = hclust(dist(ips_educacaoM), method = "complete")
cluster_educacao3 = hclust(dist(ips_educacaoM), method = "average")

plot(cluster_educacao, labels = ips_educacao_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_educacao2, labels = ips_educacao_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_educacao3, labels = ips_educacao_reorg$regiao_administrativa, main = "Método Average")

###### Infraestrutura e Mobilidade: INF######

ips_infra = inner_join(ips_long, ind_tipo, by = "variavel")

ips_infra = filter(ips_infra, ano == 2016)
ips_infra = filter(ips_infra, tipo == "INF")
ips_infra = ips_infra[,-5]

ips_infra_reorg = ips_infra |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_infra) = ips$regiao_administrativa[ips$ano == 2016]

ips_infraM = scale(ips_infra_reorg[,-c(1,2)])
cluster_infra = hclust(dist(ips_infraM), method = "single")
cluster_infra2 = hclust(dist(ips_infraM), method = "complete")
cluster_infra3 = hclust(dist(ips_infraM), method = "average")

plot(cluster_infra, labels = ips_infra_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_infra2, labels = ips_infra_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_infra3, labels = ips_infra_reorg$regiao_administrativa, main = "Método Average")

###### Meio Ambiente: MEI ######

ips_mei = inner_join(ips_long, ind_tipo, by = "variavel")

ips_mei = filter(ips_mei, ano == 2016)
ips_mei = filter(ips_mei, tipo == "MEI")
ips_mei = ips_mei[,-5]

ips_mei_reorg = ips_mei |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_mei) = ips$regiao_administrativa[ips$ano == 2016]

ips_meiM = scale(ips_mei_reorg[,-c(1,2)])
cluster_mei = hclust(dist(ips_meiM), method = "single")
cluster_mei2 = hclust(dist(ips_meiM), method = "complete")
cluster_mei3 = hclust(dist(ips_meiM), method = "average")

plot(cluster_mei, labels = ips_mei_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_mei2, labels = ips_mei_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_mei3, labels = ips_mei_reorg$regiao_administrativa, main = "Método Average")

###### Direitos e Inclusão Social: DIS ######

ips_direito = inner_join(ips_long, ind_tipo, by = "variavel")

ips_direito = filter(ips_direito, ano == 2016)
ips_direito = filter(ips_direito, tipo == "DIS")
ips_direito = ips_direito[,-5]

ips_direito_reorg = ips_direito |> 
  tidyr::spread(key = variavel, value = valor)

#row.names(ips_direito) = ips$regiao_administrativa[ips$ano == 2016]

ips_direitoM = scale(ips_direito_reorg[,-c(1,2)])
cluster_direito = hclust(dist(ips_direitoM), method = "single")
cluster_direito2 = hclust(dist(ips_direitoM), method = "complete")
cluster_direito3 = hclust(dist(ips_direitoM), method = "average")

plot(cluster_direito, labels = ips_direito_reorg$regiao_administrativa, main = "Método Single")
plot(cluster_direito2, labels = ips_direito_reorg$regiao_administrativa, main = "Método Complete")
plot(cluster_direito3, labels = ips_direito_reorg$regiao_administrativa, main = "Método Average")


par(mfrow = c(2,1))
plot(cluster_saude, labels = ips_saude_reorg$regiao_administrativa, main = "Método Single SAUDE", xlab = "", sub = "")
plot(cluster_saneamento, labels = ips_saneamento_reorg$regiao_administrativa, main = "Método Single SANEAMENTO", xlab = "", sub = "")
plot(cluster_seguranca, labels = ips_seguranca_reorg$regiao_administrativa, main = "Método Single SEGURANCA", xlab = "", sub = "")
plot(cluster_educacao, labels = ips_educacao_reorg$regiao_administrativa, main = "Método Single EDUCACAO " , xlab = "", sub = "")
plot(cluster_infra, labels = ips_infra_reorg$regiao_administrativa, main = "Método Single INFRAESTRUTURA", xlab = "", sub = "")
plot(cluster_direito, labels = ips_direito_reorg$regiao_administrativa, main = "Método Single DIREITO", xlab = "", sub = "")

par(mfrow = c(2,1))
plot(cluster_saude2, labels = ips_saude_reorg$regiao_administrativa, main = "Método Single SAUDE", xlab = "", sub = "")
plot(cluster_saneamento2, labels = ips_saneamento_reorg$regiao_administrativa, main = "Método Single SANEAMENTO", xlab = "", sub = "")
plot(cluster_seguranca2, labels = ips_seguranca_reorg$regiao_administrativa, main = "Método Single SEGURANCA", xlab = "", sub = "")
plot(cluster_educacao2, labels = ips_educacao_reorg$regiao_administrativa, main = "Método Single EDUCACAO " , xlab = "", sub = "")
plot(cluster_infra2, labels = ips_infra_reorg$regiao_administrativa, main = "Método Single INFRAESTRUTURA", xlab = "", sub = "")
plot(cluster_direito2, labels = ips_direito_reorg$regiao_administrativa, main = "Método Single DIREITO", xlab = "", sub = "")

#####

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


