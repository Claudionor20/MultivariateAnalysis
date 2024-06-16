# Alguns códigos para organização
# Essa metodologia de diretório é importante para manter a organização das pastas, e além disso,
# Facilita muito se uma pessoa quiser rodar o código de outro computador, já que o código já aponta os caminhos automaticamente

diretorio <- getwd() # Acessar o diretorio atual de trabalho

caminho <- file.path(diretorio, "database") # Acessando a pasta "database" do diretório


# Exemplo de leitura e salvamento de arquivo em outra pasta
library(readr)

arquivo <- file.path(caminho, "teste.csv")
df <- read_csv(arquivo)  # Lendo uma base de dados dentro da pasta database


salvo <- file.path(caminho, "teste1.csv")
write.csv(df, salvo , row.names = F) # Salvando o csv na pasta database

dasdsa


