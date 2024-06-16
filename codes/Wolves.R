#### Pacotes ####

if (!require(candisc)) {
  install.packages("candisc")
  library(candisc)
}
library(candisc)
library(dplyr)
library(ggplot2)

####Base de dados  ####
data(Wolves)
str(Wolves)

Wolves = Wolves |> mutate(across(c(4:6), as.numeric))
str(Wolves)

##### Análise exploratória: ####

#

table(Wolves$sex)
table(Wolves$location)

Wolves |> ggplot(aes(x = group))+
  geom_bar(fill = "#5A639C", width = .5)+
  labs( y = "Frequência", x = "Grupo")+
  theme_minimal()


#Histogramas - quantitativas
par(mfrow = c(3,3))
str(Wolves)
for(i in 1:ncol(Wolves)){
  name = names(Wolves)[i]
  if(is.numeric(Wolves[[i]])){
    label = attr(Wolves[[i]], "label")
    main_title = ifelse(!is.null(label), label, name)
    
    hist(Wolves[[i]], xlab= "", main = main_title)
  }
}


#Boxplot por grupo

plots <- list()

for (i in 1:ncol(Wolves)) {
  var_name <- names(Wolves)[i]
  
  if (is.numeric(Wolves[[var_name]])) {
    label <- attr(Wolves[[var_name]], "label")
    
    main_title <- ifelse(!is.null(label), label, var_name)
  
    p <- ggplot(data = Wolves, aes_string(x = "group", y = var_name)) +
      geom_boxplot() +
      labs(title = main_title, x = "", y = "") +
      theme_minimal()
    
    plots[[length(plots) + 1]] <- p
  }
}
library(gridExtra)
do.call("grid.arrange", c(plots, ncol = 3))

# Análise de correlação

par(mfrow = c(1,1))
pairs(Wolves[,4:12], 
      pch=21)

corrplot::corrplot(cor(Wolves[,4:12]), method = "square" , order = "hclust", tl.col='black', tl.cex=.75) 

cor(Wolves[,4:12])






