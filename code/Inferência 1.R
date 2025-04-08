library(PNADcIBGE)
library(survey)
library(ggplot2)
#library(gtsummary)
#library(tidyverse)
#library(flextable)

# TESTE 1 = TOTAL SEXO 2023 -----------------------------------------------
#carregar os microdados
dadosPNADc23.1 <- get_pnadc(year = 2023, quarter = 1)
dadosPNADc23.2 <- get_pnadc(year = 2023, quarter = 2)
dadosPNADc23.3 <- get_pnadc(year = 2023, quarter = 3)
dadosPNADc23.4 <- get_pnadc(year = 2023, quarter = 4)
dadosPNADc22.1 <- get_pnadc(year = 2022, quarter = 1)
dadosPNADc22.2 <- get_pnadc(year = 2022, quarter = 2)
dadosPNADc22.3 <- get_pnadc(year = 2022, quarter = 3)
dadosPNADc22.4 <- get_pnadc(year = 2022, quarter = 4)

sexo23.1 <- svytotal(x=~V2007, design = dadosPNADc23.1, na.rm = T)#estimativa do total da variável sexo
sexo23.2 <- svytotal(x=~V2007, design = dadosPNADc23.2, na.rm = T)
sexo23.3 <- svytotal(x=~V2007, design = dadosPNADc23.3, na.rm = T)
sexo23.4 <- svytotal(x=~V2007, design = dadosPNADc23.4, na.rm = T)
sexo22.1 <- svytotal(x=~V2007, design = dadosPNADc22.1, na.rm = T)
sexo22.2 <- svytotal(x=~V2007, design = dadosPNADc22.2, na.rm = T)
sexo22.3 <- svytotal(x=~V2007, design = dadosPNADc22.3, na.rm = T)
sexo22.4 <- svytotal(x=~V2007, design = dadosPNADc22.4, na.rm = T)

sexo2023totalA <- c(sexo23.1, sexo23.2, sexo23.3, sexo23.4, sexo22.1, sexo22.2, sexo22.3, sexo22.4) #criação de vetor com os resultados
resultado_sexo <- data.frame( #transformando em dataframe
  Trimestre = c(1, 2, 3, 4), #trimestres
  Masculino = c(coef(sexo23.1)[1], coef(sexo23.2)[1], coef(sexo23.3)[1], coef(sexo23.4)[1], coef(sexo22.1)[1], coef(sexo22.2)[1], coef(sexo22.3)[1], coef(sexo22.4)[1]), #total masc
  Feminino = c(coef(sexo23.1)[2], coef(sexo23.2)[2], coef(sexo23.3)[2], coef(sexo23.4)[2], coef(sexo22.1)[2], coef(sexo22.2)[2], coef(sexo22.3)[2], coef(sexo22.4)[2]) #total fem
)
resultado_sexo$Total <- resultado_sexo$Masculino + resultado_sexo$Feminino #adicionar coluna de total no database

add_aggregates <- function(design, var) {  #função para processar cada metadado e depois adicionar em um banco agregado
  design$variables$total_masc <- coef(svytotal(~V2007, design))[1]
  design$variables$total_fem <- coef(svytotal(~V2007, design))[2]
  design$variables$total <- coef(svytotal(~V2007, design))[1] +
    coef(svytotal(~V2007, design))[2]
  return(design)
}

survey_designs <- list(
  dadosPNADc23.1, dadosPNADc23.2, dadosPNADc23.3, dadosPNADc23.4, #lista com os metadados a serem trabalhados
  dadosPNADc22.1, dadosPNADc22.2, dadosPNADc22.3, dadosPNADc22.4
)

survey_designs <- lapply(survey_designs, function(design) add_aggregates(design)) #aplicar a função na lsita

svyboxplot(formula=VD4035~1, design=survey_designs, main="Boxplot do Número de Horas Trabalhadas") #boxplot

svyhist(formula = ~as.numeric(V2007), design = survey_designs, freq = T, main = "Histograma", xlab = "População por sexo") #histograma

resultado_sexo$Tempo <- factor( #aplicar os metadados a uma lista sem mudar seu formato
  c("Q1/2023", "Q2/2023", "Q3/2023", "Q4/2023",
  "Q1/2022", "Q2/2022", "Q3/2022", "Q4/2022"),
  levels = c("Q1/2022", "Q2/2022", "Q3/2022", "Q4/2022", #aplicar a ordem de progressão trimestral
             "Q1/2023", "Q2/2023", "Q3/2023", "Q4/2023"),
  ordered = T
)

ggplot(resultado_sexo, aes(x = Tempo, y = Total, group = 1)) + #gráfico de linha pra análise temporal~
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  theme_minimal() +
  labs(
    title = "População por tempo",
    x = "Período (em trimestres de ano)",
    y = "População total"
  )

