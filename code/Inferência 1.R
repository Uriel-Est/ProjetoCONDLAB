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


# Estudo 2019.3 -----------------------------------------------------------


dadosPNADc19.3 <- get_pnadc(year = 2019, quarter = 3, labels = F)
#DO AGAIN BITCH
#VD4001 == 1 #força de trabalho
#VD4002 == 1 #trabalhando
#VD4009 == 9 or 10 #autônomo ou familiar
#VD4010 == 1 #agricultura

unique(dadosPNADc19.3$variables$VD4001)#checando as respostas nas variáveis desejadas
unique(dadosPNADc19.3$variables$VD4002)
unique(dadosPNADc19.3$variables$VD4009)
unique(dadosPNADc19.3$variables$VD4010)

dadosPNADc19.3 <- update( #atualiza o design amostral
  dadosPNADc19.3, #o design original
  CONDLAB = ifelse( #cria a nova variável com base nas condições
    VD4001 == "1" & VD4002 == "1" & 
      VD4009 %in% c("09", "10") & VD4010 == "01", 
    1, 0 #atribui 1 se as condições forem atendidas, senão 0
  )
)

table(dadosPNADc19.3$variables$CONDLAB) #verificar se a variável foi criada corretamente

dadosPNADc19.3 <- update( #dividir o Brasil entre três alternativas: Brasil, Nordeste, Brasil s/ Nordeste
  dadosPNADc19.3,
  Nordeste = ifelse(UF %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29"), 1, 0),
  RestoBrasil = ifelse(UF %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29"), 0, 1),
  Ambos = 1  #todas as observações do dataset caem sobre ambos~
)

design_nordeste <- subset(dadosPNADc19.3, Nordeste == 1) #subset define os designs
design_restoBrasil <- subset(dadosPNADc19.3, RestoBrasil == 1)
design_ambos <- dadosPNADc19.3  #não há a necessidade de filtros, visto que inclui tudo~

resultadoNord <- svytotal(~CONDLAB, design = design_nordeste) #realizar os cálculos de estimativa para cada uma das regiões de interesse
resultadoResto <- svytotal(~CONDLAB, design = design_restoBrasil)
resultadoAmbos <- svytotal(~CONDLAB, design = design_ambos)

totalNord <- resultadoNord[["CONDLAB"]]  #extrair resultado
totalResto <- resultadoResto[["CONDLAB"]]  #para o Brasil s/ nordeste
totalAmbos <- resultadoAmbos[["CONDLAB"]]  #para o Brasil inteiro

porcentagemNord <- (totalNord / totalAmbos) * 100
porcentagemResto <- (totalResto / totalAmbos) * 100
porcentagemAmbos <- (totalAmbos / totalAmbos) * 100

porcentagemNord <- round(porcentagemNord, digits = 1)
porcentagemResto <- round(porcentagemResto, digits = 1)