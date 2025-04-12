library(PNADcIBGE)
library(survey)
library(ggplot2)
library(dplyr)
library(srvyr)
library(tidyr)
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

##PRÓXIMOS PASSOS:
##1. REFAZER RENOMEANDO TODAS ESSES TROÇOS PRA PRIMEIRA VARIÁVEL DESEJADA
##2. FAZER AS PRÓXIMAS VARIÁVEIS SEU IMBECIL DE MERDA
##3. TRANSFORMAR EM % DIVDINDO PELA FORÇA DE TRABALHO [QUE SERÁ A SOMA DE TODAS ELAS]
##4. SE OS VALORES FICAREM OKAY, ENTÃO REFAÇA DE NOVO PARA O ANO DE 2024.3
##5. SE DELICIAR EM SUA VITÓRIA ABSOLUTA E CURTIR SÃO PAULO TRANQUILAMENTE


# Encadeando as 16 variáveis ----------------------------------------------

dados2019.3 <- as_survey_rep(dadosPNADc19.3) # Cria uma réplica do objeto survey

dados2019.3 <- update(dados2019.3,
                      CONDLAB = ifelse(
                        VD4001 == 1 & VD4002 == 1 & 
                          ((VD4009 == "09" & VD4010 == "01") | (VD4009 == "10" & VD4010 == "01")), 1, # 1 Conta própria/familiar na agricultura
                        ifelse(
                          VD4001 == 1 & VD4002 == 1 &
                            VD4009 == "02" & VD4010 == "01", 2, # 2 Assalariado sem carteira na agricultura
                          ifelse(
                            VD4001 == 1 & VD4002 == 1 &
                              VD4009 == "01" & VD4011 %in% c(1, 2, 3), 3, # 3 Assalariado com carteira colarinho branco superior
                            ifelse(
                              VD4001 == 1 & VD4002 == 1 &
                                VD4009 == "01" & VD4011 %in% c(4, 5), 4, # 4 Assalariado com carteira colarinho branco inferior / serviços / comércio
                              ifelse(
                                VD4001 == 1 & VD4002 == 1 &
                                  VD4009 == "01" & VD4011 %in% c(6, 7, 8), 5, # 5 Assalariado com carteira qualificado / semi-qualificado
                                ifelse(
                                  VD4001 == 1 & VD4002 == 1 &
                                    VD4009 == "01" & VD4011 == 9, 6, # 6 Assalariado com carteira não qualificado
                                  ifelse(
                                    VD4001 == 1 & VD4002 == 1 &
                                      VD4009 == "02" & V4018 == 1 & VD4010 != "01", 7, # 7 Sem carteira, pequenas empresas, fora agricultura
                                    ifelse(
                                      VD4001 == 1 & VD4002 == 1 &
                                        VD4009 == "02" & V4018 %in% c(2, 3, 4) & VD4010 != "01", 8, # 8 Sem carteira, médias/grandes empresas, fora agricultura
                                      ifelse(
                                        VD4001 == 1 & VD4002 == 1 &
                                          VD4009 %in% c("03", "04"), 9, # 9 Trabalhador doméstico
                                        ifelse(
                                          VD4001 == 1 & VD4002 == 1 &
                                            VD4009 == "06", 10, # 10 Setor público sem carteira
                                          ifelse(
                                            VD4001 == 1 & VD4002 == 1 &
                                              (VD4009 %in% c("05", "07") | (VD4009 == "01" & VD4011 == 10)), 11, # 11 Setor público com carteira / militares
                                            ifelse(
                                              VD4001 == 1 & VD4002 == 1 &
                                                VD4009 == "08" & V4019 == 1, 12, # 12 Empregador formal com CNPJ
                                              ifelse(
                                                VD4001 == 1 & VD4002 == 1 &
                                                  VD4009 == "09" & VD4012 == 1 & VD4010 != "01", 13, # 13 Autônomo formal fora da agricultura
                                                ifelse(
                                                  VD4001 == 1 & VD4002 == 1 &
                                                    ((VD4009 == "08" & V4019 == 2 & VD4016 >= 998) |
                                                       (VD4009 == "09" & VD4010 > "01" & VD4012 == 2 & VD4016 >= 998)), 14, # 14 Informal “produtivo” fora da agricultura
                                                  ifelse(
                                                    VD4001 == 1 & VD4002 == 1 &
                                                      ((VD4009 == "08" & V4019 == 2 & VD4016 < 998) |
                                                         (VD4009 == "09" & VD4010 > "01" & VD4012 == 2 & VD4016 < 998)), 15, # 15 Informal marginal fora da agricultura
                                                    ifelse(
                                                      VD4001 == 1 & VD4002 == 1 &
                                                        VD4009 == "10" & VD4010 > "01", 16, # 16 Familiar auxiliar fora da agricultura
                                                      NA_integer_ # caso não atenda a nenhum critério
                                                    )))))))))))))))))
)


all_levels <- as.character(1:16)

svytable(~CONDLAB, dados2019.3)
svytable(~is.na(CONDLAB), dados2019.3)

Brazil_flat <- svytable(~factor(CONDLAB, levels = all_levels), dados2019.3)# Brasil inteiro

ufs_nordeste <- c("21", "22", "23", "24", "25", "26", "27", "28", "29") # Nordeste
dados_nordeste <- subset(dados2019.3, UF %in% ufs_nordeste)
Nordeste_flat <- svytable(~factor(CONDLAB, levels = all_levels), dados_nordeste)

dados_sem_nordeste <- subset(dados2019.3, !(UF %in% ufs_nordeste)) # Brasil sem nordeste
NoNordeste_flat <- svytable(~factor(CONDLAB, levels = all_levels), dados_sem_nordeste)

Brazil_prop <- prop.table(Brazil_flat)
Nordeste_prop <- prop.table(Nordeste_flat)
NoNordeste_prop <- prop.table(NoNordeste_flat)

sum(Brazil_prop)
sum(Nordeste_prop)
sum(NoNordeste_prop)

print(Brazil_prop)
print(Nordeste_prop)
print(NoNordeste_prop)

df <- dados2019.3$variables
table(df$CONDLAB)
table(is.na(df$CONDLAB))


# de onda em onda, variável a variável -----------------------------------------


# Começando tudo como NA~
df$CONDLAB <- NA_integer_
# 0 = Não-empregado (desempregado ou fora da força de trabalho)
df$CONDLAB[df$VD4001 == 2 | df$VD4002 == 2] <- 0  # Novo código para não-empregados
#o Sol brilha, as pessoas trabalham:
sol <- df$VD4001 == 1 & df$VD4002 == 1
#onda 1: Self Employed/family work in agriculture
onda1 <- (df$VD4009 %in% c(9, 10)) & df$VD4010 == "01"
#aplicando a condição do Sol~
df$CONDLAB[sol & onda1] <- 1
table(df$CONDLAB, useNA = "ifany")
table(sol)
table(onda1)

#Onda 2: Wage work without carteira in agriculture
df$onda2_flag <- (df$VD4009 == 2) & df$VD4010 == "01"
#aplicar ao CONDLAB
df$CONDLAB[sol & df$onda2_flag] <- 2
#Confirmação::
table(df$onda2_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 3: Private sector wage worker with carteira, upper white-collar
VD4009 == 1 & VD4011 %in% c(1, 2, 3)
df$onda3_flag <- (df$VD4009 == "01") & (df$VD4011 %in% c("01", "02", "03"))
df$CONDLAB[sol & df$onda3_flag] <- 3
#Confirmação::
table(df$onda3_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 4: Private sector with carteira, lower white-collar/services/trade
df$onda4_flag <- (df$VD4009 == "01") & (df$VD4011 %in% c("04", "05"))
df$CONDLAB[sol & df$onda4_flag] <- 4
#Confirmação::
table(df$onda4_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 5: Private sector with carteira, blue-collar skilled/semi-skilled
df$onda5_flag <- (df$VD4009 == "01") & (df$VD4011 %in% c("06", "07", "08"))
df$CONDLAB[sol & df$onda5_flag] <- 5
#Confirmação::
table(df$onda2_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")


#Onda 6: Private sector with carteira, blue-collar, unskilled
df$onda6_flag <- (df$VD4009 == "01") & (df$VD4011 == "09")
df$CONDLAB[sol & df$onda6_flag] <- 6
#Confirmação::
table(df$onda6_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 7: Private sector without carteira, small enterprise [up to 5 workers]
df$onda7_flag <- (df$VD4009 == "02") & (df$V4018 == 1) & (df$VD4010 != "01")
df$CONDLAB[sol & df$onda7_flag] <- 7
#Confirmação::
table(df$onda7_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 8: Private sector without carteira, medium/large enterprise[, non-agro[>5 workers]
df$onda8_flag <- (df$VD4009 == "02") & (df$V4018 %in% c(2, 3, 4)) & (df$VD4010 != "01")
df$CONDLAB[sol & df$onda8_flag] <- 8
#Confirmação::
table(df$onda8_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 9: Domestic workers (with or without carteira)
df$onda9_flag <- df$VD4009 %in% c("03", "04")
df$CONDLAB[sol & df$onda9_flag] <- 9
#Confirmação::
table(df$onda9_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 10: Public sector workers without carteira
df$onda10_flag <- df$VD4009 == "06"
df$CONDLAB[sol & df$onda10_flag] <- 10
#Confirmação::
table(df$onda10_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 11: Public sector with carteira / military / civil servant
df$onda11_flag <- df$VD4009 == "05" |
  df$VD4009 == "07" |
  (df$VD4009 == "01" & df$VD4011 == "10")
df$CONDLAB[sol & df$onda11_flag] <- 11
#Confirmação::
table(df$onda11_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 12: Formal employer (with CNPJ registration)
df$onda12_flag <- df$VD4009 == "08" & df$V4019 == "1"
df$CONDLAB[sol & df$onda12_flag] <- 12
#Confirmação::
table(df$onda12_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 13: Formal non-agricultural self-employed work (contributing to social security)
df$onda13_flag <- df$VD4009 == "09" & df$VD4012 == "1" & df$VD4010 != "01"
df$CONDLAB[sol & df$onda13_flag] <- 13
#Confirmação::
table(df$onda13_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 14: Informal "productive" non-agricultural self-employed work or employer
df$onda14_flag <- (df$VD4009 == "08" & df$V4019 == "2" & df$VD4016 >= 998) |
  (df$VD4009 == "09" & df$VD4010 > "01" & df$VD4012 == "2" & df$VD4016 >= 998)
df$CONDLAB[sol & df$onda14_flag] <- 14
# Confirmação::
table(df$onda14_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 15: Informal "marginal" non-agricultural self-employed work or employer
df$onda15_flag <- (df$VD4009 == "08" & df$V4019 == "2" & df$VD4016 < 998) |
  (df$VD4009 == "09" & df$VD4010 > "01" & df$VD4012 == "2" & df$VD4016 < 998)
df$CONDLAB[sol & df$onda15_flag] <- 15
#Confirmação
table(df$onda15_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

#Onda 16: Non-agricultural family work
df$onda16_flag <- (df$VD4009 == "10" & df$VD4010 > "01")
df$CONDLAB[sol & df$onda16_flag] <- 16
#Confirmação
table(df$onda16_flag, useNA = "ifany")
table(df$CONDLAB, useNA = "ifany")

# Checar se todos os empregados (sol) foram classificados
unclassified_employed <- sum(sol & is.na(df$CONDLAB), na.rm = T)
if(unclassified_employed > 0) warning(paste(unclassified_employed, "casos empregados não classificados!"))