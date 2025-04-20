library(PNADcIBGE)
library(survey)
library(ggplot2)
library(dplyr)
library(srvyr)
library(tidyr)
library(knitr)

# Condições Laborais: -----------------------------------------------------------------
#Distribuição em % dos ocupados por condição laboral, para as três categorias regionais:

# Carregar os dados
dadosPNADc19.3 <- get_pnadc(year = 2019, quarter = 3, labels = FALSE)
gc()

# Criar o objeto survey design
pnad_design <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~round(V1028),
  data = dadosPNADc19.3$variables,
  nest = TRUE
)

# Atualizar variáveis como caracteres
pnad_design <- update(pnad_design,
                      VD4009 = as.character(VD4009),
                      VD4010 = as.character(VD4010),
                      VD4011 = as.character(VD4011),
                      VD4019 = as.character(VD4019)
)

# Criar a variável CONDLAB inicial como NA
pnad_design <- update(pnad_design, CONDLAB = NA)

# Criar vetor lógico: pessoas ocupadas (o Sol brilha)
pnad_design <- update(pnad_design, sol = VD4001 == 1 & VD4002 == 1)

# Onda 1: Conta própria/familiar na agricultura
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 %in% c("09", "10") & VD4010 == "01", 1, CONDLAB))


# Onda 2: Assalariado sem carteira na agricultura
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "02" & VD4010 == "01", 2, CONDLAB))

# Onda 3: Privado com carteira, colarinho branco superior
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "01" & VD4011 %in% c("01", "02", "03"), 3, CONDLAB))

# Onda 4: Privado com carteira, colarinho branco inferior
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "01" & VD4011 %in% c("04", "05"), 4, CONDLAB))

# Onda 5: Privado com carteira, qualificado/semiqualificado
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "01" & VD4011 %in% c("06", "07", "08"), 5, CONDLAB))

# Onda 6: Privado com carteira, não qualificado
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "01" & VD4011 == "09", 6, CONDLAB))

# Onda 7: Privado sem carteira, pequenas empresas
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "02" & V4018 == "1" & VD4010 != "01", 7, CONDLAB))

# Onda 8: Privado sem carteira, médias/grandes empresas
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "02" & V4018 %in% c("2", "3", "4") & VD4010 != "01", 8, CONDLAB))

# Onda 9: Trabalhador doméstico
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 %in% c("03", "04"), 9, CONDLAB))

# Onda 10: Setor público sem carteira
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "06", 10, CONDLAB))

# Onda 11: Setor público com carteira / militares
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & (VD4009 %in% c("05", "07") | (VD4009 == "01" & VD4011 == "10")), 11, CONDLAB))

# Onda 12: Empregador formal
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "08" & V4019 == "1", 12, CONDLAB))

# Onda 13: Autônomo formal
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "09" & VD4012 == "1" & VD4010 != "01", 13, CONDLAB))

# Onda 14: Informal produtivo
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & (
                        (VD4009 == "08" & V4019 == "2" & VD4016 >= 998) |
                          (VD4009 == "09" & VD4010 != "01" & VD4012 == "2" & VD4016 >= 998)
                      ), 14, CONDLAB))

# Onda 15: Informal marginal
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & (
                        (VD4009 == "08" & V4019 == "2" & VD4016 < 998) |
                          (VD4009 == "09" & VD4010 != "01" & VD4012 == "2" & VD4016 < 998)
                      ), 15, CONDLAB))

# Onda 16: Familiar auxiliar fora da agricultura
pnad_design <- update(pnad_design,
                      CONDLAB = ifelse(sol & VD4009 == "10" & VD4010 != "01", 16, CONDLAB))

# Nordeste
design_nordeste <- subset(pnad_design, UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29))
design_nordeste <- subset(design_nordeste,VD4001==1 & VD4002==1)
total_nordeste <- svytotal(~CONDLAB, design_nordeste, na.rm = TRUE)/1000
round((total_nordeste/sum(total_nordeste))*100,1)
table(pnad_design$variables$CONDLAB, useNA = "ifany")
svytable(~CONDLAB, design = design_nordeste)
round(prop.table(svytable(~CONDLAB, design = design_nordeste)) * 100, 1)

design_restobrasil <- subset(pnad_design, !(UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)))
design_restobrasil <- subset(design_restobrasil, VD4001 == 1 & VD4002 == 1)
total_restobrasil <- svytotal(~CONDLAB, design_restobrasil, na.rm = TRUE) / 1000
round(prop.table(svytable(~CONDLAB, design = design_restobrasil)) * 100, 1)


# Renda Média por Condição Laboral ----------------------------------------

dadosPNADc19.3$variables$CONDLAB <- pnad_design$variables$CONDLAB

dadosPNADc19.3$variables$VD4019 <- as.numeric(dadosPNADc19.3$variables$VD4019)

# Novo salário mínimo de referência
salario_minimo_2019 <- 998

# Atualizar o objeto pnad_design com a variável convertida
pnad_design <- update(dadosPNADc19.3,
                      renda_sm = VD4019 / salario_minimo_2019,
                      CONDLAB = CONDLAB)

# Subsets regionais
design_total     <- subset(pnad_design, VD4001 == 1 & VD4002 == 1)
design_nordeste  <- subset(pnad_design, UF %in% 21:29 & VD4001 == 1 & VD4002 == 1)
design_br_sem_ne <- subset(pnad_design, !(UF %in% 21:29) & VD4001 == 1 & VD4002 == 1)

# Calcular as médias por CONDLAB
calc_medias <- function(design) {
  sapply(1:16, function(c) {
    sub_dsgn <- subset(design, CONDLAB == c)
    tryCatch(
      coef(svymean(~renda_sm, sub_dsgn, na.rm = TRUE)),
      error = function(e) NA
    )
  })
}

# Aplicar para cada região
med_total     <- calc_medias(design_total)
med_nordeste  <- calc_medias(design_nordeste)
med_br_sem_ne <- calc_medias(design_br_sem_ne)

# Juntar os resultados
medias_renda <- data.frame(
  CONDLAB = 1:16,
  Brasil_Total = round(med_total, 3),
  Nordeste = round(med_nordeste, 3),
  Brasil_Sem_NE = round(med_br_sem_ne, 3)
)

medias_renda


# Distribuição em % da força de trabalho por sexo e raça -----------------------

pnad_design <- update(pnad_design,
                      sexo_raca4 = factor(
                        ifelse(V2010 %in% c(3, 5), NA,  # Exclui amarelos e indígenas
                               ifelse(V2007 == 1 & V2010 == 1, "Homem Branco",
                                      ifelse(V2007 == 1 & V2010 %in% c(2, 4), "Homem Negro",
                                             ifelse(V2007 == 2 & V2010 == 1, "Mulher Branca",
                                                    ifelse(V2007 == 2 & V2010 %in% c(2, 4), "Mulher Negra", NA))))),
                        levels = c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
                      )
)

design_total     <- subset(pnad_design, VD4001 == 1 & VD4002 == 1)
design_nordeste  <- subset(pnad_design, UF %in% 21:29 & VD4001 == 1 & VD4002 == 1)

calc_proporcoes_sexo_raca <- function(design) {
  tab <- svytable(~sexo_raca4, design)
  prop <- prop.table(tab) * 100
  round(prop, 1)
}

prop_total    <- calc_proporcoes_sexo_raca(design_total)
prop_nordeste <- calc_proporcoes_sexo_raca(design_nordeste)

tabela_final <- data.frame(
  Categoria = names(prop_total),
  Brasil = as.numeric(prop_total),
  Nordeste = as.numeric(prop_nordeste)
)
knitr::kable(tabela_final, caption = "Distribuição % da força de trabalho por sexo e raça")


# Distribuição em % da força de trabalho por sexo e raça por categoria de ocupação------------------------


# Garante que as variáveis rotuladas existam no design principal
pnad_design <- update(pnad_design,
                      sexo = factor(V2007, levels = c(1, 2), labels = c("Homem", "Mulher")),
                      raca = factor(V2010, levels = 1:5,
                                    labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
)

# Refaz os subconjuntos com as variáveis incluídas
design_total     <- subset(pnad_design, VD4001 == 1 & VD4002 == 1)
design_nordeste  <- subset(pnad_design, UF %in% 21:29 & VD4001 == 1 & VD4002 == 1)
design_br_sem_ne <- subset(pnad_design, !(UF %in% 21:29) & VD4001 == 1 & VD4002 == 1)

# Função para calcular proporções por CONDLAB x sexo x raca
calc_condlab_porcentagem <- function(design) {
  # Contagem cruzada
  tab <- svytable(~CONDLAB + sexo + raca, design)
  
  # Transforma para data.frame
  df <- as.data.frame(tab)
  
  # Soma total por CONDLAB (para proporção relativa a cada onda)
  total_por_onda <- aggregate(Freq ~ CONDLAB, df, sum)
  names(total_por_onda)[2] <- "total_onda"
  
  # Junta para calcular percentual
  df <- merge(df, total_por_onda, by = "CONDLAB")
  df$percentual <- round(100 * df$Freq / df$total_onda, 2)
  
  return(df)
}

tab_total     <- calc_condlab_porcentagem(design_total)
tab_nordeste  <- calc_condlab_porcentagem(design_nordeste)
tab_br_sem_ne <- calc_condlab_porcentagem(design_br_sem_ne)


# Taxa de sindicalização 2020-2024 ----------------------------------------


