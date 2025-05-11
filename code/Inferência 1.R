library(PNADcIBGE)
library(sidrar)
library(survey)
library(ggplot2)
library(dplyr)
library(srvyr)
library(tidyr)
library(knitr)
library(purrr)
library(tidyverse)
library(writexl)

# 1 .Condições Laborais: -----------------------------------------------------------------
#Distribuição em % dos ocupados por condição laboral, para as três categorias regionais:

# 1.1 Carregar os dados
dadosPNADc19.3 <- get_pnadc(year = 2024, quarter = 3, labels = FALSE)
gc()

# 1.2 Criar o objeto survey design
pnad_design <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~round(V1028),
  data = dadosPNADc19.3$variables,
  nest = TRUE
)

# 1.3 Atualizar variáveis como caracteres
pnad_design <- update(pnad_design,
                      VD4009 = as.character(VD4009),
                      VD4010 = as.character(VD4010),
                      VD4011 = as.character(VD4011),
                      VD4019 = as.character(VD4019)
)

# 1.4 Criar a variável CONDLAB inicial como NA
pnad_design <- update(pnad_design, CONDLAB = NA)

#  1.5 Criar vetor lógico: pessoas ocupadas (o Sol brilha):
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

# 1.6 Regiões de interesse:
design_nordeste <- subset(pnad_design, UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29))
design_nordeste <- subset(design_nordeste,VD4001==1 & VD4002==1)
total_nordeste <- svytotal(~CONDLAB, design_nordeste, na.rm = TRUE)/1000
round((total_nordeste/sum(total_nordeste))*100,1)
table(pnad_design$variables$CONDLAB, useNA = "ifany")
svytable(~CONDLAB, design = design_nordeste)
var_nordeste <- round(prop.table(svytable(~CONDLAB, design = design_nordeste)) * 100, 1)
print(var_nordeste)

design_restobrasil <- subset(pnad_design, !(UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)))
design_restobrasil <- subset(design_restobrasil, VD4001 == 1 & VD4002 == 1)
total_restobrasil <- svytotal(~CONDLAB, design_restobrasil, na.rm = TRUE) / 1000
var_resto <- round(prop.table(svytable(~CONDLAB, design = design_restobrasil)) * 100, 1)
print(var_resto)

design_br <- subset(pnad_design, VD4001 == 1 & VD4002 == 1)
total_br <- svytotal(~CONDLAB, design_br, na.rm = T) / 1000
prop_br <- round(prop.table(svytable(~CONDLAB, design = design_br)) * 100, 1)
print(prop_br)

# Criando a tabela [categorias em ing]:
# Nomeando as categorias:
condlab_labels <- c(
  "1" = "Self-employed/family worker in agriculture",
  "2" = "Wage worker without carteira in agriculture",
  "3" = "Private sector wage worker with carteira, upper white-collar",
  "4" = "Private sector wage worker with carteira, lower white-collar/services/trade",
  "5" = "Private sector wage worker with carteira, blue-collar, skilled or semi-skilled",
  "6" = "Private sector wage worker with carteira, blue-collar, unskilled",
  "7" = "Private sector wage worker without carteira, small enterprise (up to five workers)",
  "8" = "Private sector wage worker without carteira, medium/large enterprise (over five workers)",
  "9" = "Domestic service worker",
  "10" = "Public sector worker without carteira",
  "11" = "Public sector worker with carteira/military/civil servant",
  "12" = "Formal employer (with CNPJ registration)",
  "13" = "Formal non-agricultural self-employed worker (contributing to social-security)",
  "14" = "Informal 'productive' non-agricultural self-employed worker or employer",
  "15" = "Informal 'marginal' non-agricultural self-employed worker or employer",
  "16" = "Non-agricultural family worker"
)

# Tabela 1:
tabela_final1 <- bind_rows(
  # Nordeste
  data.frame(
    Regiao = "Northeast",
    Categoria = names(var_nordeste),
    Percentual = as.numeric(var_nordeste)
  ),
  # Brasil s/ Nordeste
  data.frame(
    Regiao = "Brazil w/o northeast",
    Categoria = names(var_resto),
    Percentual = as.numeric(var_resto)
  ),
  # Brasil 100%
  data.frame(
    Regiao = "Brazil",
    Categoria = names(prop_br),
    Percentual = as.numeric(prop_br)
  )
)

# Don't go tall, go wide:
tabela_final1_wide <- tabela_final1 %>%
  pivot_wider(
    names_from = Regiao,
    values_from = Percentual
  )

tabela_final1_wide <- tabela_final1_wide %>%
  mutate(
    Label = condlab_labels
  ) %>%
  select(Categoria, Label, everything())

# 2. Renda Média por Condição Laboral ----------------------------------------

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

income_table <- medias_renda %>%
  mutate(
    Label = condlab_labels
  ) %>%
  select(
    Category = CONDLAB,
    Label,
    Brazil_Total = Brasil_Total,
    Northeast = Nordeste,
    Brazil_wo_NE = Brasil_Sem_NE
    )

# Use this to preview ANY table cleanly before export
preview_pretty <- function(df) {
  options(width = 200)  # Widen console
  print(
    knitr::kable(df, format = "simple", align = 'c'),
    row.names = FALSE
  )
}

preview_pretty(income_table)

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

print(tabela_final)

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

# 1. Converter CONDLAB para caractere (ou numérico) em todas as tabelas antes do join
tab_total <- tab_total %>% mutate(CONDLAB = as.character(CONDLAB))
tab_nordeste <- tab_nordeste %>% mutate(CONDLAB = as.character(CONDLAB))
tab_br_sem_ne <- tab_br_sem_ne %>% mutate(CONDLAB = as.character(CONDLAB))

# 2. Agora sim juntar os dados
dados_completos <- bind_rows(
  tab_total %>% mutate(Regiao = "Brazil"),
  tab_nordeste %>% mutate(Regiao = "Northeast"), 
  tab_br_sem_ne %>% mutate(Regiao = "Brazil w/o NE")
) %>%
  left_join(
    data.frame(
      CONDLAB = as.character(1:16),  # Garantir que seja caractere
      Categoria = condlab_labels
    ),
    by = "CONDLAB"
  )

tabela_organizada <- dados_completos %>%
  mutate(
    # Criar coluna combinada de Região > Sexo > Raça
    Grupo = paste(Regiao, sexo, raca, sep = " > ")
  ) %>% 
  select(CONDLAB, Categoria, Grupo, percentual) %>%
  # Transformar em formato wide (uma coluna por grupo)
  pivot_wider(
    names_from = Grupo,
    values_from = percentual,
    names_sort = TRUE  # Ordena as colunas alfabeticamente
  ) %>%
  # Ordenar pelas categorias de trabalho
  arrange(CONDLAB)

# Calcular médias por região
dados_completos %>% 
  group_by(Regiao, CONDLAB) %>% 
  summarise(
    Media_Percentual = mean(percentual, na.rm = TRUE),
    .groups = 'drop'
  )

writexl::write_xlsx(tabela_organizada, "tabela_teste.xlsx")

# 5 Taxa de sindicalização 2020-2024 ----------------------------------------

# 5.1. Baixar apenas o 3º trimestre de 2019 (entrevista 3)
pnadc_2019T3 <- get_pnadc(
  year = 2019,
  interview = 1,  # <-- entrevista número 3
  design = TRUE,
  labels = FALSE,
  deflator = FALSE,
  vars = c("V1028", "V1029", "V1030", "V4072A", "UF")  # pesos e sindicalização
)

# 5.2. Criar variável de sindicalização (lógica com NA onde for inválido)
pnadc_2019T3 <- update(
  pnadc_2019T3,
  sindicado = ifelse(V4072A %in% c(1, 2, 3), V4072A == 3, NA)
)

# 5.3. Calcular taxa de sindicalização com svymean
resultado_brasil <- svymean(~sindicado, pnadc_2019T3, na.rm = TRUE)

# 5.4. Se quiser restringir para Nordeste:
pnadc_nordeste <- subset(pnadc_2019T3, UF %in% 21:29)  # códigos IBGE para estados do NE
resultado_nordeste <- svymean(~sindicado, pnadc_nordeste, na.rm = TRUE)

# 5.5. Mostrar os resultados
resultado_brasil
resultado_nordeste


# 6. Variação de Preço 2019-2024 ---------------------------------------------


# 6.1Baixa os dados do IPCA (variação percentual trimestral)
# 2024
fetch_ipca19 <- function() {
  get_sidra(
  x = 1419,                  # Tabela do IPCA
  variable = 63,             # Código da variável "Variação percentual no período"
  period = c("201907", "201908", "201909")  # Períodos no formato AAAQ (trimestre)
) %>%
  mutate(
    ano = substr(`Mês (Código)`, 1, 4),
    mes = substr(`Mês (Código)`, 5, 6),
    trimestre = "2019.3",
    fonte = "IPCA Oficial"
  )
}
# 2023
fetch_ipca24 <- function() {
  get_sidra(
    x = 7060,
    variable = 63,
    period = c("202407", "202408", "202409") # Jul-Set 2024
  ) %>% 
    mutate(
      ano = substr(`Mês (Código)`, 1, 4),
      mes = substr(`Mês (Código)`, 5, 6),
      trimestre = "2024.3",
      fonte = "IPCA15 (prévia)"
  )
}
# Executar
dados_consolidados <- bind_rows(
  fetch_ipca19(),
  fetch_ipca24()
) %>% 
  group_by(trimestre, fonte) %>% 
  summarise(
    ipca_medio = mean(Valor, na.rm = TRUE),
    .groups = 'drop'
  )

print(dados_consolidados)

dados_consolidados_df <- as.data.frame(dados_consolidados)

# Exportação --------------------------------------------------------------


# Write multiple tables to separate sheets in the same Excel file
write_xlsx(list("Sheet1" = tabela_final1_wide, "Sheet2" = income_table, "Sheet3" = tabela_final, "Sheet4" = tabela_organizada, "Sheet6" = dados_consolidados), "CONDLAB.xlsx")
