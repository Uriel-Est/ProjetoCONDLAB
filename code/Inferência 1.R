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

knitr::kable(tabela_final1_wide, digits = 1)

writexl::write_xlsx(tabela_final1_wide, "C:/Users/uriel/OneDrive/Documentos/txt/UFPB Estatística/CONDLAB/ProjetoCONDLAB/code/teste_CONDLAB.xlsx")

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

# 3. Distribuição em % da força de trabalho por sexo e raça -----------------------

# 3.1 Atualizar o design com categorias raciais completas
pnad_design <- update(pnad_design,
                      sexo_raca8 = factor(
                        case_when(
                          V2007 == 1 & V2010 == 1 ~ "Homem Branco",
                          V2007 == 1 & V2010 == 2 ~ "Homem Preto",
                          V2007 == 1 & V2010 == 3 ~ "Homem Amarelo",
                          V2007 == 1 & V2010 == 4 ~ "Homem Pardo",
                          V2007 == 1 & V2010 == 5 ~ "Homem Indígena",
                          V2007 == 2 & V2010 == 1 ~ "Mulher Branca",
                          V2007 == 2 & V2010 == 2 ~ "Mulher Preta",
                          V2007 == 2 & V2010 == 3 ~ "Mulher Amarela",
                          V2007 == 2 & V2010 == 4 ~ "Mulher Parda",
                          V2007 == 2 & V2010 == 5 ~ "Mulher Indígena",
                          TRUE ~ NA_character_
                        ),
                        levels = c("Homem Branco", "Homem Preto", "Homem Amarelo", "Homem Pardo", "Homem Indígena",
                                   "Mulher Branca", "Mulher Preta", "Mulher Amarela", "Mulher Parda", "Mulher Indígena")
                      )
)

# 3.2 Criar subsets regionais
design_total     <- subset(pnad_design, VD4001 == 1 & VD4002 == 1)
design_nordeste  <- subset(pnad_design, UF %in% 21:29 & VD4001 == 1 & VD4002 == 1)
design_br_sem_ne <- subset(pnad_design, !(UF %in% 21:29) & VD4001 == 1 & VD4002 == 1)

# 3.3 Função de cálculo para estimativa
calc_proporcoes_sexo_raca <- function(design) {
  tab <- svytable(~sexo_raca8, design, exclude = NULL)
  prop <- prop.table(tab) * 100
  round(prop, 1)
}

# 3.4 Calcular proporções
prop_total    <- calc_proporcoes_sexo_raca(design_total)
prop_nordeste <- calc_proporcoes_sexo_raca(design_nordeste)
prop_br_sem_ne <- calc_proporcoes_sexo_raca(design_br_sem_ne)

# 3.5 Criação da tabela final
tabela_final <- data.frame(
  Categoria = names(prop_total),
  Brasil = as.numeric(prop_total),
  Nordeste = as.numeric(prop_nordeste),
  Brasil_sem_NE = as.numeric(prop_br_sem_ne)
)

# 3.6 Exportação e organização
knitr::kable(tabela_final, caption = "Distribuição % da força de trabalho por sexo e raça")

print(tabela_final)


# 4. Distribuição em % dos ocupados por condição laboral, sexo e raça --------


# 4.1 Atualizar design com categorias consolidadas
pnad_design <- update(pnad_design,
                      sexo_raca = factor(
                        case_when(
                          V2007 == 1 & V2010 == 1 ~ "Homem Branco",
                          V2007 == 1 & V2010 %in% c(2,4) ~ "Homem Negro",
                          V2007 == 2 & V2010 == 1 ~ "Mulher Branca",
                          V2007 == 2 & V2010 %in% c(2,4) ~ "Mulher Negra",
                          TRUE ~ NA_character_
                        ),
                        levels = c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
                      ),
                      CONDLAB = factor(CONDLAB, levels = 1:16, labels = condlab_labels)
)

# 4.2 Função de cálculo otimizada
calcular_distribuicao <- function(design) {
  svytable(~CONDLAB + sexo_raca, design) %>% 
    prop.table(margin = 2) %>% 
    as.data.frame() %>% 
    mutate(percentual = round(Freq * 100, 1)) %>%
    select(-Freq)
}

# 4.3 Processar todas as regiões
dados_regioes <- bind_rows(
  calcular_distribuicao(subset(pnad_design, VD4001 == 1 & VD4002 == 1)) %>% 
    mutate(Regiao = "Brasil"),
  
  calcular_distribuicao(subset(pnad_design, UF %in% 21:29 & VD4001 == 1 & VD4002 == 1)) %>% 
    mutate(Regiao = "Nordeste"),
  
  calcular_distribuicao(subset(pnad_design, !(UF %in% 21:29) & VD4001 == 1 & VD4002 == 1)) %>% 
    mutate(Regiao = "Brasil_sem_NE")
)

# 4.4 Formatar tabela final
tabela_final <- dados_regioes %>% 
  pivot_wider(
    names_from = c(Regiao, sexo_raca),
    names_glue = "{Regiao}_{str_replace(sexo_raca, ' ', '_')}",
    values_from = percentual
  ) %>% 
  mutate(Total = rowSums(select(., -CONDLAB), na.rm = T)) %>%
  arrange(CONDLAB)

# Total ocupados (em mil)
total_nordeste      # vetor svytotal de cada condlab
total_restobrasil   # idem
total_br            # total Brasil inteiro

# Colunas de Total para cada Região
soma_nordeste <- sum(total_nordeste)
soma_restobrasil <- sum(total_restobrasil)
soma_brasil <- sum(total_br)

peso_nordeste <- soma_nordeste / soma_brasil
peso_restobrasil <- soma_restobrasil / soma_brasil

tabela_final <- tabela_final %>%
  mutate(
    Total_Ponderado = (`Northeast` * peso_nordeste +
                         `Brazil w/o northeast` * peso_restobrasil)
  )

# 4.5 Visualização organizada (apenas primeiras linhas)
head(tabela_final) %>% 
  knitr::kable(caption = "Distribuição por ocupação, sexo e raça (%)")


# 4.6 Adaptação²
tabela_final <- dados_regioes %>% 
  mutate(
    Grupo = paste(Regiao, sexo_raca, sep = " - "),
    percentual = paste0(percentual, "%")
  ) %>% 
  select(CONDLAB, Grupo, percentual) %>% 
  pivot_wider(names_from = Grupo, values_from = percentual)

writexl::write_xlsx(tabela_final, "C:/Users/uriel/OneDrive/Documentos/txt/UFPB Estatística/CONDLAB/ProjetoCONDLAB/code/distribuicao_condlab_sexo_raca.xlsx")


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


##Resultados resgatados de Características adicionais do mercado de trabalho PNADC [IBGE]

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

## Dados resgatados de https://www.dadosdemercado.com.br/indices/ipca

# Exportação --------------------------------------------------------------


# Write multiple tables to separate sheets in the same Excel file
write_xlsx(list("Sheet1" = tabela_final1_wide,
                "Sheet2" = income_table,
                "Sheet3" = tabela_final,
                "Sheet4" = tabela_organizada,
                "Sheet6" = dados_consolidados),
           "C:/Users/uriel/OneDrive/Documentos/txt/UFPB Estatística/CONDLAB/ProjetoCONDLAB/code/CONDLAB.xlsx")
