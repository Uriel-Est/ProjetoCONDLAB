library(survey)
library(dplyr)
library(tidyr)

# --------------------------------------------------------
# Create CONDLAB variable
# --------------------------------------------------------

dados <- dadosPNADc19.3$variables

pnad_design <- svydesign(
  ids = ~UPA,
  strata = ~Estrato,
  weights = ~V1028,
  data = df,
  nest = T
) %>%
  update(
    VD4009 = as.character(VD4009),
    VD4010 = as.character(VD4010),
    VD4011 = as.character(VD4011),
    VD4019 = as.character(VD4019),
    
        # Criar variável CONDLAB com todas as condições - DEEPSEEK
        CONDLAB = factor(case_when(
          # 0. Não-empregado
          VD4001 != 1 | VD4002 != 1 ~ 0,
          
          # 1. Conta própria/familiar agricultura
          VD4009 %in% c("09","10") & VD4010 == "01" ~ 1,
          
          # 2. Assalariado sem carteira agricultura
          VD4009 == "02" & VD4010 == "01" ~ 2,
          
          # 3. Privado carteira colarinho branco superior
          VD4009 == "01" & VD4011 %in% c("01","02","03") ~ 3,
          
          # 4. Privado carteira colarinho branco inferior
          VD4009 == "01" & VD4011 %in% c("04","05") ~ 4,
          
          # 5. Privado carteira qualificado/semi-qualificado
          VD4009 == "01" & VD4011 %in% c("06","07","08") ~ 5,
          
          # 6. Privado carteira não qualificado
          VD4009 == "01" & VD4011 == "09" ~ 6,
          
          # 7. Privado sem carteira pequenas empresas
          VD4009 == "02" & V4018 == "1" & VD4010 != "01" ~ 7,
          
          # 8. Privado sem carteira médias/grandes empresas
          VD4009 == "02" & V4018 %in% c("2","3","4") & VD4010 != "01" ~ 8,
          
          # 9. Trabalhador doméstico
          VD4009 %in% c("03","04") ~ 9,
          
          # 10. Público sem carteira
          VD4009 == "06" ~ 10,
          
          # 11. Público com carteira/militares
          VD4009 %in% c("05","07") | (VD4009 == "01" & VD4011 == "10") ~ 11,
          
          # 12. Empregador formal
          VD4009 == "08" & V4019 == "1" ~ 12,
          
          # 13. Autônomo formal
          VD4009 == "09" & VD4012 == "1" & VD4010 != "01" ~ 13,
          
          # 14. Informal produtivo
          (VD4009 == "08" & V4019 == "2" & VD4016 >= 998) |
            (VD4009 == "09" & VD4010 != "01" & VD4012 == "2" & VD4016 >= 998) ~ 14,
          
          # 15. Informal marginal
          (VD4009 == "08" & V4019 == "2" & VD4016 < 998) |
            (VD4009 == "09" & VD4010 != "01" & VD4012 == "2" & VD4016 < 998) ~ 15,
          
          # 16. Familiar auxiliar fora agricultura
          VD4009 == "10" & VD4010 != "01" ~ 16,
          
          # Catch-all para casos não classificados
          TRUE ~ NA_integer_
        ), levels = 0:16, labels = c(
          "Não-empregado",
          "Conta própria/familiar agricultura",
          "Assalariado sem carteira agricultura",
          "Privado carteira colarinho branco superior",
          "Privado carteira colarinho branco inferior",
          "Privado carteira qualificado/semi-qualificado",
          "Privado carteira não qualificado",
          "Privado sem carteira pequenas empresas",
          "Privado sem carteira médias/grandes empresas",
          "Trabalhador doméstico",
          "Público sem carteira",
          "Público com carteira/militares",
          "Empregador formal",
          "Autônomo formal",
          "Informal produtivo",
          "Informal marginal",
          "Familiar auxiliar fora agricultura"
        ))
      )
    
    # Verificar resultados
    svytotal(~CONDLAB, pnad_design) %>% summary()

# --------------------------------------------------------
# Create survey design object
# --------------------------------------------------------

nordeste_ufs <- c(21, 22, 23, 24, 25, 26, 27, 28, 29) # Maranhão to Bahia
    
pnad_design <- update(pnad_design,
                      regiao = factor(ifelse(UF %in% nordeste_ufs, 
                                             "Nordeste", "Brasil sem Nordeste")))

design_nordeste <- subset(pnad_design, regiao == "Nordeste")
design_restobrasil <- subset(pnad_design, regiao == "Brasil sem Nordeste")

# Total para o Brasil todo
total_brasil <- svytotal(~CONDLAB, pnad_design, na.rm = TRUE)

# Total para o Nordeste
total_nordeste <- svytotal(~CONDLAB, design_nordeste, na.rm = TRUE)

# Total para o Brasil sem o Nordeste
total_restobrasil <- svytotal(~CONDLAB, design_restobrasil, na.rm = TRUE)
