######## 0. Leitura de pacote ########

library(PNADcIBGE)
library(survey)

######## 0.1 Criação de variaveis ########

## CONDLAB

# if VD4001==1 & VD4002==1 & Ano ==2019{
#   
#   gen CONDLAB = 1 if VD4009==9 & VD4010 == 1 | VD4009==10 & VD4010 == 1 
#   replace CONDLAB = 2 if VD4009 == 2 & VD4010 == 1
#   replace CONDLAB = 3 if VD4009 == 1 & VD4011 == 1 | VD4009 == 1 & VD4011 == 2 | VD4009 == 1 & VD4011 == 3
#   replace CONDLAB = 4 if VD4009 == 1 & VD4011 == 4 | VD4009 == 1 & VD4011 == 5
#   replace CONDLAB = 5 if VD4009 == 1 & VD4011 == 6 | VD4009 == 1 & VD4011 == 7 | VD4009 == 1 & VD4011 == 8
#   replace CONDLAB = 6 if VD4009 == 1 & VD4011 == 9
#   replace CONDLAB = 7 if VD4009 == 2 & V4018 == 1 & VD4010 != 1
#   replace CONDLAB = 8 if VD4009 == 2 & V4018 == 2 & VD4010 != 1 | VD4009 == 2 & V4018 == 3  & VD4010 != 1 | VD4009 == 2 & V4018 == 4  & VD4010 != 1
#   replace CONDLAB = 9 if VD4009 == 3 | VD4009 == 4
#   replace CONDLAB = 10 if VD4009 == 6
#   replace CONDLAB = 11 if VD4009 == 5 | VD4009 == 7 | VD4009 == 1 & VD4011==10
#   replace CONDLAB = 12 if VD4009 == 8 & V4019  == 1
#   replace CONDLAB = 13 if VD4009 == 9 & VD4012 == 1 & VD4010 != 1
#   replace CONDLAB = 14 if (VD4009== 8 & V4019==2 & VD4016 >= 998) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 >= 998)
#   replace CONDLAB = 15 if (VD4009== 8 & V4019==2 & VD4016 < 998) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 < 998)
#   replace CONDLAB = 16 if VD4009 == 10 & VD4010 > 1	
#   
#   
# }
# 
# if VD4001==1 & VD4002==1 & Ano ==2017 {
#   
#   replace CONDLAB = 1 if VD4009==9 & VD4010 == 1 | VD4009==10 & VD4010 == 1 
#   replace CONDLAB = 2 if VD4009 == 2 & VD4010 == 1
#   replace CONDLAB = 3 if VD4009 == 1 & VD4011 == 1 | VD4009 == 1 & VD4011 == 2 | VD4009 == 1 & VD4011 == 3
#   replace CONDLAB = 4 if VD4009 == 1 & VD4011 == 4 | VD4009 == 1 & VD4011 == 5
#   replace CONDLAB = 5 if VD4009 == 1 & VD4011 == 6 | VD4009 == 1 & VD4011 == 7 | VD4009 == 1 & VD4011 == 8
#   replace CONDLAB = 6 if VD4009 == 1 & VD4011 == 9
#   replace CONDLAB = 7 if VD4009 == 2 & V4018 == 1 & VD4010 != 1
#   replace CONDLAB = 8 if VD4009 == 2 & V4018 == 2 & VD4010 != 1 | VD4009 == 2 & V4018 == 3  & VD4010 != 1 | VD4009 == 2 & V4018 == 4  & VD4010 != 1
#   replace CONDLAB = 9 if VD4009 == 3 | VD4009 == 4
#   replace CONDLAB = 10 if VD4009 == 6
#   replace CONDLAB = 11 if VD4009 == 5 | VD4009 == 7 | VD4009 == 1 & VD4011==10
#   replace CONDLAB = 12 if VD4009 == 8 & V4019  == 1
#   replace CONDLAB = 13 if VD4009 == 9 & VD4012 == 1 & VD4010 != 1
#   replace CONDLAB = 14 if (VD4009== 8 & V4019==2 & VD4016 >= 937) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 >= 937)
#   replace CONDLAB = 15 if (VD4009== 8 & V4019==2 & VD4016 < 937) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 < 937)
#   replace CONDLAB = 16 if VD4009 == 10 & VD4010 > 1	
#   
#   
# }
# 
# if VD4001==1 & VD4002==1 & Ano == 2014{
#   
#   replace CONDLAB = 1 if VD4009==9 & VD4010 == 1 | VD4009==10 & VD4010 == 1 
#   replace CONDLAB = 2 if VD4009 == 2 & VD4010 == 1
#   replace CONDLAB = 3 if VD4009 == 1 & VD4011 == 1 | VD4009 == 1 & VD4011 == 2 | VD4009 == 1 & VD4011 == 3
#   replace CONDLAB = 4 if VD4009 == 1 & VD4011 == 4 | VD4009 == 1 & VD4011 == 5
#   replace CONDLAB = 5 if VD4009 == 1 & VD4011 == 6 | VD4009 == 1 & VD4011 == 7 | VD4009 == 1 & VD4011 == 8
#   replace CONDLAB = 6 if VD4009 == 1 & VD4011 == 9
#   replace CONDLAB = 7 if VD4009 == 2 & V4018 == 1 & VD4010 != 1
#   replace CONDLAB = 8 if VD4009 == 2 & V4018 == 2 & VD4010 != 1 | VD4009 == 2 & V4018 == 3  & VD4010 != 1 | VD4009 == 2 & V4018 == 4  & VD4010 != 1
#   replace CONDLAB = 9 if VD4009 == 3 | VD4009 == 4
#   replace CONDLAB = 10 if VD4009 == 6
#   replace CONDLAB = 11 if VD4009 == 5 | VD4009 == 7 | VD4009 == 1 & VD4011==10
#   replace CONDLAB = 12 if VD4009 == 8 & VD4012  == 1
#   replace CONDLAB = 13 if VD4009 == 9 & VD4012 == 1 & VD4010 != 1
#   replace CONDLAB = 14 if (VD4009== 8 & V4019==2 & VD4016 >= 724) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 >= 724)
#   replace CONDLAB = 15 if (VD4009== 8 & V4019==2 & VD4016 < 724) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 < 724)
#   replace CONDLAB = 16 if VD4009 == 10 & VD4010 > 1	
#   
#   
# }


## Ou se achar melhor assim...

# Calculation of CONDLAB
# 
# if VD4001==1 (Pessoas na força de trabalho) & VD4002==1 (Pessoas ocupadas)
# 
# 1 "Conta própria/familiar na agricultura" 
# gen CONDLAB = 1 if VD4009==9 & VD4010 == 1 | VD4009==10 & VD4010 == 1 
# VD4010=01	Agricultura, pecuária, produção florestal, pesca e aquicultura 
# VD4009=09	Conta-própria
# VD4009=10	Trabalhador familiar auxiliar
# 
# 2 "Assalariado sem carteira na agricultura" 
# replace CONDLAB = 2 if VD4009 == 2 & VD4010 == 1
# VD4009=02	Empregado no setor privado sem carteira de trabalho assinada
# VD4010=01	Agricultura, pecuária, produção florestal, pesca e aquicultura 
# 
# 3 "Assalariado setor privado com carteira, colarinho branco superior" 
# replace CONDLAB = 3 if VD4009 == 1 & VD4011 == 1 | VD4009 == 1 & VD4011 == 2 | VD4009 == 1 & VD4011 == 3
# VD4009=01	Empregado no setor privado com carteira de trabalho assinada
# VD4011=01	Diretores e gerentes
# VD4011=02	Profissionais das ciências e intelectuais
# VD4011=03	Técnicos e profissionais de nível médio
# 
# 4 "Assalariado setor privado com carteira, colarinho branco inferior/serviços/comercio " 
# replace CONDLAB = 4 if VD4009 == 1 & VD4011 == 4 | VD4009 == 1 & VD4011 == 5
# VD4009=01	Empregado no setor privado com carteira de trabalho assinada
# VD4011=04	Trabalhadores de apoio administrativo
# VD4011=05	Trabalhadores dos serviços, vendedores dos comércios e mercados
# 
# 5 "Assalariado setor privado com carteira, qualificado o semi-qualificado" 
# replace CONDLAB = 5 if VD4009 == 1 & VD4011 == 6 | VD4009 == 1 & VD4011 == 7 | VD4009 == 1 & VD4011 == 8
# VD4009=01	Empregado no setor privado com carteira de trabalho assinada
# VD4011=06	Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca
# VD4011=07	Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios
# VD4011=08	Operadores de instalações e máquinas e montadores
# 
# 6 "Assalariado setor privado com carteira, não qualificado" 
# replace CONDLAB = 6 if VD4009 == 1 & VD4011 == 9
# VD4009=01	Empregado no setor privado com carteira de trabalho assinada
# VD4011=09	Ocupações elementares
# 
# 7 "Assalariado sem carteira setor privado pequenas empresas (até 5 trabalhadores)" 
# replace CONDLAB = 7 if VD4009 == 2 & V4018 == 1 & VD4010 != 1
# VD4009=02	Empregado no setor privado sem carteira de trabalho assinada
# V4018=1	1 a 5 pessoas
# VD4010 !=01	Not Agricultura, pecuária, produção florestal, pesca e aquicultura 
# 
# 
# 
# 8 "Assalariado sem carteira setor privado medias/grandes empresas" 
# replace CONDLAB = 8 if VD4009 == 2 & V4018 == 2 | VD4009 == 2 & V4018 == 3  & VD4010 != 1 | VD4009 == 2 & V4018 == 4  & VD4010 != 1
# ** need to add criterion VD4010 != 1 in the first one **
#   VD4009=02	Empregado no setor privado sem carteira de trabalho assinada
# V4018=2	6 a 10 pessoas 
# V4018=3	11 a 50 pessoas
# V4018=4	51 ou mais pessoas
# VD4010 !=01	Not Agricultura, pecuária, produção florestal, pesca e aquicultura 
# 
# 9 "Trabalhador doméstico"
# replace CONDLAB = 9 if VD4009 == 3 | VD4009 == 4
# VD4009=03	Trabalhador doméstico com carteira de trabalho assinada
# VD4009=04	Trabalhador doméstico sem carteira de trabalho assinada
# 
# 10 "Trabalhador do setor público sem carteira" 
# replace CONDLAB = 10 if VD4009 == 6
# VD4009=06	Empregado no setor público sem carteira de trabalho assinada
# 
# 11 "Trabalhador do setor público com carteira, militares e funcionários públicos" 
# replace CONDLAB = 11 if VD4009 == 5 | VD4009 == 7 | VD4009 == 1 & VD4011==10
# VD4009=05	Empregado no setor público com carteira de trabalho assinada
# VD4009=07	Militar e servidor estatutário
# VD4009=01	Empregado no setor privado com carteira de trabalho assinada
# VD4011=10	Membros das forças armadas, policiais e bombeiros militares
# 
# 12 "Empregador formal (com CNPJ)" 
# replace CONDLAB = 12 if VD4009 == 8 & V4019  == 1
# VD4009=08	Empregador
# V4019=1	Sim (registrado CNPJ)
# 
# 13 "Trabalhador autônomo formal (contribuindo para a previdência)" 
# replace CONDLAB = 13 if VD4009 == 9 & VD4012 == 1 & VD4010 != 1
# VD4009=09	Conta-própria
# VD4010 !=01	Not Agricultura, pecuária, produção florestal, pesca e aquicultura 
# VD4012=1	Sim contribuinte previdência
# 
# 14 "Empregador/autônomo informal “produtivo” fora da agricultura" 
# replace CONDLAB = 14 if (VD4009== 8 & V4019==2 & VD4016 >= 998) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 >= 998)
# VD4009=08	Empregador
# VD4009=09	Conta-própria
# VD4010>01	Not Agricultura, pecuária, produção florestal, pesca e aquicultura 
# V4019=2	Não (não registrado CNPJ)
# VD4016>998	Income greater than minimum wage
# 
# 
# 15 "Empregador/autônomo informal marginal fora da agricultura " 
# replace CONDLAB = 15 if (VD4009== 8 & V4019==2 & VD4016 < 998) | (VD4009== 9 & VD4010 > 1 & VD4012==2 & VD4016 < 998)
# VD4009=08	Empregador
# VD4009=09	Conta-própria
# V4019=2	Não (não registrado CNPJ)
# VD4016<998	Income less than minimum wage
# VD4010>01	Not Agricultura, pecuária, produção florestal, pesca e aquicultura 
# VD4012=2	Não contribuinte previdência
# 
# 16 "Trabalhador familiar auxiliar fora da agricultura”
# replace CONDLAB = 16 if VD4009 == 10 & VD4010 > 1
# VD4009=10	Trabalhador familiar auxiliar
# VD4010>01	Not Agricultura, pecuária, produção florestal, pesca e aquicultura 



## As três categorias regionais (colunas) que queremos para as tabelas são (a) o Nordeste (b)
## o Brasil sem o Nordeste, e (c) o Brasil inteiro. Não é exatamente o mesmo que nos livros.

######## 1.	Distribuição em % dos ocupados por condição laboral, para as três categorias regionais.  Ver tabela 3.2 no livro em inglês e tabela 2 do capítulo VII no livro em português. Não precisamos das últimas linhas das tabelas nos livros (totais para agricultura, etc.)   


######## 2.	Renda média por condição laboral para as três categorias regionais. Ver tabela 3.3 no livro em inglês; tabela 3 do capítulo VII no livro em português. Não precisamos das últimas linhas da tabela no livro em inglês (totais para agricultura, etc.)  ########


######## 3.	Distribuição em % da força de trabalho por sexo e raça para as três categorias regionais (ver tabela 1 do capítulo VIII no livro em português) ########


######## 4.	Distribuição em % dos ocupados por condição laboral, sexo e raça. Ver tabela 3.7 no livro em inglês – quatro categorias: homens brancos, homens negros, mulheres brancos, mulheres negros (“negro”=preto+pardo; amarelo e indígenas excluídos). Precisamos desta tabela por separado para o Nordeste e para o Brasil inteiro. ########


######## 5.	Taxa de sindicalização com dadas da PNADC, Brasil inteiro e Nordeste, 2020-2024 (ver tabela 7 no capítulo IX no livro em português) ########


######## 6.	Indice de preços para 2019.3 e 2024.3 (nacional). ########
