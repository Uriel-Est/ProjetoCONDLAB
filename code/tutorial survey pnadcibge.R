#Tutorial
library(PNADcIBGE)
library(survey)

# Carregamento dos bancos de dados ----------------------------------------
dadosPNADcT <- get_pnadc(year = 2024, quarter = 3) #busca da internet a pnad do ano/trimestre desejados
dadosPNADcT <- get_pnadc(year = 2024, quarter = 3, vars = c("VD4001", "VD4002"))#busca a pnad com as variáveis desejadas
#^VD4001 = Condição em relação a força de trabalho // VD4002 = Condição de Ocupação^
dadosPNADcT #dá as informações da situação do banco de dados
class(dadosPNADcT) #classe do banco
#Caso não queira trabalhar com esse formato, podemos usar design=F para baixar microdados brutos:
dadosPNADcT_brutos <- get_pnadc(year = 2024, quarter = 3, vars = c("VD4001", "VD4002"), design = F)
dadosPNADcT_brutos
#Também é possível baixar os dados sem incluir os rótulos de nível, através do argumento 'labels'
dadosPNADcT_brutos_sem <- get_pnadc(year = 2024, quarter = 3, vars = c("VD4001", "VD4002"), labels = F, design = F)
dadosPNADcT_brutos_sem

#para importação de microdados anuais, basta informar o número da entrevista desejada no argumento 'interview'
dadosPNADcT_anual_visita <- get_pnadc(year = 2023, interview = 1)
dadosPNADcT_anual_visita
#todas as outras labels podem ser utilizadas igualmente aos dados trimestrais~


#para realziara importação dos microdados de temas e tópicos suplementares de moradores selecionados
dadosPNADcT_anual_trimestre_morador_selecionado <- get_pnadc(year = 2023, topic = 4, selected = T)
dadosPNADcT_anual_trimestre_morador_selecionado

variaveis_selecionadasT <- c("UF", "V2001", "V2005", "V2007", "V2009", "V2010", "V3007", "VD3004", 
                             "VD4001", "VD4002", "VD4020", "VD4035") #função para simplificar as variáveis escolhdias
dadosPNADcT <- get_pnadc(year = 2017, quarter = 4, vars = variaveis_selecionadasT) 

totalrendaT <- svytotal(x=~VD4020, design = dadosPNADcT, na.rm = T)
#estimação do total de uma variável numérica única, mostra também o erro padrão [se]
totalrendaT

# Estimativas Estatísticas -----------------------------------------------------
#assim, podemos calcular outros dados estatísticos para auxiliar a estimativa
cv(object = totalrendaT) #coeficiente de variação
confint(object = totalrendaT) #intervalos de confiança
confint(object = totalrendaT, level = 0.99) #level especifica o nível de confiança desejado para o cálculo do intervalo

#também é possível estimar totais populacionais categóricos, como o sexo:
totalsexoT <- svytotal(x=~V2007, design = dadosPNADcT, na.rm = T)
totalsexoT
#estimar mais de uma variável categórica no mesmo código com operador +:
totalsexoraçaT <- svytotal(x=~V2007+V2010, design = dadosPNADcT, na.rm = T)
totalsexoraçaT
#estimar o total resultante do cruzamento de duas variáveis pela função interaction:
totalsexoEraçaT <- svytotal(x=~interaction(V2007, V2010), design = dadosPNADcT, na.rm = T)
totalsexoEraçaT
ftable(x = totalsexoEraçaT) #a função ftable realiza uma saída mais organizada dos dados


#a média é calculada através da função svymean, que tem sintaxe igual a svytotal
mediarendaT <- svymean(x=~VD4020, design = dadosPNADcT, na.rm = T)
mediarendaT
#o cálculo de coeficientes de variação e intervalo de confiança é a mesma coisa:
cv(object = mediarendaT)
confint(object = mediarendaT)

#é possível também estimar frequência relativa através de svymean com uma sintaxe análoga:
propsexoT <- svymean(x=~V2007, design = dadosPNADcT, na.rm = T)
propsexoT
#da mesma forma estimamos a proporção de mais de uma variável:
propraçasexoT <- svymean(x=~V2007+V2010, design = dadosPNADcT, na.rm = T)
propraçasexoT
#continua igual no cruzamento, com interaction
propraçaEsexoT <- svymean(x=~interaction(V2007, V2010), design = dadosPNADcT, na.rm = T)
ftable(x = propraçaEsexoT)

#para estimar razões entre variáveis, utilizaremos svyratio
#sua sintaxe utiliza quatro argumentos: a variável que estará no numerador, a variável do denominador
#o objeto do plano amostral e a opção de remover valores não aplicáveis:
txdesocupT <- svyratio(numerator = ~(VD4002 == "Pessoas desocupadas"), denominator = ~(VD4001 == "Pessoas na força de trabalho"),
                       design = dadosPNADcT, na.rm = T)
txdesocupT
#calculo do coeficiente de variação e intervalos de confiança:
cv(object = txdesocupT)
confint(object = txdesocupT)

#medianas e quantis são estimados através de svyquantile, a única novidade é estimar os quantis desejados:
xmediarendaT <- svyquantile(x=~VD4020, design = dadosPNADcT, quantiles = 0.5, ci = T, na.rm = T)
mediarendaT #parâmetro ci indica se quer o coeficiente de confiança, ou não, para os quantis estimados~
SE(object = xmediarendaT) #erro padrão
cv(object = xmediarendaT) #coeficiente de variação
#calcular vários quantis simuntaneamente adicionando um vetor no argumento quantiles
quantisrendaT <- svyquantile(x=~VD4020, design = dadosPNADcT, 
                             quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9), ci = F, na.rm = T)
quantisrendaT

#estimar totais e médias variáveis de rendimento deflacionado, passando a obter estimativas sobre valores reais
#primeiro é necessário criar a variável de rendimento com valores deflacionados nos microdados
dadosPNADcT$variables <- transform(dadosPNADcT$variables, VD4020_real = VD4020*Efetivo)
#após a criação, para obter o total deflacionado basta usar svytotal:
totalrenda_realT <- svytotal(x=~VD4020_real, design = dadosPNADcT, na.rm = T)
totalrenda_realT
#para obter a média dessa variável é só usar svymean:
mediarenda_realT <- svymean(x=~VD4020_real, design = dadosPNADcT, na.rm = T)
mediarenda_realT
#o processo de deflacionamento é importantíssimo para comparação de série histórica


