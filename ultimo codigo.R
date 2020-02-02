library(lubridate)
library(dplyr)
library(sf)
library(brazilmaps)
library(ggplot2)
library(tsbox)
install.packages("tsbox")

library(zoo)
library(lubridate)
install.packages("timeSeries")
library(timeSeries)
install.packages("sqldf")
library(sqldf)
install.packages("forecast")
library("forecast")



#ARQUIVO CORRETO EH sao_paulo

#Lendo conjunto de dados cartográficos do estado de São Paulo
dado = st_read("sao_paulo", stringsAsFactors = FALSE)
dado <- na.omit(dado) 

#Selecionando os municípios da concessão, é feio mais o que funcionou
cidade = dado[dado$NOMEMUNICP  == "ITAQUAQUECETUBA",]
cidade2 = dado[dado$NOMEMUNICP == "GUARULHOS",]
cidade3 = dado[dado$NOMEMUNICP == "FERRAZ DE VASCONCELOS",]
cidade4 = dado[dado$NOMEMUNICP == "POA",]
cidade5 = dado[dado$NOMEMUNICP == "SUZANO",]
cidade6 = dado[dado$NOMEMUNICP == "MOJI DAS CRUZES",]
cidade7 = dado[dado$NOMEMUNICP == "BIRITIBA-MIRIM",]
cidade8 = dado[dado$NOMEMUNICP == "GUARAREMA",]
cidade9 = dado[dado$NOMEMUNICP == "SALESOPOLIS",]
cidade10 = dado[dado$NOMEMUNICP == "SAO SEBASTIAO",]
cidade11 = dado[dado$NOMEMUNICP == "CARAGUATATUBA",]
cidade12 = dado[dado$NOMEMUNICP == "SANTA BRANCA",]
cidade13 = dado[dado$NOMEMUNICP == "JACAREI",]
cidade14 = dado[dado$NOMEMUNICP == "JAMBEIRO",]
cidade15 = dado[dado$NOMEMUNICP == "SAO JOSE DOS CAMPOS",]
cidade16 = dado[dado$NOMEMUNICP == "CACAPAVA",]
cidade17 = dado[dado$NOMEMUNICP == "MONTEIRO LOBATO",]
cidade18 = dado[dado$NOMEMUNICP == "TAUBATE",]
cidade19 = dado[dado$NOMEMUNICP == "TREMEMBE",]
cidade20 = dado[dado$NOMEMUNICP == "PINDAMONHANGABA",]
cidade21 = dado[dado$NOMEMUNICP == "ROSEIRA",]
cidade22 = dado[dado$NOMEMUNICP == "POTIM",]
cidade23 = dado[dado$NOMEMUNICP == "APARECIDA",]
cidade24 = dado[dado$NOMEMUNICP == "GUARATINGUETA",]
cidade25 = dado[dado$NOMEMUNICP == "LORENA",]
cidade26 = dado[dado$NOMEMUNICP == "CANAS",]
cidade27 = dado[dado$NOMEMUNICP == "CACHOEIRA PAULISTA",]
cidade28 = dado[dado$NOMEMUNICP == "CRUZEIRO",]

#Juntando todas os municípios da concessão da EDP (não achei outra forma)
edp = rbind(cidade,cidade2,cidade3,cidade4,cidade5,cidade6,cidade7,cidade8,cidade9,cidade10,cidade11,cidade12,cidade13,cidade14,cidade15,cidade16,cidade17,cidade18,cidade19,cidade20,cidade21,cidade22,cidade23,cidade24,cidade25,cidade26,cidade27,cidade28)

#Removendo meus NA
edp2 <- na.omit(edp) 

#Plotando mapa da concessão da EDP SP
plot_brmap(edp)


#Lendo meu dataset
inter = read.csv("data_interrupt.csv", header=F, sep = ",", dec = ",")

#Renomeando os campos
names(inter)[1:14] = c("ID","ACCOUNT_NUMBER","TIME_RECEIVED","MES","ANO","COMMENTS","TIPO","MOTIVO","NOME_LOGRADOURO","NOME_BAIRRO","NOME_MUNICIPIO","CEP","CLIENTE_LAT","CLIENTE_LONG")

#Fatorando o campo TIPO
inter$TIPO = factor(inter$TIPO,labels=c("FE","FFC","EF","PQ","FCP","LOI","GA","OER","E","RT","IP","PCA","RU","FC","CI","IA","RV","II","VL"))

#Removendo a primeira linha contendo os nomes dos campos
da = inter[-1,]
inter = da

#Tive que forçar transformar o ANO e MÊS em fatores pois antes as palavras ANO e MÊS por algum motivo estavam sendo
#consideradas
inter$ANO = factor(inter$ANO,labels=c("2015"))
inter$MES = factor(inter$MES,labels=c("01","02","03","04","05","06","07","08","09","10","11","12"))

########################################################################################




#Realizando um agrupamento
te = table(inter$MES, inter$ANO)
#Transformando em dataframe para ser lido pelo ggplot
te = as.data.frame(te)

#Plotando número de reclamações totais por mês durante 2015
ggplot(data = mun1) + geom_bar(aes(x = Var1, y = Freq), stat = "identity") +
  labs(title = "Número de reclamações durante o ano de 2015",
       subtitle = "Reclamações da base EDP SP",
       y = "Reclamações",
       x = "Mês") + theme_bw(base_size = 15)




newdata = rbind(m1,m2,m3,m4,m5)
newdata

#Agrupando por mes e municipio todas as reclamações
mun1 = table(inter2$MES, inter2$NOME_MUNICIPIO)

#Função para agrupar e somar as reclamações por município
mun2 = aggregate(mun1$Freq, by=list(Var2=mun1$Var2), FUN=sum)

#Ordenando por numero de reclamacoes
mun2 = mun2[order(-mun2$x),]

#Pegando os top 5 municipios com reclamacoes
topmun = mun2[1:5,]

#Plot do municipio de Guarulhos
ggplot(newdata[newdata$cidade == "GUARULHOS",]) + geom_bar(aes(x = indice, y = qtd), stat = "identity") +
  labs(title = "Reclamações do município de Guarulhos",
       y = "Reclamações",
       x = "Mês") + theme_bw(base_size = 15)


str(inter2)
########################################################################################################


ggplot(data = mun) + geom_bar(aes(x = Var2, y = Freq), stat = "identity") +
  labs(title = "Número de reclamações durante o ano de 2015",
       subtitle = "Reclamações da base EDP SP",
       y = "Reclamações",
       x = "Mês") + theme_bw(base_size = 15)


str(inter)
unique(inter$MOTIVO)



tipo = table(inter$TIPO, inter$ANO)
tipo = as.data.frame(tipo)


#Plotando número de reclamações por tipo
ggplot(data = tipo) + geom_bar(aes(x = Var1, y = Freq), stat = "identity") +
  labs(title = "Número de reclamações por tipo em 2015",
       y = "QTD",
       x = "Tipo de reclamação") + theme_bw(base_size = 15)

################################################################################################################

tp = table(inter$TIPO, inter$MES)

ef = tp[tp$Var1 == "EF",]
ef = as.data.frame(ef)

ggplot(data = ef, aes(x = ef$Var2, y = ef$Freq, group = 1)) + geom_line() + geom_point() +
labs(title = "Ditrbuição da reclamação tipo EF",
     y = "QTD",
     x = "Mês") + theme_bw(base_size = 15)

gef = ggplot(data = ef, aes(x = ef$Var2, y = ef$Freq, group = 1))

################################################################################################################
#TENTATIVA PARA GERAR O TIME SERIES VIA FUNÇÃO
x = ts(inter$TIME_RECEIVED, start = c(2015,1), frequency = 12)


gp = data.frame(date = seq.Date(from = as.Date("01/01/2015", "%d/%m/%Y"), to = as.Date("01/12/2015", "%d/%m/%Y"), by = "month"))

#Utilizei comando abaixo para remover as horas da vairavel time received
inter$TIME_RECEIVED <- format(as.POSIXct(inter$TIME_RECEIVED,format='%d/%m/%Y %H:%M:%S'),format='%Y-%m-%d')

inds <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "day")

str(inter2)
summary(inter2)

inter2$TIME_RECEIVED = as.Date(inter2$TIME_RECEIVED)

create_ts <- function(col_idx){
  ## Create a time series object
  i_ts <- as.numeric(inter2[,col_idx]) %>%
    #tsclean(replace.missing = TRUE, lambda = NULL) %>%
    ts(start = c(2015, as.numeric(format(inds[1], "%j"))),
       frequency = 365)
  return(i_ts)
}

w = ts(inter2, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.5)

qtd = inter2 %>%
  group_by(TIME_RECEIVED) %>%
  summarise(qtd_call = n())


#### UTILIZAR A PARTIR DESSE PONTO QUANDO TIVER OS DADOS DE 2016 ########
# 1 AGRUPAR OS DADOS DO ANO CORRENTE
# 2 MESCLAR OS DADOS UTILIZANDO RBIND
# 3 GERAR O TS A PARTIR DESSE MERGE


ano = table(inter$ANO,inter$MES )
ano = as.data.frame(ano)
ano = select(ano,-c(Var1))
str(ano)

ano


names(ano)[1:2] = c("Mes", "Qtd")

ano2 = ano$Freq - trunc(ano$Freq * 0.10)

ano2 = as.data.frame(ano2)

ano
ano2
ano3

head(inter)

cp = table(inter$NOME_MUNICIPIO, inter$NOME_BAIRRO )
head(cp)

cp = inter %>%
  group_by( NOME_MUNICIPIO, MES) %>%
  summarise(qtd_call = n())

head(munis)

gua = cp %>%
  dplyr::filter(NOME_MUNICIPIO == "GUARULHOS")

munis[1,]$qtd_call

#For para diminuir em 10% a quantidade de reclamacoes por ano
for(i in 1:nrow(ano)){
  ano3[i,]$Freq = ano2[i,]$Freq - trunc(ano2[i,]$Freq * 0.10)
}

munid = munis


for (i in unique(cp$NOME_MUNICIPIO)) {
  
  munis = muntotal %>%
    dplyr::filter(NOME_MUNICIPIO == i)
  
  for(u in 1:nrow(munis)){
    munis[u,]$qtd_call = munis[u,]$qtd_call - trunc(munis[u,]$qtd_call * 0.10)
  }
  
  if (cont == 0) {
    muntotal2 = munis
  } else {
    muntotal2 = rbind(muntotal2, munis)
  }
  
  cont = cont + 1
  
}

head(mun2017)
head(mun2016)
head(mun2015)

periodo = rbind(mun2015,mun2016,mun2017)
head(periodo)


munis = periodo %>%
  dplyr::filter(NOME_MUNICIPIO == "APARECIDA")

xi <-ts(munis$qtd_call,start=c(2015),freq=12)

xi
dxi = decompose(xi)

plot(xi)

trunc(x$mean)

str(x)
trunc(x$mean)

tab = as.data.frame(  trunc(x$mean) )
as_tibble(trunc(x$mean))

tab = trunc(x$mean)

tab = as.data.frame(tab)
tab

ovo = ts_df(tab)
ovo

str(periodo)

tab$time <- format(as.POSIXct(tab$time,format='%Y/%m/%d'),format='%m')

tab$time = month(tab$time)
tab$NOME_MUNICIPIO = "APARECIDA"

tab
tab = 0
tab$NOME_MUNICIPIO = "APARECIDA"
tab$qtd_call = 21

x <- data.frame("NOME_MUNICIPIO" = c("APARECIDA"), stringsAsFactors = FALSE)
x$MES = 01

head(periodo)

tabela$MES = month(tab$time)
month(tab[1,]$time)

tabela = data.frame("NOME_MUNICIPIO" = c(1:24), stringsAsFactors = FALSE)

tabela$MES = substr(tab$time, 6,7)

tabela$NOME_MUNICIPIO = "APARECIDA"
cont

head(tab)

str(tabela)

#Loop sobre os 28 municipios
for (i in unique(cp$NOME_MUNICIPIO)) {
  
  #Criando dataset novo com 24 registros referentes ao periodo previsto
  tabela = data.frame("NOME_MUNICIPIO" = c(1:24), stringsAsFactors = FALSE)

  #Setando para o municipio
  tabela$NOME_MUNICIPIO = i
  
  #Retorno dos dados de reclamacoes de determinado municipio no periodo de 3 anos
  munis = periodo %>%
    dplyr::filter(NOME_MUNICIPIO == i)
 
  #Criando timeseries desse periodo
  xi <-ts(munis$qtd_call,start=c(2015),freq=12)
  
  #Prevendo as informacoes para esse municipio
  x = forecast(xi)
  
  #Obtendo os valores previstos
  tab = trunc(x$mean)
  
  tab = ts_df(tab)
  
  #Tratando a coluna da data
  tabela$MES = substr(tab$time, 6,7)

  tabela$qtd_call = tab$value

  if (cont == 0) {
    tabelaprev = tabela
  } else {
    tabelaprev = rbind(tabelaprev, tabela)
  }
  
  cont = cont + 1
  
}

head(tabelaprev)
head(gua)


aparecida = periodo %>%
  dplyr::filter(NOME_MUNICIPIO == "APARECIDA")

aparecida

ap <-ts(aparecida$qtd_call,start=c(2015, 1),freq=12)
ap

#Holt-Winters exponential smoothing with trend and additive seasonal component.
hap = HoltWinters(ap, beta = TRUE, gamma = TRUE)

hap$fitted

hap
plot(hap)

aparecida

ap1 = decompose(ap)
plot(ap1)

head(x)
summary(x)

mun =  (tabelaprev %>%
  dplyr::filter(NOME_MUNICIPIO == "APARECIDA"))[1,]

mun$NOME_MUNICIPIO

l = (m %>%
  dplyr::filter(NOME_MUNICIPIO == "APARECIDA"))[1,] %>%
  dplyr::select(CLIENTE_LAT)

f = l[1,]

tabelaprev[1,]$LAT = 1
tabelaprev[1,]$LAT = f

head(tabelaprev)

head(tabelaprev)


for (i in 1:nrow(tabelaprev)) {
  #Peguei o nome de cada municipio
  mun =  (tabelaprev %>%
            dplyr::filter(NOME_MUNICIPIO == tabelaprev[i,]$NOME_MUNICIPIO))[1,]
  
  tabelaprev[i,]$LAT = (m %>%
      dplyr::filter(NOME_MUNICIPIO == mun$NOME_MUNICIPIO))[1,] %>%
    dplyr::select(CLIENTE_LAT)
  
  tabelaprev[i,]$LONG = (m %>%
                      dplyr::filter(NOME_MUNICIPIO == mun$NOME_MUNICIPIO))[1,] %>%
    dplyr::select(CLIENTE_LONG)
  
}

head(tabelaprev)

cp = as.data.frame(cp)
head(cp)
xi <-ts(cp$qtd_call,start=c(2015),freq=12)



ano2 = ano
a4 = rbind(ano3,ano2,ano)


x4 <-ts(a4$Freq,start=c(2015),freq=12)

plot(x4)

x4

dc = decompose(x4)
plot(dc, col = "red")

dc

hw = HoltWinters(x4, beta = FALSE, gamma = FALSE)
plot(hw)
hw$fitted
hw$fitted

sn = snaive(x4)
plot(sn)

na = naive(x4, h = 12)
plot(na)

se = ses(x4)
plot(se)

x4 %>%
  auto.arima() %>%
  forecast(h=20) %>%
  autoplot()

tab = tbats(x4)
plot(tab)

x = forecast(x4)
summary(x)
plot(x)

x$fitted
x$residuals

summary(x)
plot(forecast(x4))

summary(x4)














