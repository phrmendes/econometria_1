######Econometria I
#####Primeira aula de laborat?rio, script 1
#####Preparado por Thiago Fonseca Morello (CECS-UFABC)
#####Melhor visualizado com fonte 16 (edit--->GUI preferences, size: 16) 
###O objetivo deste roteiro de aula ? apresentar comandos b?sicos do R, comandos necess?rios
###para estima??o de uma regress?o linear m?ltipla e introduzir a interpreta??o das 
###estimativas pontuais.

###Regress?es que ser?o estimadas

#(1) Infec??es por COVID-19, g?nero e caracter?sticas socioecon?micas, ?mbito internacional

# Infec??es per capita = ?0 + ?1*Dirigente_mulher + ?2*fertilidade + ?2*polui??o + ?3*Lockdown +
#?4*Continente  + u

#Fonte: Abras, A.G., Fava, A.C.P., Kuwahara, M. Y. (2020) Women at the Top: Female Heads of State and COVID-19 Policy Responses. Feminist Economics.

#(2) Mortalidade devido a COVID-19 e caracter?sticas socioecon?micas, ?mbito internacional

# ?bitos per capita = ?0 + ?1*Dirigente_mulher + ?2*fertilidade + ?2*polui??o + ?3*Lockdown + 
#?4*Continente + ?5*Infec??es_per_capita  + u

#Fonte: Abras, Fava e Kuwahara, 2020 FEMECON.

#(3) Equa??o de Mincer

# Sal?rio hor?rio real = ?0 + ?1*educa??o + ?2*experi?ncia + ?3*setor + ?4*ocupa??o 
#+ ?5*g?nero + ?6*etnia + ?7*regi?o + ?7*?rea_urbana + ut

#Fontes:
#GIUBERTI, Ana Carolina; MENEZES-FILHO, Na?rcio. Discrimina??o de rendimentos por g?nero: uma compara??o entre o Brasil e os Estados Unidos. Econ. Apl., Ribeir?o Preto , v. 9, n. 3, p. 369-384, Set. 2005.
#Meara, K., Pastore, F., & Webster, A. (2020). The gender pay gap in the USA: a matching study. Journal of Population Economics, 33(1), 271-305.

###Limpando a ?rea de trabalho: acionar o comando abaixo com ctrl+r
rm(list=ls())

###Carregando os dados 
#Para carregar os dados, que ja est?o dispon?veis em seu computador, siga os passos
#(A) Procedimento manual 
#(1) Clique no topo da janela "R Console" acionando-a;
#(2) Selecione file--->load workspace
#(3) V? at? a pasta da desktop em que voc? salvou a pasta "metria_1_lab_1" descompactada 
#(4) Selecione o arquivo "dados_lab_1.Rdata"
#(B) Procedimento autom?tico
#Copiar o endere?o do diret?rio em que se encontra o arquivo "dados.Rdata"
#Substituir "\" por "/" no endere?o como separador das pastas
#Exemplo: setwd("D:/desk_comp/Brasa/UFABC/metria_1/2020/labs")
#Acionar o comando abaixo com ctrl+r
load("dados.Rdata")

############################(A)Prepara??o dos dados: equa??es 1 e 2

###Elimina??o de unidades de cross-section com valor n?o-observado
#? comum, em dados sociecon?micos que algumas vari?veis apresentem,
#para algumas unidades, valores n?o-observados. Em sondagens com
#aplica??o de question?rio ? pessoas ou empresas, ? o que se tem 
#quando a informa??o n?o ? concedida, i.e., o entrevistado se recusa
#a responder ou a quest?o n?o se aplica (p.ex., idade dos filhos n?o
#se aplica a fam?lia sem filhos). Em dados de pa?ses, como ? o caso da
#base db_cov, trata-se de informa??o n?o gerada pelo pa?s (p.ex., pa?ses
#em situa??o de guerra n?o possuem servi?os estat?sticos funcionando, e, 
#portanto, n?o calculam PIB e outras estat?sticas).

#Observa??o: os dados de COVID-19 s?o totais acumulados at? o dia 14 de Julho de 2020

#Os dados de COVID-19 podem ser visualizados a partir do comando "View"
#H? valores n?o-observados para diversas unidades. Tais valores s?o 
#indicados com o s?mbolo "NA" no R e podem ser visualizados
#abaixo
View(db_cov)

#P.ex., fertilidade ? n?o-observada para alguns pa?ses. 
#Para calcular o n?mero deles, basta usar o comando abaixo.
length(which(is.na(db_cov$fertility_)==TRUE))
#N?o ? observado para 19 pa?ses.

#O mesmo ? v?lido para outras vari?veis. ? necess?rio excluir as unidades (pa?ses)
#que possuem valor n?o-observado para pelo menos uma vari?vel da regress?o-alvo.
#O primeiro passo ? identificar as unidades com pelo menos uma
#vari?vel n?o observada, o que ? feito abaixo
db_covs<-subset(db_cov,is.na(casos_por_milhao)==F & is.na(obitos_por_milhao)==F & is.na(fem_head) == F & is.na(fertility_) == F & is.na(airpoll_exposure_) == F & is.na(d_lockdown) == F)

#Tamanho da base de dados remanescente
dim(db_covs)


##1.Sum?rio estat?stico das vari?veis
summary(db_covs)
#Notar que ainda h? casos de valores n?o observados, mas apenas para
#as vari?veis n?o pertencentes ?s regress?es.

#Para selecionar um subconjunto de vari?veis (colunas) utiliza-se o
#comando "subset", com argumento "select" apontado para o vetor com
#os nomes das colunas desejadas.
db_covs<-subset(db_covs, select=c(casos_por_milhao,obitos_por_milhao,fem_head,fertility_,airpoll_exposure_,d_lockdown,Continent,country))

#2.Sum?rio estat?stico das vari?veis selecionadas
summary(db_covs)
#Agora n?o h? mais valores n?o observados

##3.Visualiza??o gr?fica (dispers?o)
#mortalidade vs infec??es
plot(db_covs$casos_por_milhao,db_covs$obitos_por_milhao,xlab="Infec??es",ylab="?bitos")
#Qual ? a dire??o da rela??o entre as duas vari?veis?
##4.Coeficientes de correla??o com teste de signific?ncia
#mortalidade vs infec??es
cor.test(db_covs$casos_por_milhao,db_covs$obitos_por_milhao)

#A correla??o ? estatisticamente significativa ou n?o?

#H? contradi??o com o gr?fico de dispers?o?


#5.Estat?sticas-resumo por continente 
#O comando aggregate gera estat?sticas-resumo para grupos de observa??es
#Uma das sintaxes poss?veis do comando relaciona vari?vel a ser resumida, Y
#e vari?vel definidora de grupos, X, com a nota??o Y~X 
#A estat?stica ? indicada com o comando "FUN=estat?stica"
#Com mean = m?dia, median=mediana, sd=desvio-padr?o, etc;

#N?mero m?dio de casos per capita por continente: qual continente apresentou maior
#preval?ncia de COVID-19?
aggregate(casos_por_milhao~Continent,FUN=mean,data=db_covs)

#? o que esper?vamos?

#N?mero m?dio de ?bitos per capita por continente: qual continente apresentou maior
#mortalidade per capita
#OBS: n?o se trata da taxa de letalidade (i.e., ?bitos/casos)
aggregate(obitos_por_milhao~Continent,FUN=mean,data=db_covs)

#? o que esper?vamos?

#Taxa m?dia de letalidade: qual continente apresentou maior taxa de letalidade?
#A taxa de letalidade ? uma medida da gravidade do impacto na sa?de causado pelo COVID-19 
#ou do grau de risco ? vida imposto pela aquisi??o do v?rus
#a.gerando a vari?vel taxa de letalidade
db_covs$letalidade<-db_covs$obitos_por_milhao/db_covs$casos_por_milhao
#b.calculando a m?dia continental
aggregate(db_covs$letalidade~Continent,FUN=mean,data=db_covs)

#? o que esper?vamos?
#Qual ? a raz?o da diferen?a do ranque comparando ?s outras classifica??es?


#6.Pa?ses outliers de vari?vel dependente
#Os percentis da distribui??o emp?rica de uma vari?vel permitem identificar valores discrepantes
#da tend?ncia central (m?dia), i.e., muito pequenos ou muito grandes.
#Para obter os percentis com intervalo de 5 pontos percentuais, usar o comando quantile
quantile(db_covs$letalidade,1:20/20)

#Como refer?ncia para outliers inferiores (valores muito baixo), pode-se tomar o primeiro quinto percentil.
#Quais s?o os pa?ses com letalidade pertencente ao primeiro quinto percentil?

as.character(db_covs$country[which(db_covs$letalidade==0)])


#? o que esper?vamos?
#Qual ? a caracter?stica de geografia f?sica mais recorrente nesse grupo de paises?

#Como refer?ncia para outliers inferiores (valores muito baixo), pode-se tomar o ?ltimo quinto percentil.
#i.e., o nonag?simo-quinto percentil
#Quais s?o os pa?ses com letalidade muito alta?

as.character(db_covs$country[which(db_covs$letalidade>=0.087045428)])

#? o que esper?vamos?
#O que isso nos diz sobre o ranque continental?


############################(B)Prepara??o dos dados: equa??o 3
#1.Sele??o de vari?veis
#Apenas as vari?veis que comp?em a regress?o 3 e 4 s?o selecionadas abaixo
db_min<-subset(db_min,select=c(sal_hor,anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_bca_ama,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5,d_urbano,ano))

##2.Visualiza??o gr?fica (dispers?o)
#sal?rio hor?rio vs educa??o
plot(db_min$anos_educ[which(db_min$ano==2015)],db_min$sal_hor[which(db_min$ano==2015)],xlab="educa??o",ylab="sal?rio hor?rio",xaxt="n")
axis(1,at=0:max(db_min$anos_educ))
#Esse gr?fico de dispers?o ? peculiar por conta do car?ter discreto da vari?vel no eixo horizontal
#Para tornar mais not?ria a presen?a (ou n?o) de uma rela??o funcional entre as duas vari?veis
#? ?til incluir o gr?fico da fun??o emp?rica de expectativa condicional. 
#Para isso utiliza-se o comando aggregate, o qual gera estat?sticas para uma determinada vari?vel, Y, 
#referentes ? subgrupos amostrais definidos em fun??o de uma segunda vari?vel, X.
#As estat?sticas, no caso, m?dias, podem ser observadas abaixo.
aggregate(sal_hor~anos_educ,FUN=mean,data=db_min)
#Qual ? a rela??o revelada pela FEC emp?rica?

#3.Agora, acrescentando a FEC emp?rica no gr?fico de dispers?o, tem-se
plot(db_min$anos_educ[which(db_min$ano==2015)],db_min$sal_hor[which(db_min$ano==2015)],xlab="educa??o",ylab="sal?rio hor?rio",xaxt="n")
axis(1,at=0:max(db_min$anos_educ))
lines(aggregate(sal_hor~anos_educ,FUN=mean,data=db_min),col="red",lwd=2)

##4.Coeficientes de correla??o com teste de signific?ncia
#infec??es vs PIB
cor.test(db_min$anos_educ,db_min$sal_hor)

#Est? de acordo com o gr?fico?

####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es

###[1] Regress?o infec??es

###Vari?vel dependente
#(.) Infec??es per capita (por um milh?o de habitantes): casos_por_milhao

###Lista de explicativas (independentes)
#Bloco 1, institui??es
#(1) Pa?s ? presidido ou dirigido por mulher: fem_head
#Bloco 2, demografia
#(2) Taxa de fertilidade: fertility_ 
#[uma proxy para a capacidade de renova??o da popula??o e, pois, juventude da popula??o] 
#Bloco 4, outras causas de doen?as
#(3) Exposi??o ? polui??o atmosf?rica: airpoll_exposure_
#Bloco 5, pol?ticas anti-COVID
#(4) Lockdown foi implementado: d_lockdown
#Bloco 6, dummies de continente
#(4) Continente: Continent_


#Sobre as dummies de continente
#H? duas maneiras de incluir no R. A primeira ? autom?tica, utilizando-se o operador I(.) aplicado
#? vari?vel categ?rica ao especificar a f?rmula da regress?o
#A segunda ? gerando manualmente as dummies como segue abaixo
#tabela de categorias
table(db_covs$Continent,useNA="ifany")
db_covs$d_africa<-0
db_covs$d_africa[which(db_covs$Continent=="Africa")]<-1
db_covs$d_asia<-0
db_covs$d_asia[which(db_covs$Continent=="Asia")]<-1
db_covs$d_europe<-0
db_covs$d_europe[which(db_covs$Continent=="Europe")]<-1
db_covs$d_northamerica<-0
db_covs$d_northamerica[which(db_covs$Continent=="NorthAmer")]<-1
db_covs$d_oceania<-0
db_covs$d_oceania[which(db_covs$Continent=="Oceania")]<-1
db_covs$d_southamerica<-0
db_covs$d_southamerica[which(db_covs$Continent=="SouthAmer")]<-1

#Se temos 6 continentes de quantas dummies precisamos?
#Se, por exemplo, a Europa fosse tomada como continente de base,
#os coeficientes das dummies mediriam o efeito de cada continente 
#comparado ? Europa. I.e., a diferen?a entre um dado continente e a
#Europa no componente das infec??es n?o relacionado com as demais vari?veis.


###0.Verificar os dados
#Sempre ? recomend?vel antes de estimar a regress?o
summary(subset(db_covs))

#Importante: o que a m?dia de uma dummy representa?

###1.Especificar formula
#As dummies ser?o incorporadas automaticamente
formula<-casos_por_milhao~fem_head+fertility_+airpoll_exposure_+d_lockdown+I(Continent)


###2.Estimar o modelo
##Para isso ? utilizado o comando "lm" (linear model)", o qual roda a
#a regress?o, i.e., estima os par?metros. Os resultados s?o armazenados
#no objeto mqo_1
mqo_1<-lm(formula,data=db_covs)



###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
summary(mqo_1)

#Qual continente foi tomado por base automaticamente?

#O que se pode concluir?
#Quais vari?veis possuem rela??o positiva com o n?mero de infec??es?
#Quais vari?veis possuem rela??o negativa com o n?mero de infec??es?
#Qual ? a propor??o da varia??o da vari?vel dependente explicada?


###[2] Regress?o mortalidade

###Vari?vel dependente
#(.) ?bitos per capita (por um milh?o de habitantes): obitos_por_milhao

###Lista de explicativas (independentes)
#Bloco 1, institui??es
#(1) Pa?s ? presidido ou dirigido por mulher: fem_head
#Bloco 2, demografia
#(2) Taxa de fertilidade: fertility_ 
# [uma proxy para a capacidade de renova??o da popula??o e, pois, juventude da popula??o] 
#Bloco 4, COVID-19 e outras causas de doen?as
#(3) Exposi??o ? polui??o atmosf?rica: airpoll_exposure_
#(4) Infec??es: casos_por_milhao
#Bloco 5, pol?ticas anti-COVID
#(5) Lockdown foi implementado: d_lockdown
#Bloco 6, dummies de continente
#(6) Continente: Continent_

###0.Verificar os dados
#Sempre ? recomend?vel antes de estimar a regress?o
summary(subset(db_covs))
 

###1.Especificar formula
formula<-obitos_por_milhao~fem_head+fertility_+airpoll_exposure_+d_lockdown+I(Continent)+casos_por_milhao
 

###2.Estimar o modelo
##Para isso ? utilizado o comando "lm" (linear model)", o qual roda a
#a regress?o, i.e., estima os par?metros. Os resultados s?o armazenados
#no objeto mqo_1
mqo_2<-lm(formula,data=db_covs)


 
###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
summary(mqo_2)

#O que se pode concluir?
#Quais vari?veis possuem rela??o positiva com o n?mero de ?bitos?
#Quais vari?veis possuem rela??o negativa com o n?mero de ?bitos?
#Qual ? a propor??o da varia??o da vari?vel dependente explicada?

###Comparando as duas regress?es
cbind(?bitos=coef(mqo_2),infec??es=c(coef(mqo_1),NA))

#Quais explicativas apresentam rela??es distintas nas duas regress?es?


###[3] Regress?o Minceriana

###Vari?vel dependente
#(.) Sal?rio hor?rio
###Lista de explicativas (independentes) [igual ao exerc?cio anterior]
#Bloco 1, Educa??o e experi?ncia
#(1) Anos de estudo: anos_educ
#(2) Experi?ncia: exper_idade_trab (idade - idade em que come?ou a trabalhar)
#(3) Experi?ncia ao quadrado: exper_idade_trab_sq
#Bloco 2, Caracter?sticas pessoais
#(4) Etnia preta ou parda: etnia_pre_par
#(5) Sexo masculino: d_hom
#Bloco 3, Setor e ocupa??o
#(6) Setor de atividade: agropecu?ria, ind?stria, constru??o, Educa??o/Sa?de/Social
#Outros servi?os, administra??o p?blica; d_ativ*
#(7) Ocupa??o: administra??o, agricultura, com?rcio, outros servi?os, dirigente, ci?ncia/tecnologia
#t?cnico de n?vel m?dio, reparo/manuten??o, for?as armadas; d_ocup*
#Bloco 4, Regional
#(8) Grande regi?o brasileiras: Nordeste (2), Sudeste (3), Sul (4), Centro Oeste (5); d_reg_2-d_reg_5
#(9) ?rea urbana: d_urbano

#Perguntas importantes: 
#(A) h? cinco regi?es brasileiras: por que apenas quatro foram inclu?das?
#(B) H? a rigor dois grupos de etnia nas subamostras da PNAD consideradas, (1) brancos e amarelos e (2) pretos e pardos
#Por que um deles n?o foi considerado?
#(C) Por que a vari?vel experi?ncia ao quadrado ? inclu?da?


###0.Verificar os dados
#Sempre ? recomend?vel antes de estimar a regress?o
summary(subset(db_min,select=c(anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_bca_ama,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5,d_urbano)))


#Tamanho dos dados, 2004
dim(subset(db_min,ano==2004))
#Tamanho dos dados, 2015
dim(subset(db_min,ano==2015))


###1.Especificar formula
formula<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano

###2.Estimar o modelo: ano de 2004
mqo_3<-lm(formula,data=subset(db_min,ano==2004))

###3.Estimar o modelo: ano de 2015
mqo_4<-lm(formula,data=subset(db_min,ano==2015))

###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
cbind('2004'=coef(mqo_3),'2015'=coef(mqo_4))

#O que se pode concluir?
#Quais vari?veis possuem rela??o positiva com o sal?rio hor?rio?
#Quais vari?veis possuem rela??o negativa com o sal?rio hor?rio?
#Como interpretar o coeficiente da educa??o?
#Como interpretar o coeficiente da experi?ncia (medida em anos)?
#Como interpretar o coeficiente da etnia preta e parda?
#Como interpretar o coeficiente do g?nero masculino?
#Para quais vari?veis o coeficiente estimado difere entre 2004 e 2015?
#O retorno da educa??o aumentou?
#O retorno da experi?ncia aumentou?
#Faz sentido afirmar que h? diferencia??o regional em sal?rio hor?rio?





