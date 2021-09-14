

######Econometria I
#####Primeira aula de laboratório, script 1
#####Preparado por Thiago Fonseca Morello (CECS-UFABC)
#####Melhor visualizado com fonte 16 (edit--->GUI preferences, size: 16) 
###O objetivo deste roteiro de aula é apresentar comandos básicos do R, comandos necessários
###para estimação de uma regressão linear múltipla e introduzir a interpretação das 
###estimativas pontuais.

###Regressões que serão estimadas

#(1) Infecções por COVID-19, gênero e características socioeconômicas, âmbito internacional

# Infecções per capita = ß0 + ß1*Dirigente_mulher + ß2*fertilidade + ß2*poluição + ß3*Lockdown +
#ß4*Continente  + u

#Fonte: Abras, A.G., Fava, A.C.P., Kuwahara, M. Y. (2020) Women at the Top: Female Heads of State and COVID-19 Policy Responses. Feminist Economics.

#(2) Mortalidade devido a COVID-19 e características socioeconômicas, âmbito internacional

# Óbitos per capita = ß0 + ß1*Dirigente_mulher + ß2*fertilidade + ß2*poluição + ß3*Lockdown + 
#ß4*Continente + ß5*Infecções_per_capita  + u

#Fonte: Abras, Fava e Kuwahara, 2020 FEMECON.

#(3) Equação de Mincer

# Salário horário real = ß0 + ß1*educação + ß2*experiência + ß3*setor + ß4*ocupação 
#+ ß5*gênero + ß6*etnia + ß7*região + ß7*área_urbana + ut

#Fontes:
#GIUBERTI, Ana Carolina; MENEZES-FILHO, Naércio. Discriminação de rendimentos por gênero: uma comparação entre o Brasil e os Estados Unidos. Econ. Apl., Ribeirão Preto , v. 9, n. 3, p. 369-384, Set. 2005.
#Meara, K., Pastore, F., & Webster, A. (2020). The gender pay gap in the USA: a matching study. Journal of Population Economics, 33(1), 271-305.

###Limpando a área de trabalho: acionar o comando abaixo com ctrl+r
rm(list=ls())

###Carregando os dados 
#Para carregar os dados, que ja estão disponíveis em seu computador, siga os passos
#(A) Procedimento manual 
#(1) Clique no topo da janela "R Console" acionando-a;
#(2) Selecione file--->load workspace
#(3) Vá até a pasta da desktop em que você salvou a pasta "metria_1_lab_1" descompactada 
#(4) Selecione o arquivo "dados_lab_1.Rdata"
#(B) Procedimento automático
#Copiar o endereço do diretório em que se encontra o arquivo "dados.Rdata"
#Substituir "\" por "/" no endereço como separador das pastas
#Exemplo: setwd("D:/desk_comp/Brasa/UFABC/metria_1/2020/labs")
#Acionar o comando abaixo com ctrl+r
load("dados.Rdata")

############################(A)Preparação dos dados: equações 1 e 2

###Eliminação de unidades de cross-section com valor não-observado
#É comum, em dados socieconômicos que algumas variáveis apresentem,
#para algumas unidades, valores não-observados. Em sondagens com
#aplicação de questionário à pessoas ou empresas, é o que se tem 
#quando a informação não é concedida, i.e., o entrevistado se recusa
#a responder ou a questão não se aplica (p.ex., idade dos filhos não
#se aplica a família sem filhos). Em dados de países, como é o caso da
#base db_cov, trata-se de informação não gerada pelo país (p.ex., países
#em situação de guerra não possuem serviços estatísticos funcionando, e, 
#portanto, não calculam PIB e outras estatísticas).

#Observação: os dados de COVID-19 são totais acumulados até o dia 14 de Julho de 2020

#Os dados de COVID-19 podem ser visualizados a partir do comando "View"
#Há valores não-observados para diversas unidades. Tais valores são 
#indicados com o símbolo "NA" no R e podem ser visualizados
#abaixo
View(db_cov)

#P.ex., fertilidade é não-observada para alguns países. 
#Para calcular o número deles, basta usar o comando abaixo.
length(which(is.na(db_cov$fertility_)==TRUE))
#Não é observado para 19 países.

#O mesmo é válido para outras variáveis. É necessário excluir as unidades (países)
#que possuem valor não-observado para pelo menos uma variável da regressão-alvo.
#O primeiro passo é identificar as unidades com pelo menos uma
#variável não observada, o que é feito abaixo
db_covs<-subset(db_cov,is.na(casos_por_milhao)==F & is.na(obitos_por_milhao)==F & is.na(fem_head) == F & is.na(fertility_) == F & is.na(airpoll_exposure_) == F & is.na(d_lockdown) == F)

#Tamanho da base de dados remanescente
dim(db_covs)


##1.Sumário estatístico das variáveis
summary(db_covs)
#Notar que ainda há casos de valores não observados, mas apenas para
#as variáveis não pertencentes às regressões.

#Para selecionar um subconjunto de variáveis (colunas) utiliza-se o
#comando "subset", com argumento "select" apontado para o vetor com
#os nomes das colunas desejadas.
db_covs<-subset(db_covs, select=c(casos_por_milhao,obitos_por_milhao,fem_head,fertility_,airpoll_exposure_,d_lockdown,Continent,country))

#2.Sumário estatístico das variáveis selecionadas
summary(db_covs)
#Agora não há mais valores não observados

##3.Visualização gráfica (dispersão)
#mortalidade vs infecções
plot(db_covs$casos_por_milhao,db_covs$obitos_por_milhao,xlab="Infecções",ylab="Óbitos")
#Qual é a direção da relação entre as duas variáveis?

##4.Coeficientes de correlação com teste de significância
#mortalidade vs infecções
cor.test(db_covs$casos_por_milhao,db_covs$obitos_por_milhao)

#A correlação é estatisticamente significativa ou não?

#Há contradição com o gráfico de dispersão?


#5.Estatísticas-resumo por continente 
#O comando aggregate gera estatísticas-resumo para grupos de observações
#Uma das sintaxes possíveis do comando relaciona variável a ser resumida, Y
#e variável definidora de grupos, X, com a notação Y~X 
#A estatística é indicada com o comando "FUN=estatística"
#Com mean = média, median=mediana, sd=desvio-padrão, etc;

#Número médio de casos per capita por continente: qual continente apresentou maior
#prevalência de COVID-19?
aggregate(casos_por_milhao~Continent,FUN=mean,data=db_covs)

#É o que esperávamos?

#Número médio de óbitos per capita por continente: qual continente apresentou maior
#mortalidade per capita
#OBS: não se trata da taxa de letalidade (i.e., óbitos/casos)
aggregate(obitos_por_milhao~Continent,FUN=mean,data=db_covs)

#É o que esperávamos?

#Taxa média de letalidade: qual continente apresentou maior taxa de letalidade?
#A taxa de letalidade é uma medida da gravidade do impacto na saúde causado pelo COVID-19 
#ou do grau de risco à vida imposto pela aquisição do vírus
#a.gerando a variável taxa de letalidade
db_covs$letalidade<-db_covs$obitos_por_milhao/db_covs$casos_por_milhao
#b.calculando a média continental
aggregate(db_covs$letalidade~Continent,FUN=mean,data=db_covs)

#É o que esperávamos?
#Qual é a razão da diferença do ranque comparando às outras classificações?


#6.Países outliers de variável dependente
#Os percentis da distribuição empírica de uma variável permitem identificar valores discrepantes
#da tendência central (média), i.e., muito pequenos ou muito grandes.
#Para obter os percentis com intervalo de 5 pontos percentuais, usar o comando quantile
quantile(db_covs$letalidade,1:20/20)

#Como referência para outliers inferiores (valores muito baixo), pode-se tomar o primeiro quinto percentil.
#Quais são os países com letalidade pertencente ao primeiro quinto percentil?

as.character(db_covs$country[which(db_covs$letalidade==0)])


#É o que esperávamos?
#Qual é a característica de geografia física mais recorrente nesse grupo de paises?

#Como referência para outliers inferiores (valores muito baixo), pode-se tomar o último quinto percentil.
#i.e., o nonagésimo-quinto percentil
#Quais são os países com letalidade muito alta?

as.character(db_covs$country[which(db_covs$letalidade>=0.087045428)])

#É o que esperávamos?
#O que isso nos diz sobre o ranque continental?


############################(B)Preparação dos dados: equação 3
#1.Seleção de variáveis
#Apenas as variáveis que compõem a regressão 3 e 4 são selecionadas abaixo
db_min<-subset(db_min,select=c(sal_hor,anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_bca_ama,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5,d_urbano,ano))

##2.Visualização gráfica (dispersão)
#salário horário vs educação
plot(db_min$anos_educ[which(db_min$ano==2015)],db_min$sal_hor[which(db_min$ano==2015)],xlab="educação",ylab="salário horário",xaxt="n")
axis(1,at=0:max(db_min$anos_educ))
#Esse gráfico de dispersão é peculiar por conta do caráter discreto da variável no eixo horizontal
#Para tornar mais notória a presença (ou não) de uma relação funcional entre as duas variáveis
#é útil incluir o gráfico da função empírica de expectativa condicional. 
#Para isso utiliza-se o comando aggregate, o qual gera estatísticas para uma determinada variável, Y, 
#referentes à subgrupos amostrais definidos em função de uma segunda variável, X.
#As estatísticas, no caso, médias, podem ser observadas abaixo.
aggregate(sal_hor~anos_educ,FUN=mean,data=db_min)
#Qual é a relação revelada pela FEC empírica?

#3.Agora, acrescentando a FEC empírica no gráfico de dispersão, tem-se
plot(db_min$anos_educ[which(db_min$ano==2015)],db_min$sal_hor[which(db_min$ano==2015)],xlab="educação",ylab="salário horário",xaxt="n")
axis(1,at=0:max(db_min$anos_educ))
lines(aggregate(sal_hor~anos_educ,FUN=mean,data=db_min),col="red",lwd=2)

##4.Coeficientes de correlação com teste de significância
#infecções vs PIB
cor.test(db_min$anos_educ,db_min$sal_hor)

#Está de acordo com o gráfico?

####Regressões####Regressões####Regressões####Regressões####Regressões####Regressões####Regressões####Regressões

###[1] Regressão infecções

###Variável dependente
#(.) Infecções per capita (por um milhão de habitantes): casos_por_milhao

###Lista de explicativas (independentes)
#Bloco 1, instituições
#(1) País é presidido ou dirigido por mulher: fem_head
#Bloco 2, demografia
#(2) Taxa de fertilidade: fertility_ 
[uma proxy para a capacidade de renovação da população e, pois, juventude da população] 
#Bloco 4, outras causas de doenças
#(3) Exposição à poluição atmosférica: airpoll_exposure_
#Bloco 5, políticas anti-COVID
#(4) Lockdown foi implementado: d_lockdown
#Bloco 6, dummies de continente
#(4) Continente: Continent_


#Sobre as dummies de continente
#Há duas maneiras de incluir no R. A primeira é automática, utilizando-se o operador I(.) aplicado
#à variável categórica ao especificar a fórmula da regressão
#A segunda é gerando manualmente as dummies como segue abaixo
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
#comparado à Europa. I.e., a diferença entre um dado continente e a
#Europa no componente das infecções não relacionado com as demais variáveis.


###0.Verificar os dados
#Sempre é recomendável antes de estimar a regressão
summary(subset(db_covs))

#Importante: o que a média de uma dummy representa?

###1.Especificar formula
#As dummies serão incorporadas automaticamente
formula<-casos_por_milhao~fem_head+fertility_+airpoll_exposure_+d_lockdown+I(Continent)


###2.Estimar o modelo
##Para isso é utilizado o comando "lm" (linear model)", o qual roda a
#a regressão, i.e., estima os parâmetros. Os resultados são armazenados
#no objeto mqo_1
mqo_1<-lm(formula,data=db_covs)



###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
summary(mqo_1)

#Qual continente foi tomado por base automaticamente?

#O que se pode concluir?
#Quais variáveis possuem relação positiva com o número de infecções?
#Quais variáveis possuem relação negativa com o número de infecções?
#Qual é a proporção da variação da variável dependente explicada?


###[2] Regressão mortalidade

###Variável dependente
#(.) Óbitos per capita (por um milhão de habitantes): obitos_por_milhao

###Lista de explicativas (independentes)
#Bloco 1, instituições
#(1) País é presidido ou dirigido por mulher: fem_head
#Bloco 2, demografia
#(2) Taxa de fertilidade: fertility_ 
[uma proxy para a capacidade de renovação da população e, pois, juventude da população] 
#Bloco 4, COVID-19 e outras causas de doenças
#(3) Exposição à poluição atmosférica: airpoll_exposure_
#(4) Infecções: casos_por_milhao
#Bloco 5, políticas anti-COVID
#(5) Lockdown foi implementado: d_lockdown
#Bloco 6, dummies de continente
#(6) Continente: Continent_

###0.Verificar os dados
#Sempre é recomendável antes de estimar a regressão
summary(subset(db_covs))
 

###1.Especificar formula
formula<-obitos_por_milhao~fem_head+fertility_+airpoll_exposure_+d_lockdown+I(Continent)+casos_por_milhao
 

###2.Estimar o modelo
##Para isso é utilizado o comando "lm" (linear model)", o qual roda a
#a regressão, i.e., estima os parâmetros. Os resultados são armazenados
#no objeto mqo_1
mqo_2<-lm(formula,data=db_covs)


 
###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
summary(mqo_2)

#O que se pode concluir?
#Quais variáveis possuem relação positiva com o número de óbitos?
#Quais variáveis possuem relação negativa com o número de óbitos?
#Qual é a proporção da variação da variável dependente explicada?

###Comparando as duas regressões
cbind(óbitos=coef(mqo_2),infecções=c(coef(mqo_1),NA))

#Quais explicativas apresentam relações distintas nas duas regressões?


###[3] Regressão Minceriana

###Variável dependente
#(.) Salário horário
###Lista de explicativas (independentes) [igual ao exercício anterior]
#Bloco 1, Educação e experiência
#(1) Anos de estudo: anos_educ
#(2) Experiência: exper_idade_trab (idade - idade em que começou a trabalhar)
#(3) Experiência ao quadrado: exper_idade_trab_sq
#Bloco 2, Características pessoais
#(4) Etnia preta ou parda: etnia_pre_par
#(5) Sexo masculino: d_hom
#Bloco 3, Setor e ocupação
#(6) Setor de atividade: agropecuária, indústria, construção, Educação/Saúde/Social
#Outros serviços, administração pública; d_ativ*
#(7) Ocupação: administração, agricultura, comércio, outros serviços, dirigente, ciência/tecnologia
#técnico de nível médio, reparo/manutenção, forças armadas; d_ocup*
#Bloco 4, Regional
#(8) Grande região brasileiras: Nordeste (2), Sudeste (3), Sul (4), Centro Oeste (5); d_reg_2-d_reg_5
#(9) Àrea urbana: d_urbano

#Perguntas importantes: 
#(A) há cinco regiões brasileiras: por que apenas quatro foram incluídas?
#(B) Há a rigor dois grupos de etnia nas subamostras da PNAD consideradas, (1) brancos e amarelos e (2) pretos e pardos
#Por que um deles não foi considerado?
#(C) Por que a variável experiência ao quadrado é incluída?


###0.Verificar os dados
#Sempre é recomendável antes de estimar a regressão
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
#Quais variáveis possuem relação positiva com o salário horário?
#Quais variáveis possuem relação negativa com o salário horário?
#Como interpretar o coeficiente da educação?
#Como interpretar o coeficiente da experiência (medida em anos)?
#Como interpretar o coeficiente da etnia preta e parda?
#Como interpretar o coeficiente do gênero masculino?
#Para quais variáveis o coeficiente estimado difere entre 2004 e 2015?
#O retorno da educação aumentou?
#O retorno da experiência aumentou?
#Faz sentido afirmar que há diferenciação regional em salário horário?





