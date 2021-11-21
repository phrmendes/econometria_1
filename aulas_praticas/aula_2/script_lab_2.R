
######Econometria I
#####Segunda aula de laborat?rio, script 2
#####Preparado por Thiago Fonseca Morello (CECS-UFABC)
#####Melhor visualizado com fonte 16 (edit--->GUI preferences, size: 16) 
###O objetivo deste roteiro de aula ? apresentar exemplos de testes de signific?ncia
#individual, conjunta e global para regress?o m?ltipla. 
#S?o utilizados os mesmos exemplos do laborat?rio 1, como segue abaixo.

###Regress?es que ser?o estimadas

#(1) Equa??o de Mincer

# Sal?rio hor?rio real = ?0 + ?1*educa??o + ?2*experi?ncia + ?3*setor + ?4*ocupa??o 
#+ ?5*g?nero + ?6*etnia + ?7*regi?o + ?7*?rea_urbana + ut

#Fontes:
#GIUBERTI, Ana Carolina; MENEZES-FILHO, Na?rcio. Discrimina??o de rendimentos por g?nero: uma compara??o entre o Brasil e os Estados Unidos. Econ. Apl., Ribeir?o Preto , v. 9, n. 3, p. 369-384, Set. 2005.
#Meara, K., Pastore, F., & Webster, A. (2020). The gender pay gap in the USA: a matching study. Journal of Population Economics, 33(1), 271-305.

#(1) Infec??es por COVID-19, g?nero e caracter?sticas socioecon?micas, ?mbito internacional

# Infec??es per capita = ?0 + ?1*Dirigente_mulher + ?2*fertilidade + ?2*polui??o + ?3*Lockdown +
#?4*Continente  + u

#Fonte: Abras, A.G., Fava, A.C.P., Kuwahara, M. Y. (2020) Women at the Top: Female Heads of State and COVID-19 Policy Responses. Feminist Economics.

#(2) Mortalidade devido a COVID-19 e caracter?sticas socioecon?micas, ?mbito internacional

# ?bitos per capita = ?0 + ?1*Dirigente_mulher + ?2*fertilidade + ?2*polui??o + ?3*Lockdown + 
#?4*Continente + ?5*Infec??es_per_capita  + u

#Fonte: Abras, Fava e Kuwahara, 2020 FEMECON.


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


####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es####Regress?es

###[1] Regress?o Minceriana

###Relembrando vari?veis da regress?o

###Vari?vel dependente
#(.) Sal?rio hor?rio
###Lista de explicativas (independentes)
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

###0.Sum?rio estat?stico das vari?veis
summary(subset(db_min,select=c(anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_bca_ama,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5)))

###1.F?rmula da regress?o
formula<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano

###2.Regress?o 1: 2004
mqo_1<-lm(formula,data=subset(db_min,ano==2004))

###3.Regress?o 2: 2015
mqo_2<-lm(formula,data=subset(db_min,ano==2015))

###4.Compreendendo a janela de sa?da do R com os resultados de estima??o
#O comando summary(mqo_1) apresenta o conjunto completo de estat?sticas e testes referentes ? regress?o estimada
summary(mqo_1)

#Na primeira coluna, "Estimate", temos as estimativas pontuais dos par?metros
#Na segunda coluna, "Std.Error", temos as estimativas pontuais dos erros-padr?o dos estimadores
#Na terceira coluna, "t value", h? o valor observado da estat?stica do teste signific?ncia individual
#de cada vari?vel independente
#O que temos ent?o na quarta coluna? Qual ? o significado de P(>|t|)?
#Na parte inferior, h?:
#(a) Residual standard error, acompanhando graus de liberdade: do que se trata?
#(b) Multiple R-squared? O que ? isso?
#(c) Adjusted R-squared? Qual ? a diferen?a em rela??o ao anterior?
#(d) F-statistic, com graus de liberdade: trata-se da estat?stica de qual teste?
#(e) p-valor para a estat?stica F

###5.Teste de signific?ncia individual: educa??o
#5.a) educa??o
#As hip?teses do teste s?o:
#H0: b(k) = 0 vs H1: b(k) != 0 [teste bilateral]
#Sendo b(k) o valor populacional do coeficiente da k-?sima vari?vel
#A estat?stica do teste individual ?:
#estimativa pontual / erro padr?o = Estimate / Std. Error
#Recordando que o erro padr?o ? a raiz do estimador consistente da vari?ncia do estimador

#(i) obtendo a estimativa pontual para a k-?sima explicativa
#As estimativas pontuais s?o armazenadas no objeto mqo_1
#Para observ?-las, basta utilizar a fun??o coef(.)
coef(mqo_1)
#Para observar apenas o k-?simo elemento do vetor de coeficientes, basta acrescentar o ?ndice "[k+1]"
#P.ex., para obter o intercepto
coef(mqo_1)[1]
#Agora, para obter a pontual do coeficiente da primeira explicativa, educa??o:
coef(mqo_1)[2]

#(ii) obtendo o erro padr?o do estimador para a k-?sima explicativa 
#As vari?ncias dos estimadores ficam armazenadas na matriz de vari?ncia-covari?ncia
#Essa pode ser obtida utilizando-se a fun??o vcov
vcov(mqo_1)
#Como a matriz ? grande, de tamanho (K+1) X (K+1), no caso 26 x 26, 
#? melhor usar o comando View
View(vcov(mqo_1))


#? correto que alguns elementos da matriz de vari?ncia-covari?ncia sejam negativos?

#Vamos visualizar a matriz de vari?ncia-covari?ncia.
#Para isso, n?mero negativos ser?o pintados de vermelho
#E positivos, de azul
#Primeiramente ? preciso instalar o pacote raster no hard disk.
#Para isso, retire o "#" da frente da linha abaixo e tecle ctrl+r
#install.packages("raster")
#Selecione um dos mirrors, p.ex., SP1
#Uma vez instalado, reestabele?a, acima, o "#" que precedia o comando "install.packages("raster")"
#para evitar de acionar novamente a instala??o na pr?xima vez que rodar esse script.
#Acionando o pacote
library(raster)
#Gerando a paleta de cores
colfunc<-colorRampPalette(c("red","blue"))
#Gerando o gr?fico
plot(raster(vcov(mqo_1)),col=colfunc(2),breaks=c(min(vcov(mqo_1)),0,max(vcov(mqo_1))),legend=F,xlim=c(0,1),ylim=c(0,1))
lines(seq(0,1,by=0.1),1-seq(0,1,by=0.1),lwd=6)

#O primeiro padr?o not?rio ? que h? apenas n?meros positivos na diagonal principal
#Por que isso?
#Qual estat?stica est? na diagonal principal da matriz de vari?ncia-covari?ncia?
#A implica??o do primeiro padr?o ? que os n?meros negativos est?o fora da diagonal principal
#? correto isso? O que temos fora da diagonal principal da matriz?

#Notar a simetria em torno da diagonal principal (ou seja, a diagonal principal ? como um espelho).
#Ser? que uma matriz de covari?ncia-vari?ncia ? sempre sim?tria?

#O erro padr?o referente ao k-?simo coeficiente ? o k+1-?simo termo da diagonal principal 
#da matriz de vari?ncia-covari?ncia. Basta utilizar a fun??o diag com ?ndice k+1
#P.ex., a vari?ncia do estimador para o coeficiente da primeira explicativa, educa??o, ?:
diag(vcov(mqo_1))[2]

#(iii) Estat?stica t:
#valor observado = estimativa pontual / erro padr?o = Estimate / Std. Error
coef(mqo_1)[2]/sqrt(diag(vcov(mqo_1))[2])
#Notar a fun??o sqrt no denominador que aplica a raiz
#Este ? o valor da estat?stica do teste.
#Armazenando no objeto t_obs
t_obs<-coef(mqo_1)[2]/sqrt(diag(vcov(mqo_1))[2])

#Obtendo a regi?o cr?tica para o teste de signific?ncia da educa??o
#A estat?stica tem uma distribui??o assint?tica t de Student com N-(K+1) graus de liberdade 
#N = n?mero de observa??es = n?mero de linhas da matriz de dados
nrow(subset(db_min,ano==2004))
#K + 1 = n?mero de par?metros (contando com o intercepto)
length(coef(mqo_1))
#Logo N - (K+1) = 93106-26
93106-26
#Armazenando no objeto gl
gl<-93106-26
#Os valores cr?ticos s?o, pois, os limiares que cont?m entre si 95% de probabilidade na
#distribui??o t com 93080 graus de liberdade
qt(0.025,gl)
#Os valores cr?ticos s?o, pois, ~ -1.96
#A regi?o cr?tica ?, pois, (-inf;-1.96] U (1.96,+inf)
#Em que inf = infinito

#Obtendo o p-valor para o teste de signific?ncia da educa??o
#O p-valor ? a probabilidade de ocorrer valor mais extremo do que o observado para a estat?stica
#Basta utilizar a fun??o pt, que ? a fun??o de distribui??o t acumulada
#Lembrando-se que, em um teste bilateral, o p-valor ? calculado como
#2*P(T<-|t_obs|)
2*pt(-abs(t_obs),gl)
#O p-valor ? nulo

#Decis?o quanto ? signific?ncia individual da educa??o
#O valor observado da estat?stica ?
t_obs
#A regi?o cr?tica ? (-inf;-1.96] U (1.96,+inf)
#O valor observado pertence ? regi?o cr?tica?
#Qual ? a decis?o correta, rejeitar ou n?o a hip?tese nula?
#O p-valor foi inferior ? 5%?
#Qual ? a decis?o correta, rejeitar ou n?o a hip?tese nula?

#O que se conclui quanto ? educa??o: tal fator mostrou-se influente na remunera??o?


###6.Teste de signific?ncia individual: g?nero masculino

#6.a Estimativa pontual
#Para obt?-la, ? preciso saber qual ? a posi??o do g?nero masculino no vetor de estimativas pontuais
#O comando abaixo numera o elementos do vetor
cbind(1:26,coef(mqo_1))
#Trata-se da sexta posi??o, ou seja, a pontual desejada ?:
coef(mqo_1)[6]

#6.b Erro padr?o
#A matriz de vari?ncia covari?ncia est? estruturada de maneira a que o elemento
#na j-?sima coluna e na j-?sima linha seja a vari?ncia do estimador do (j-1)-?simo coeficiente
#Notar que, como o intercepto est? na primeira posi??o do vetor de pontuais, a vari?ncia
#correspondente est? na primeira linha e na primeira coluna. Por isso, a vari?ncia do primeiro
#coeficiente est? na segunda linha e na segunda coluna.
#Com isso, como a explicativa ? a quinta (ver lista acima) a vari?ncia desejada ?
diag(vcov(mqo_1))[6]
#Notar que se trata do mesmo ?ndice da pontual

#6.c Estat?stica t:
#valor observado = estimativa pontual / erro padr?o = Estimate / Std. Error
coef(mqo_1)[6]/sqrt(diag(vcov(mqo_1))[6])
#Notar a fun??o sqrt no denominador que aplica a raiz
#Este ? o valor da estat?stica do teste.
t_obs<-coef(mqo_1)[6]/sqrt(diag(vcov(mqo_1))[6])

#Obtendo a regi?o cr?tica para o teste de signific?ncia da educa??o
#O n?mero de graus de liberdade ? o mesmo para todos os coeficientes e por
#isso j? foi calculado.
#Gerando novamente de maneira mais autom?tica
gl<-nrow(subset(db_min,ano==2004))-length(coef(mqo_1))
#Os valores cr?ticos obtidos da seguinte forma:
qt(0.025,gl)
#Os valores cr?ticos s?o, pois, ~ -1.96
#A regi?o cr?tica ?, pois, (-inf;-1.96] U (1.96,+inf)
#Em que inf = infinito

#Obtendo o p-valor para o teste de signific?ncia da educa??o: 2*P(T<-|t_obs|)
2*pt(-abs(t_obs),gl)
#Ser? que esse n?mero ? pequeno ou grande?

#Decis?o quanto ? signific?ncia individual da educa??o
#O valor observado da estat?stica ?
t_obs
#A regi?o cr?tica ? (-inf;-1.96] U (1.96,+inf)
#O valor observado pertence ? regi?o cr?tica?
#Qual ? a decis?o correta, rejeitar ou n?o a hip?tese nula?
#O p-valor foi inferior ? 5%?
#Qual ? a decis?o correta, rejeitar ou n?o a hip?tese nula?

#O que se conclui quanto ao g?nero masculino: tal fator mostrou-se influente na remunera??o?

#Qual conclus?o se pode retirar quanto ao funcionamento do mercado de trabalho?


###7.Visualizando os testes de signific?ncia para todos os coeficientes a partir da janela de sa?da do R
##Todas as estat?sticas necess?rias para a tomada de decis?o quanto aos testes de signific?ncia individual 
#de todos os par?metros podem ser geradas aplicando-se o procedimento anterior ao vetor de estimativas pontuais
#e justapondo horizontalmente todos os vetores de estat?sticas, como no comando abaixo.
cbind(Estimate=coef(mqo_1),Std_Err=diag(vcov(mqo_1)),t_stat=coef(mqo_1)/sqrt(diag(vcov(mqo_1))),p_value=2*pt(-abs(coef(mqo_1)/sqrt(diag(vcov(mqo_1)))),gl))

#Agora vamos tomar a decis?o em rela??o ? todas as explicativas unicamente com base no p-valor, esta
#a estat?stica mais comumente utilizada para tanto na pr?tica econom?trica.
#Vamos colocar a tabela no Excel para poder editar


#Qual ? a pergunta que devemos fazer ao p-valor para estabelecer signific?ncia, i.e., rejei??o da hip?tese de nulidade?
#Em uma primeira coluna do Excel, vamos anotar exclusivamente signific?ncia

#Agora, em uma segunda coluna, anotamos anotamos o sinal das explicativas significativas
#Notar que o sinal das explicativas n?o significativas ? zero, pois essa ? a influ?ncia que o teste relevou que elas t?m populacionalmente.

###8.Teste de signific?ncia global
#A estat?stica do teste ?:
#F = {R2/K}/{(1-R2)/(N-K-1)}
#(notar que esta ? a ?nica estat?stica de teste; n?o ? poss?vel, como no 
#caso do teste de restri??es de exclus?o, obter uma estat?stica a partir
#das SQRs dos modelos restrito e irrestrito)

#Encontrando o R2 na sa?da do R
#Nota: trata-se do R ordin?rio n?o-ajustado
summary(mqo_1)
#A estat?stica buscada ? o "Multiple R-squared"
#Valor = 0.3932
r2<-0.3932
#Calculando a estat?stica:
#(a) K + 1 = length(coef(mqo_1)) --> K = length(coef(mqo_1))-1
gl_num<-length(coef(mqo_1))-1
#(b) N-(K+1) = nrow(subset(db_min,ano==2004)) - length(coef(mqo_1))
gl_den<-nrow(subset(db_min,ano==2004)) - length(coef(mqo_1))
#(c) R2
f<-(r2/gl_num)/((1-r2)/gl_den)
f
#(d) p-valor com base na distribui??o F
#O teste ser? realizado como unilateral, 
#tomando como p-valor a probabilidade de observar um valor
#mais positivo do que o observado.
#O que requer introduzir a op??o "lower.tail=F" na FDA reportada
#pelo R. O comando ? pf(f_obs,gl_num,gl_den,lower.tail=F)
pf(f,gl_num,gl_den,lower.tail=F)
#(e) comparando com a sa?da do R
#A ?ltima linha da sa?da do R possui a estat?stica F, sendo informado
#(i) o valor da estat?stica, (2) o n?mero de gl do numerador, (3) o 
#n?mero de gl do denominador e (4) o p-valor
#Vamos conferir os quatro
summary(mqo_1)
#(e.1) valor observado da estat?stica
f
#Ok, o valor da sa?da ? aproximado pois n?o cont?m os decimais
#(e.2) gl num
gl_num
#ok, ? o primeiro gl na sa?da do R
#(e.3) gl denom
gl_den
#ok, ? o segundo gl na sa?da do R
#(e.4) p-valor: ? a ?ltima informa??o
#tem-se o menor n?mero que o R ? capaz de reportar e, 
#portanto, est? de acordo com o p-valor nulo obtido.

#Qual ? a decis?o correta: rejeitar ou n?o a hip?tese nula de que o modelo n?o explica a vari?vel dependente?

###9.Teste de signific?ncia conjunta: irrelev?ncia de grupos de vari?veis explicativas
#9.a) Bloco 3, Setor e ocupa??o
#Estat?stica do teste: nesse caso h? duas vers?es:
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
#SQR_R = SQR do modelo restrito, i.e., sem as vari?veis cuja signific?ncia conjunta se deseja testar
#SQR_IR = SQR do modelo irrestrito, com todas as explicativas
#s = n?mero de explicativas cuja signific?ncia ? testada
#(2) {(R2_IR-R2_R)/s}/{(1-R2_IR)/(N-K-1)}
#recordando que SQR = (1-R2)SQT
#Aten??o: a subtra??o no numerador est? "invertida" comparando-se (1) e (2)
#(9.b) Calculando a primeira vers?o
#SQR_R: ? necess?rio rodar a regress?o restrita
#A formula abaixo j? exclui as vari?veis de atividade e ocupa??o
formula_R<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Estima??o
mqo_R<-lm(formula_R,data=subset(db_min,ano==2004))
#Obtendo a SQR:
summary(mqo_R)
#A SQR n?o est? dispon?vel a partir da janela de sa?da, ? preciso calcular somando os quadrados dos res?duos, que ficam
#armazenados no elemento "residuals"
SQR_R<-sum(mqo_R$residuals^2)
#Obtendo SQR_IR
SQR_IR<-sum(mqo_1$residuals^2)
#Obtendo o n?mero de explicativas-alvo do teste como diferen?a do n?mero de par?metros dos dois modelos
length(coef(mqo_1))-length(coef(mqo_R))
#Contando manualmente para conferir
length(c("d_ativ_agro","d_ativ_indus","d_ativ_const","d_ativ_social","d_ativ_serv","d_ativ_apub","d_ocup_adm","d_ocup_agro","d_ocup_com","d_ocup_serv","d_ocup_dirigente","d_ocup_cie_art","d_ocup_tec_med","d_ocup_rep_manut","d_ocup_armada"))
#OK
s<-length(coef(mqo_1))-length(coef(mqo_R))
#Calculando a estat?stica
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
f1<-((SQR_R-SQR_IR)/s)/(SQR_IR/gl_den)
f1
#notar que gl_den = N - K - 1 j? havia sido obtido antes
##A regi?o cr?tica unilateral pode ser obtida como segue
#Valor cr?tico superior
qf(1-0.025,s,gl_den)
#RC = {1.832,inf}
#p-valor
pf(f1,s,gl_den,lower.tail=F)
#Qual ? a decis?o correta: rejeitar ou n?o a hip?tese nula de contribui??o irrelevante das vari?veis de atividade e ocupa??o?

#(9.c) Calculando a segunda vers?o
#Obtendo o R2 do modelo restrito (Aten??o: trata-se do R2 n?o-ajustado)
summary(mqo_R)
R2_R<-0.3011
#Obtendo o R2 do modelo irrestrito
summary(mqo_1)
R2_IR<-0.3932
#Calculando a estat?stica do teste:
#{(R2_IR-R2_R)/s}/{(1-R2_IR)/(N-K-1)}
f2<-((R2_IR-R2_R)/s)/((1-R2_IR)/gl_den)
f2
##Comparando as duas estat?sticas
#As duas estat?sticas t?m de ser equivalentes, uma vez que s?o algebricamente iguais
f1
f2
#H? um erro de arredondamento, provavelmente devido ao uso do R2 tal como reportado na janela de sa?da e,
#pois, com poucas casas decimais.

###10.Intervalos de confian?a para coeficientes
#A f?rmula do IC ?:
#{b(k)-t(0.025)DP(k);b(k)-t(0.025)DP(k)}
#10.1 coeficiente da educa??o
#10.1.a)limiares de confian?a = t(0.025)
#Os limiares de confian?a s?o valores entre os quais h? probabilidade equivalente ao n?vel de
#signific?ncia, nesse caso de 95%;
#Basta utilizar a fun??o qt((1-NC)/2,gl), NC = 0.95
qt(0.025,gl_den)
#Nota: como o limiar ? negativo, o IC ser? calculado com a seguinte adapta??o:
#{b(k)+t(0.025)DP(k);b(k)-t(0.025)DP(k)}
#Recordando que gl_den = N-K-1, como calculado no teste de signific?ncia global
#Os demais elementos, incluindo estimativa pontual e erro padr?o do estimador s?o obtidos da mesma
#maneira que para o teste de signific?ncia individual
#segue abaixo o IC
c(coef(mqo_1)[2]+qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))[2]),coef(mqo_1)[2]-qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))[2]))

#Janela de sa?da com todas as estat?sticas relevantes
cbind(Estimate=coef(mqo_1),Std_Err=diag(vcov(mqo_1)),t_stat=coef(mqo_1)/sqrt(diag(vcov(mqo_1))),p_value=2*pt(-abs(coef(mqo_1)/sqrt(diag(vcov(mqo_1)))),gl),CI_l=coef(mqo_1)+qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))),CI_h=coef(mqo_1)-qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))))

#O que se pode concluir?
#Quais vari?veis possuem rela??o positiva com o n?mero de ?bitos?
#Quais vari?veis possuem rela??o negativa com o n?mero de ?bitos?
#Quais vari?veis possuem rela??o nula com o n?mero de ?bitos?


##Regress?o Minceria para 2015
summary(mqo_2)

#O que se pode concluir?
#Quais vari?veis possuem rela??o positiva com a remunera??o?
#Quais vari?veis possuem rela??o negativa com a remunera??o?
#Quais vari?veis possuem rela??o nula com a remunera??o?



###[2] Regress?o infec??es vs g?nero e socioecon?micos

###prepara??o dos dados
db_covs<-subset(db_cov,is.na(casos_por_milhao)==F & is.na(obitos_por_milhao)==F & is.na(fem_head) == F & is.na(fertility_) == F & is.na(airpoll_exposure_) == F & is.na(d_lockdown) == F)
db_covs<-subset(db_covs, select=c(casos_por_milhao,obitos_por_milhao,fem_head,fertility_,airpoll_exposure_,d_lockdown,Continent,country))


###Vari?vel dependente
#(.) Infec??es per capita (por um milh?o de habitantes): casos_por_milhao

###Lista de explicativas (independentes)
#Bloco 1, institui??es
#(1) Pa?s ? presidido ou dirigido por mulher: fem_head
#Bloco 2, demografia
#(2) Taxa de fertilidade: fertility_ 
# [uma proxy para a capacidade de renova??o da popula??o e, pois, juventude da popula??o] 
#Bloco 4, outras causas de doen?as
#(3) Exposi??o ? polui??o atmosf?rica: airpoll_exposure_
#Bloco 5, pol?ticas anti-COVID
#(4) Lockdown foi implementado: d_lockdown
#Bloco 6, dummies de continente
#(4) Continente: Continent_


###1.Especificar formula
formula<-casos_por_milhao~fem_head+fertility_+airpoll_exposure_+d_lockdown+I(Continent)


###2.Estimar o modelo
mqo_1<-lm(formula,data=db_covs)

###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
summary(mqo_1)

#O que se pode concluir?
#Quais vari?veis possuem rela??o nula com o n?mero de ?bitos?
#Quais vari?veis possuem rela??o positiva com o n?mero de infec??es?
#Quais vari?veis possuem rela??o negativa com o n?mero de infec??es?
#Qual ? a propor??o da varia??o da vari?vel dependente explicada?


###[3] Regress?o mortalidade vs socioecon?micos

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
#Quais vari?veis possuem rela??o nula com o n?mero de ?bitos?

#Qual ? a propor??o da varia??o da vari?vel dependente explicada?

#Quais explicativas apresentam rela??es distintas nas duas regress?es?


