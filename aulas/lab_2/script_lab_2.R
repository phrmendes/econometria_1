
######Econometria I
#####Segunda aula de laboratório, script 2
#####Preparado por Thiago Fonseca Morello (CECS-UFABC)
#####Melhor visualizado com fonte 16 (edit--->GUI preferences, size: 16) 
###O objetivo deste roteiro de aula é apresentar exemplos de testes de significância
#individual, conjunta e global para regressão múltipla. 
#São utilizados os mesmos exemplos do laboratório 1, como segue abaixo.

###Regressões que serão estimadas

#(1) Equação de Mincer

# Salário horário real = ß0 + ß1*educação + ß2*experiência + ß3*setor + ß4*ocupação 
#+ ß5*gênero + ß6*etnia + ß7*região + ß7*área_urbana + ut

#Fontes:
#GIUBERTI, Ana Carolina; MENEZES-FILHO, Naércio. Discriminação de rendimentos por gênero: uma comparação entre o Brasil e os Estados Unidos. Econ. Apl., Ribeirão Preto , v. 9, n. 3, p. 369-384, Set. 2005.
#Meara, K., Pastore, F., & Webster, A. (2020). The gender pay gap in the USA: a matching study. Journal of Population Economics, 33(1), 271-305.

#(1) Infecções por COVID-19, gênero e características socioeconômicas, âmbito internacional

# Infecções per capita = ß0 + ß1*Dirigente_mulher + ß2*fertilidade + ß2*poluição + ß3*Lockdown +
#ß4*Continente  + u

#Fonte: Abras, A.G., Fava, A.C.P., Kuwahara, M. Y. (2020) Women at the Top: Female Heads of State and COVID-19 Policy Responses. Feminist Economics.

#(2) Mortalidade devido a COVID-19 e características socioeconômicas, âmbito internacional

# Óbitos per capita = ß0 + ß1*Dirigente_mulher + ß2*fertilidade + ß2*poluição + ß3*Lockdown + 
#ß4*Continente + ß5*Infecções_per_capita  + u

#Fonte: Abras, Fava e Kuwahara, 2020 FEMECON.


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


####Regressões####Regressões####Regressões####Regressões####Regressões####Regressões####Regressões####Regressões

###[1] Regressão Minceriana

###Relembrando variáveis da regressão

###Variável dependente
#(.) Salário horário
###Lista de explicativas (independentes)
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

###0.Sumário estatístico das variáveis
summary(subset(db_min,select=c(anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_bca_ama,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5)))

###1.Fórmula da regressão
formula<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano

###2.Regressão 1: 2004
mqo_1<-lm(formula,data=subset(db_min,ano==2004))

###3.Regressão 2: 2015
mqo_2<-lm(formula,data=subset(db_min,ano==2015))

###4.Compreendendo a janela de saída do R com os resultados de estimação
#O comando summary(mqo_1) apresenta o conjunto completo de estatísticas e testes referentes à regressão estimada
summary(mqo_1)

#Na primeira coluna, "Estimate", temos as estimativas pontuais dos parâmetros
#Na segunda coluna, "Std.Error", temos as estimativas pontuais dos erros-padrão dos estimadores
#Na terceira coluna, "t value", há o valor observado da estatística do teste significância individual
#de cada variável independente
#O que temos então na quarta coluna? Qual é o significado de P(>|t|)?
#Na parte inferior, há:
#(a) Residual standard error, acompanhando graus de liberdade: do que se trata?
#(b) Multiple R-squared? O que é isso?
#(c) Adjusted R-squared? Qual é a diferença em relação ao anterior?
#(d) F-statistic, com graus de liberdade: trata-se da estatística de qual teste?
#(e) p-valor para a estatística F

###5.Teste de significância individual: educação
#5.a) educação
#As hipóteses do teste são:
#H0: b(k) = 0 vs H1: b(k) != 0 [teste bilateral]
#Sendo b(k) o valor populacional do coeficiente da k-ésima variável
#A estatística do teste individual é:
#estimativa pontual / erro padrão = Estimate / Std. Error
#Recordando que o erro padrão é a raiz do estimador consistente da variância do estimador

#(i) obtendo a estimativa pontual para a k-ésima explicativa
#As estimativas pontuais são armazenadas no objeto mqo_1
#Para observá-las, basta utilizar a função coef(.)
coef(mqo_1)
#Para observar apenas o k-ésimo elemento do vetor de coeficientes, basta acrescentar o índice "[k+1]"
#P.ex., para obter o intercepto
coef(mqo_1)[1]
#Agora, para obter a pontual do coeficiente da primeira explicativa, educação:
coef(mqo_1)[2]

#(ii) obtendo o erro padrão do estimador para a k-ésima explicativa 
#As variâncias dos estimadores ficam armazenadas na matriz de variância-covariância
#Essa pode ser obtida utilizando-se a função vcov
vcov(mqo_1)
#Como a matriz é grande, de tamanho (K+1) X (K+1), no caso 26 x 26, 
#é melhor usar o comando View
View(vcov(mqo_1))


#É correto que alguns elementos da matriz de variância-covariância sejam negativos?

#Vamos visualizar a matriz de variância-covariância.
#Para isso, número negativos serão pintados de vermelho
#E positivos, de azul
#Primeiramente é preciso instalar o pacote raster no hard disk.
#Para isso, retire o "#" da frente da linha abaixo e tecle ctrl+r
#install.packages("raster")
#Selecione um dos mirrors, p.ex., SP1
#Uma vez instalado, reestabeleça, acima, o "#" que precedia o comando "install.packages("raster")"
#para evitar de acionar novamente a instalação na próxima vez que rodar esse script.
#Acionando o pacote
library(raster)
#Gerando a paleta de cores
colfunc<-colorRampPalette(c("red","blue"))
#Gerando o gráfico
plot(raster(vcov(mqo_1)),col=colfunc(2),breaks=c(min(vcov(mqo_1)),0,max(vcov(mqo_1))),legend=F,xlim=c(0,1),ylim=c(0,1))
lines(seq(0,1,by=0.1),1-seq(0,1,by=0.1),lwd=6)

#O primeiro padrão notório é que há apenas números positivos na diagonal principal
#Por que isso?
#Qual estatística está na diagonal principal da matriz de variância-covariância?
#A implicação do primeiro padrão é que os números negativos estão fora da diagonal principal
#É correto isso? O que temos fora da diagonal principal da matriz?

#Notar a simetria em torno da diagonal principal (ou seja, a diagonal principal é como um espelho).
#Será que uma matriz de covariância-variância é sempre simétria?

#O erro padrão referente ao k-ésimo coeficiente é o k+1-ésimo termo da diagonal principal 
#da matriz de variância-covariância. Basta utilizar a função diag com índice k+1
#P.ex., a variância do estimador para o coeficiente da primeira explicativa, educação, é:
diag(vcov(mqo_1))[2]

#(iii) Estatística t:
#valor observado = estimativa pontual / erro padrão = Estimate / Std. Error
coef(mqo_1)[2]/sqrt(diag(vcov(mqo_1))[2])
#Notar a função sqrt no denominador que aplica a raiz
#Este é o valor da estatística do teste.
#Armazenando no objeto t_obs
t_obs<-coef(mqo_1)[2]/sqrt(diag(vcov(mqo_1))[2])

#Obtendo a região crítica para o teste de significância da educação
#A estatística tem uma distribuição assintótica t de Student com N-(K+1) graus de liberdade 
#N = número de observações = número de linhas da matriz de dados
nrow(subset(db_min,ano==2004))
#K + 1 = número de parâmetros (contando com o intercepto)
length(coef(mqo_1))
#Logo N - (K+1) = 93106-26
93106-26
#Armazenando no objeto gl
gl<-93106-26
#Os valores críticos são, pois, os limiares que contém entre si 95% de probabilidade na
#distribuição t com 93080 graus de liberdade
qt(0.025,gl)
#Os valores críticos são, pois, ~ -1.96
#A região crítica é, pois, (-inf;-1.96] U (1.96,+inf)
#Em que inf = infinito

#Obtendo o p-valor para o teste de significância da educação
#O p-valor é a probabilidade de ocorrer valor mais extremo do que o observado para a estatística
#Basta utilizar a função pt, que é a função de distribuição t acumulada
#Lembrando-se que, em um teste bilateral, o p-valor é calculado como
#2*P(T<-|t_obs|)
2*pt(-abs(t_obs),gl)
#O p-valor é nulo

#Decisão quanto à significância individual da educação
#O valor observado da estatística é
t_obs
#A região crítica é (-inf;-1.96] U (1.96,+inf)
#O valor observado pertence à região crítica?
#Qual é a decisão correta, rejeitar ou não a hipótese nula?
#O p-valor foi inferior à 5%?
#Qual é a decisão correta, rejeitar ou não a hipótese nula?

#O que se conclui quanto à educação: tal fator mostrou-se influente na remuneração?


###6.Teste de significância individual: gênero masculino

#6.a Estimativa pontual
#Para obtê-la, é preciso saber qual é a posição do gênero masculino no vetor de estimativas pontuais
#O comando abaixo numera o elementos do vetor
cbind(1:26,coef(mqo_1))
#Trata-se da sexta posição, ou seja, a pontual desejada é:
coef(mqo_1)[6]

#6.b Erro padrão
#A matriz de variância covariância está estruturada de maneira a que o elemento
#na j-ésima coluna e na j-ésima linha seja a variância do estimador do (j-1)-ésimo coeficiente
#Notar que, como o intercepto está na primeira posição do vetor de pontuais, a variância
#correspondente está na primeira linha e na primeira coluna. Por isso, a variância do primeiro
#coeficiente está na segunda linha e na segunda coluna.
#Com isso, como a explicativa é a quinta (ver lista acima) a variância desejada é
diag(vcov(mqo_1))[6]
#Notar que se trata do mesmo índice da pontual

#6.c Estatística t:
#valor observado = estimativa pontual / erro padrão = Estimate / Std. Error
coef(mqo_1)[6]/sqrt(diag(vcov(mqo_1))[6])
#Notar a função sqrt no denominador que aplica a raiz
#Este é o valor da estatística do teste.
t_obs<-coef(mqo_1)[6]/sqrt(diag(vcov(mqo_1))[6])

#Obtendo a região crítica para o teste de significância da educação
#O número de graus de liberdade é o mesmo para todos os coeficientes e por
#isso já foi calculado.
#Gerando novamente de maneira mais automática
gl<-nrow(subset(db_min,ano==2004))-length(coef(mqo_1))
#Os valores críticos obtidos da seguinte forma:
qt(0.025,gl)
#Os valores críticos são, pois, ~ -1.96
#A região crítica é, pois, (-inf;-1.96] U (1.96,+inf)
#Em que inf = infinito

#Obtendo o p-valor para o teste de significância da educação: 2*P(T<-|t_obs|)
2*pt(-abs(t_obs),gl)
#Será que esse número é pequeno ou grande?

#Decisão quanto à significância individual da educação
#O valor observado da estatística é
t_obs
#A região crítica é (-inf;-1.96] U (1.96,+inf)
#O valor observado pertence à região crítica?
#Qual é a decisão correta, rejeitar ou não a hipótese nula?
#O p-valor foi inferior à 5%?
#Qual é a decisão correta, rejeitar ou não a hipótese nula?

#O que se conclui quanto ao gênero masculino: tal fator mostrou-se influente na remuneração?

#Qual conclusão se pode retirar quanto ao funcionamento do mercado de trabalho?


###7.Visualizando os testes de significância para todos os coeficientes a partir da janela de saída do R
##Todas as estatísticas necessárias para a tomada de decisão quanto aos testes de significância individual 
#de todos os parâmetros podem ser geradas aplicando-se o procedimento anterior ao vetor de estimativas pontuais
#e justapondo horizontalmente todos os vetores de estatísticas, como no comando abaixo.
cbind(Estimate=coef(mqo_1),Std_Err=diag(vcov(mqo_1)),t_stat=coef(mqo_1)/sqrt(diag(vcov(mqo_1))),p_value=2*pt(-abs(coef(mqo_1)/sqrt(diag(vcov(mqo_1)))),gl))

#Agora vamos tomar a decisão em relação à todas as explicativas unicamente com base no p-valor, esta
#a estatística mais comumente utilizada para tanto na prática econométrica.
#Vamos colocar a tabela no Excel para poder editar


#Qual é a pergunta que devemos fazer ao p-valor para estabelecer significância, i.e., rejeição da hipótese de nulidade?
#Em uma primeira coluna do Excel, vamos anotar exclusivamente significância

#Agora, em uma segunda coluna, anotamos anotamos o sinal das explicativas significativas
#Notar que o sinal das explicativas não significativas é zero, pois essa é a influência que o teste relevou que elas têm populacionalmente.

###8.Teste de significância global
#A estatística do teste é:
#F = {R2/K}/{(1-R2)/(N-K-1)}
#(notar que esta é a única estatística de teste; não é possível, como no 
#caso do teste de restrições de exclusão, obter uma estatística a partir
#das SQRs dos modelos restrito e irrestrito)

#Encontrando o R2 na saída do R
#Nota: trata-se do R ordinário não-ajustado
summary(mqo_1)
#A estatística buscada é o "Multiple R-squared"
#Valor = 0.3932
r2<-0.3932
#Calculando a estatística:
#(a) K + 1 = length(coef(mqo_1)) --> K = length(coef(mqo_1))-1
gl_num<-length(coef(mqo_1))-1
#(b) N-(K+1) = nrow(subset(db_min,ano==2004)) - length(coef(mqo_1))
gl_den<-nrow(subset(db_min,ano==2004)) - length(coef(mqo_1))
#(c) R2
f<-(r2/gl_num)/((1-r2)/gl_den)
f
#(d) p-valor com base na distribuição F
#O teste será realizado como unilateral, 
#tomando como p-valor a probabilidade de observar um valor
#mais positivo do que o observado.
#O que requer introduzir a opção "lower.tail=F" na FDA reportada
#pelo R. O comando é pf(f_obs,gl_num,gl_den,lower.tail=F)
pf(f,gl_num,gl_den,lower.tail=F)
#(e) comparando com a saída do R
#A última linha da saída do R possui a estatística F, sendo informado
#(i) o valor da estatística, (2) o número de gl do numerador, (3) o 
#número de gl do denominador e (4) o p-valor
#Vamos conferir os quatro
summary(mqo_1)
#(e.1) valor observado da estatística
f
#Ok, o valor da saída é aproximado pois não contém os decimais
#(e.2) gl num
gl_num
#ok, é o primeiro gl na saída do R
#(e.3) gl denom
gl_den
#ok, é o segundo gl na saída do R
#(e.4) p-valor: é a última informação
#tem-se o menor número que o R é capaz de reportar e, 
#portanto, está de acordo com o p-valor nulo obtido.

#Qual é a decisão correta: rejeitar ou não a hipótese nula de que o modelo não explica a variável dependente?

###9.Teste de significância conjunta: irrelevância de grupos de variáveis explicativas
#9.a) Bloco 3, Setor e ocupação
#Estatística do teste: nesse caso há duas versões:
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
#SQR_R = SQR do modelo restrito, i.e., sem as variáveis cuja significância conjunta se deseja testar
#SQR_IR = SQR do modelo irrestrito, com todas as explicativas
#s = número de explicativas cuja significância é testada
#(2) {(R2_IR-R2_R)/s}/{(1-R2_IR)/(N-K-1)}
#recordando que SQR = (1-R2)SQT
#Atenção: a subtração no numerador está "invertida" comparando-se (1) e (2)
#(9.b) Calculando a primeira versão
#SQR_R: é necessário rodar a regressão restrita
#A formula abaixo já exclui as variáveis de atividade e ocupação
formula_R<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Estimação
mqo_R<-lm(formula_R,data=subset(db_min,ano==2004))
#Obtendo a SQR:
summary(mqo_R)
#A SQR não está disponível a partir da janela de saída, é preciso calcular somando os quadrados dos resíduos, que ficam
#armazenados no elemento "residuals"
SQR_R<-sum(mqo_R$residuals^2)
#Obtendo SQR_IR
SQR_IR<-sum(mqo_1$residuals^2)
#Obtendo o número de explicativas-alvo do teste como diferença do número de parâmetros dos dois modelos
length(coef(mqo_1))-length(coef(mqo_R))
#Contando manualmente para conferir
length(c("d_ativ_agro","d_ativ_indus","d_ativ_const","d_ativ_social","d_ativ_serv","d_ativ_apub","d_ocup_adm","d_ocup_agro","d_ocup_com","d_ocup_serv","d_ocup_dirigente","d_ocup_cie_art","d_ocup_tec_med","d_ocup_rep_manut","d_ocup_armada"))
#OK
s<-length(coef(mqo_1))-length(coef(mqo_R))
#Calculando a estatística
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
f1<-((SQR_R-SQR_IR)/s)/(SQR_IR/gl_den)
f1
#notar que gl_den = N - K - 1 já havia sido obtido antes
##A região crítica unilateral pode ser obtida como segue
#Valor crítico superior
qf(1-0.025,s,gl_den)
#RC = {1.832,inf}
#p-valor
pf(f1,s,gl_den,lower.tail=F)
#Qual é a decisão correta: rejeitar ou não a hipótese nula de contribuição irrelevante das variáveis de atividade e ocupação?

#(9.c) Calculando a segunda versão
#Obtendo o R2 do modelo restrito (Atenção: trata-se do R2 não-ajustado)
summary(mqo_R)
R2_R<-0.3011
#Obtendo o R2 do modelo irrestrito
summary(mqo_1)
R2_IR<-0.3932
#Calculando a estatística do teste:
#{(R2_IR-R2_R)/s}/{(1-R2_IR)/(N-K-1)}
f2<-((R2_IR-R2_R)/s)/((1-R2_IR)/gl_den)
f2
##Comparando as duas estatísticas
#As duas estatísticas têm de ser equivalentes, uma vez que são algebricamente iguais
f1
f2
#Há um erro de arredondamento, provavelmente devido ao uso do R2 tal como reportado na janela de saída e,
#pois, com poucas casas decimais.

###10.Intervalos de confiança para coeficientes
#A fórmula do IC é:
#{b(k)-t(0.025)DP(k);b(k)-t(0.025)DP(k)}
#10.1 coeficiente da educação
#10.1.a)limiares de confiança = t(0.025)
#Os limiares de confiança são valores entre os quais há probabilidade equivalente ao nível de
#significância, nesse caso de 95%;
#Basta utilizar a função qt((1-NC)/2,gl), NC = 0.95
qt(0.025,gl_den)
#Nota: como o limiar é negativo, o IC será calculado com a seguinte adaptação:
#{b(k)+t(0.025)DP(k);b(k)-t(0.025)DP(k)}
#Recordando que gl_den = N-K-1, como calculado no teste de significância global
#Os demais elementos, incluindo estimativa pontual e erro padrão do estimador são obtidos da mesma
#maneira que para o teste de significância individual
#segue abaixo o IC
c(coef(mqo_1)[2]+qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))[2]),coef(mqo_1)[2]-qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))[2]))

#Janela de saída com todas as estatísticas relevantes
cbind(Estimate=coef(mqo_1),Std_Err=diag(vcov(mqo_1)),t_stat=coef(mqo_1)/sqrt(diag(vcov(mqo_1))),p_value=2*pt(-abs(coef(mqo_1)/sqrt(diag(vcov(mqo_1)))),gl),CI_l=coef(mqo_1)+qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))),CI_h=coef(mqo_1)-qt(0.025,gl_den)*sqrt(diag(vcov(mqo_1))))

#O que se pode concluir?
#Quais variáveis possuem relação positiva com o número de óbitos?
#Quais variáveis possuem relação negativa com o número de óbitos?
#Quais variáveis possuem relação nula com o número de óbitos?


##Regressão Minceria para 2015
summary(mqo_2)

#O que se pode concluir?
#Quais variáveis possuem relação positiva com a remuneração?
#Quais variáveis possuem relação negativa com a remuneração?
#Quais variáveis possuem relação nula com a remuneração?



###[2] Regressão infecções vs gênero e socioeconômicos

###preparação dos dados
db_covs<-subset(db_cov,is.na(casos_por_milhao)==F & is.na(obitos_por_milhao)==F & is.na(fem_head) == F & is.na(fertility_) == F & is.na(airpoll_exposure_) == F & is.na(d_lockdown) == F)
db_covs<-subset(db_covs, select=c(casos_por_milhao,obitos_por_milhao,fem_head,fertility_,airpoll_exposure_,d_lockdown,Continent,country))


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


###1.Especificar formula
formula<-casos_por_milhao~fem_head+fertility_+airpoll_exposure_+d_lockdown+I(Continent)


###2.Estimar o modelo
mqo_1<-lm(formula,data=db_covs)

###Visualizando resultados
#Os resultados ficaram armazenados no objeto "mqo_1" 
#Para os visualizar com detalhe, basta aplicar o comando summary no objeto
summary(mqo_1)

#O que se pode concluir?
#Quais variáveis possuem relação nula com o número de óbitos?
#Quais variáveis possuem relação positiva com o número de infecções?
#Quais variáveis possuem relação negativa com o número de infecções?
#Qual é a proporção da variação da variável dependente explicada?


###[3] Regressão mortalidade vs socioeconômicos

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
#Quais variáveis possuem relação nula com o número de óbitos?

#Qual é a proporção da variação da variável dependente explicada?

#Quais explicativas apresentam relações distintas nas duas regressões?


