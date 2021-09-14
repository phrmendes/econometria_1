
######Econometria I
#####Terceira aula de laboratório, script 3
#####Preparado por Thiago Fonseca Morello (CECS-UFABC)
#####Melhor visualizado com fonte 16 (edit--->GUI preferences, size: 16) 
###O objetivo deste roteiro de aula é comparar os testes F e LM para o teste de restrição de exclusão,
#apresentar a interpretação de elasticidades e semi-elasticidades e apresentar operações matriciais.
#São utilizados os mesmos exemplos do laboratório 1, como segue abaixo.

###Regressão que será estimada: equação de Mincer

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
#Exemplo: 
setwd("D:/desk_comp/Brasa/UFABC/metria_1/2020/labs")
#Acionar o comando abaixo com ctrl+r
load("dados.Rdata")


####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM
####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM
####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM####Testes F e LM


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

###0.Preparação dos dados
#Neste laboratório, por conta do teste LM, vamos criar dois objetos separados, cada um com uma das duas edições (2004 e 2015) da PNAD
db_min_04<-subset(db_min,ano==2004)
db_min_15<-subset(db_min,ano==2015)

#Sumário estatístico das variáveis
summary(subset(db_min,select=c(anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_bca_ama,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5)))

###1.Fórmula da regressão
formula<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano

###2.Regressão 1: 2004
mqo_1<-lm(formula,data=db_min_04)

###3.Regressão 2: 2015
mqo_2<-lm(formula,data=db_min_15)


###4.Teste do multiplicador de Lagrange: ano de 2004
#O teste será feito para as dummies de atividade e ocupação
#4.a Primeiro passo: estimando o modelo restrito e obtendo os resíduos
formula_R<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Estimação
mqo_R<-lm(formula_R,data=db_min_04)
#Armazendo os resíduos
#O vetor "residuals" do objeto com os resultados da regressão contém os resíduos
#Ele tem de ser armazendo como uma variável adicional dentro do objeto com os dados
db_min_04$res_r<-mqo_R$residuals
#4.b Segundo passo: regressão dos resíduos do modelo restrito contra todas as explicativas 
#(incluindo as que são objeto e as que não são objeto do teste)
#(i) Fórmula (é a mesma fórmula da regressão irrestrita com os resíduos no lugar da variável dependente)
formula<-res_r~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#(ii) Estimação
mqo_res<-lm(formula=formula,data=db_min_04)
#4.c Terceiro passo: computar a estatística do teste
#A fórmula é: NR2_res, sendo R2_res o R2 do modelo com resíduos como variáveis dependentes
#Nota: trata-se do R2 não-ajustado, observado abaixo
summary(mqo_res)
#R2_res = 0.1317
#Cálculo do tamanho amostral
nrow(db_min_04)
#Estatística
lm<-nrow(db_min_04)*0.1317
lm
#4.d Quarto passo: obter a região crítica e  o p-valor
#4.d.1: RC[0.05] = {valor crítico; infinito}
#O valor crítico é obtido da distribuição qui-quadrado com número de graus de liberdade
#equivalente ao número de variáveis-alvo do teste. 
#Cálculo do número de graus de liberdade
gl<-length(coef(mqo_1))-length(coef(mqo_R))
#Além disso, o valor crítico é o número à direita do qual há 5% de probabilidade
qchisq(0.05,gl,lower.tail=F)
#Atenção para a opção lower.tail = F que solicita o valor crítico à direita na FD
#A RC é RC[0.05] = {25; infinito}
#4.d.2: p-valor = P(qui-quad > valor_observado(lm))
pchisq(lm,gl,lower.tail=F)
#Aqui também é preciso usar lower.tail.F, do contrário o programa reporta P(qui-quad < valor_observado(lm))
#4.e Quinto passo: decisão
#O valor observado da estatística foi
lm
#O valor crítico foi de 25
#Qual é a decisão correta: rejeitar ou não a hipótese nula de que atividade e ocupação são irrelevantes populacionalmente?
#O p-valor foi nulo, enquanto o nível de significância é de 5%?
#Qual é a decisão correta com base no critério do p-valor: rejeitar ou não H0?
#O que podemos concluir?

###Comparação com o teste F, feito no laboratório 2
#A seguir, reproduzem-se os comandos utilizados no lab 2:
#Obtendo a SQR:
SQR_R<-sum(mqo_R$residuals^2)
#Obtendo SQR_IR
SQR_IR<-sum(mqo_1$residuals^2)
#Obtendo o número de explicativas-alvo do teste como diferença do número de parâmetros dos dois modelos
s<-length(coef(mqo_1))-length(coef(mqo_R))
#Número de graus de liberdade do denominador
gl_den<-nrow(db_min_04)-length(coef(mqo_1))
#Calculando a estatística
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
f1<-((SQR_R-SQR_IR)/s)/(SQR_IR/gl_den)
f1
##A região crítica unilateral pode ser obtida como segue
#Valor crítico superior
qf(1-0.025,s,gl_den)
#RC = {1.832,inf}
#p-valor
pf(f1,s,gl_den,lower.tail=F)
#Qual é a decisão correta: rejeitar ou não a hipótese nula de contribuição irrelevante das variáveis de atividade e ocupação?

#Os dois testes chegaram à mesma conclusão?
#Os p-valores dos dois testes se mostraram aproximadamente iguais?


###5.Teste do multiplicador de Lagrange: ano de 2015
#Para 2015, vamos testar a significância conjunta de apenas três dummies de atividade:
#d_ativ_agro, d_ativ_indus, d_ativ_const
#Nenhuma delas foi individualmente significativa, conforme se observa abaixo.
summary(mqo_2)
#Porém, significância individual não é condição necessária para a significância conjunta.
#Ou seja, é possível ter insignificância individual e significância conjunta.
#A relação lógica, pois, entre as duas formas de significância não permite concluir
#a partir da insignificância individual se há ou não significância conjunta.
#Daí a relevância do segundo teste em tal situação.

#5.a Primeiro passo: estimando o modelo restrito e obtendo os resíduos
formula_R<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Estimação
mqo_R<-lm(formula_R,data=db_min_15)
summary(mqo_R)
#Armazendo os resíduos agora no objeto com dados de 2015
db_min_15$res_r<-mqo_R$residuals
#5.b Segundo passo: regressão dos resíduos do modelo restrito contra todas as explicativas 
#(i) Fórmula (é a mesma fórmula da regressão irrestrita com os resíduos no lugar da variável dependente)
formula<-res_r~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#(ii) Estimação
mqo_res<-lm(formula=formula,data=db_min_15)
#5.c Terceiro passo: computar a estatística do teste
summary(mqo_res)
#R2_res = 2.525*10^-5
#Cálculo do tamanho amostral
nrow(db_min_15)
#Estatística
lm<-nrow(db_min_15)*2.525*10^-5
lm
#5.d Quatro passo: obter a região crítica e  o p-valor
#Cálculo do número de graus de liberdade
gl<-length(coef(mqo_2))-length(coef(mqo_R))
#Além disso, o valor crítico é o número à direita do qual há 5% de probabilidade
qchisq(0.05,gl,lower.tail=F)
#A RC é RC[0.05] = {8; infinito}
#5.d.2: p-valor = P(qui-quad > valor_observado(lm))
pchisq(lm,gl,lower.tail=F)
#5.e Quinto passo: decisão
#O valor observado da estatística foi
lm
#O valor crítico foi de 8

#Qual é a decisão correta: rejeitar ou não a hipótese nula de que atividade e ocupação são irrelevantes populacionalmente?
#Tome por base, se ajudar, o gráfico abaixo
plot(seq(0,10,by=0.1),dchisq(seq(0,10,by=0.1),gl),type="l")
abline(v=lm,col="blue")
abline(v=qchisq(0.05,gl,lower.tail=F),col="red")

#O p-valor foi nulo, enquanto o nível de significância é de 5%?
#Qual é a decisão correta com base no critério do p-valor: rejeitar ou não H0?
#Há contradição com os testes de significância individual?
#O que podemos concluir?


###Comparação com o teste F
#Obtendo a SQR:
SQR_R<-sum(mqo_R$residuals^2)
#Obtendo SQR_IR
SQR_IR<-sum(mqo_2$residuals^2)
#Obtendo o número de explicativas-alvo do teste como diferença do número de parâmetros dos dois modelos
s<-length(coef(mqo_2))-length(coef(mqo_R))
#Número de graus de liberdade do denominador
gl_den<-nrow(db_min_15)-length(coef(mqo_1))
#Calculando a estatística
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
f1<-((SQR_R-SQR_IR)/s)/(SQR_IR/gl_den)
f1
##A região crítica unilateral pode ser obtida como segue
#Valor crítico superior
qf(1-0.025,s,gl_den)
#RC = {3.11,inf}
#p-valor
pf(f1,s,gl_den,lower.tail=F)
#Qual é a decisão correta: rejeitar ou não a hipótese nula de contribuição irrelevante das variáveis de atividade e ocupação?

#Os dois testes chegaram à mesma conclusão?
#Os p-valores dos dois testes se mostraram aproximadamente iguais?
#O que a resposta sobre a última pergunta nos ensina sobre a "equivalência assintótica" dos dois testes?
#O que podemos concluir?


####Versão matricial do teste F
#Este conteúdo se encontra no apêndice B da nota de aula 6
#As hipóteses do teste são:
#H0: RB = q vs H1: RB != q
#R = matriz r x (K+1) de restrições lineares, r = número de restrições
#B = vetor coluna (K+1) x 1 de parâmetros da regressão múltipla, incluindo o intercepto na primeira posição
#q = vetor coluna r x 1 de valores atribuídos às restrições lineares 
#Por exemplo, um teste de restrições de exclusão das três primeiras explicativas 
#compreende três restrições, b1 = 0, b2 = 0 e b3 = 0. Ou seja, r = 3.
#A matriz R é:
#   |0 1 0 0 0 0 ... 0|
#R =|0 0 1 0 0 0 ... 0|
#   |0 0 0 1 0 0 ... 0|
#O vetor q é:
#    |0|
#q = |0|
#    |0|

#Criando a matriz R
#No caso geral do teste de restrições de exclusão, cada restrição requer uma linha na
#matriz R com a unidade exatamente na posição do coeficiente-alvo e zero nas demais posições
#Como gerar uma linha com tal propriedade no R? 
#Basta utilizar a fórmula: 
#c(rep(0,J-1),1,rep(0,length(coef(mqo_1))-J-1))
#Por exemplo, vamos refazer o teste de restrições de exclusão para atividade e ocupação
#A posição da primeira dummy pode ser vista abaixo
cbind(1:26,coef(mqo_1))
#posição = 7
#Aplicando a fórmula
J<-7
c(rep(0,J-1),1,rep(0,length(coef(mqo_1))-J))
#Visualizando apropriadamente
View(c(rep(0,J-1),1,rep(0,length(coef(mqo_1))-J)))
#Notar que a linha foi gerada como coluna, mas basta transpor
#usando a função de transposição t(.)
t(c(rep(0,J-1),1,rep(0,length(coef(mqo_1))-J)))
#Visualizando apropriadamente
View(t(c(rep(0,J-1),1,rep(0,length(coef(mqo_1))-J))))
#É preciso repetir para os demais coeficientes-alvo
cbind(1:26,coef(mqo_1))
#Enquanto a posição da primeira dummy é 7, a posição da última dummy é 21
#Há 15 variáveis
#Vamos utilizar a função "sapply" para repetir o comando para todas as
#posições de 7 a 21
J<-7:21
View(t(sapply(7:21,function(x)c(rep(0,x-1),1,rep(0,length(coef(mqo_1))-x)))))
#Armazenando a matriz R
mat_R<-t(sapply(7:21,function(x)c(rep(0,x-1),1,rep(0,length(coef(mqo_1))-x))))
#Transformando a matriz R em um objeto matricial, o que é necessário para fazer operações matriciais
mat_R<-as.matrix(mat_R)

#Criando o vetor q
#O vetor q é um vetor coluna nulo com tamanho equivalente ao número de coeficientes-alvo
#O que é criado automaticamente pela função de geração de matrizes do R abaixo
#A sintaxe é matrix(elemento,número de colunas, número de linhas)
matrix(0,ncol=1,nrow=15)
#Armazenando
vec_q<-matrix(0,ncol=1,nrow=15)

#Utilizando o comando linearHypothesis desenvolvido por Fansworth
#Mais informações em http://cran.r-project.org/doc/contrib/Farnsworth-EconometricsInR.pdf
#Em primeiro lugar, é necessário instalar o pacote AER que contém diversos testes econométricos
install.packages("AER")
#Acionando o pacote
library("AER")
#Sintaxe da função que aplica o teste
#linearHypothesis(objeto com modelo irrestrito,hypothesis.matrix = matrix R, rhs = vetor q)
linearHypothesis(mqo_1,hypothesis.matrix=mat_R,rhs=vec_q)

#Lendo a janela do teste
#Hypothesis = restrições lineares sob teste
#Importante verificar se são de fato as restrições visadas
#Para isso é uma boa rever a sequencia de variáveis-alvo com base, por exemplo, na estimação do modelo irrestrito
summary(mqo_1)
#Como desejamos testar a hipótese de que todas as dummies de atividade e ocupação são conjuntamente nulas, então
#todas as dummies de d_ativ_agro a d_ocup_armada devem aparecer na subjanela "Hypothesis" do teste
linearHypothesis(mqo_1,hypothesis.matrix=mat_R,rhs=vec_q)
#Esse é caso. Então passamos para as próximas informações
#Model 1: é o modelo restrito, sem as variáveis destacadas na subjanela "Hypothesis"
#Model 2: é o modelo irrestrito.
#No último caso, a fórmula tem de bater com a utilizada no início desse script
#Que é reproduzida abaixo
#formula<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Os resultados da estimação são exibidos em uma tabela com duas linhas, a primeira correspondendo ao modelo 1, a segund ao 2.
#Res.Df = graus de liberdade da soma dos quadrados dos resíduos de cada modelo.
#São contados como N - K - 1, sendo, para o modelo restrito
nrow(db_min_04)-(26-15)
#Notar que das 26 explicativas do modelo irrestrito, 15 não estão presentes no modelo restrito
#E, para o modelo irrestrito
nrow(db_min_04)-(26)
#RSS = soma dos quadrados dos resíduos
#Df = graus de liberdade do numerador da distribuição F
#F = valor observado da estatística do teste
#Pr(>F) = p-valor do teste
#Resultou F = 940.88 e p-valor = 2.2 x 10^-16
#Vamos verificar se é o mesmo resultado obtido no teste que fizemos manualmente acima.

#Repetindo teste do laboratório 2 novamente
formula_R<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Estimação
mqo_R<-lm(formula_R,data=db_min_04)
#Obtendo a SQR:
SQR_R<-sum(mqo_R$residuals^2)
#Obtendo SQR_IR
SQR_IR<-sum(mqo_1$residuals^2)
#Obtendo o número de explicativas-alvo do teste como diferença do número de parâmetros dos dois modelos
s<-length(coef(mqo_1))-length(coef(mqo_R))
#Número de graus de liberdade do denominador
gl_den<-nrow(db_min_04)-length(coef(mqo_1))
#Calculando a estatística
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
f1<-((SQR_R-SQR_IR)/s)/(SQR_IR/gl_den)
f1
#Notar igualdade com arredondamento na segunda casa decimal em relação ao contabilizado pelo comando "LinearHypothesis"
#p-valor
pf(f1,s,gl_den,lower.tail=F)
#O p-valor também é o mesmo, pois 2.2 x 10^-16 é equivalente à zero no R (e em geral)
#Fica portanto confirmado que o comando linearHypothesis e o procedimento "manual" são equivalentes

###Teste de combinações lineares dos parâmetros
#A depender do objetivo da análise, pode ser relevante realizar testes envolvendo funções lineares dos parâmetros.
#Tais funções são geralmente fornecidas pela teoria.
#Por exemplo, ao estimar a função de demanda de um produto, os coeficientes captando as elasticidades em relação ao preço do próprio produto, em relação ao preço do produto substituo e em relação à renda têm de somar zero.
#Essa é a propriedade de homogeneidade de uma função de demanda (ver Nicholson, Microeconomic Theory, ninth edition).
#Mas é também possível formular testes a partir de perguntas de pesquisa relevantes.
#Por exemplo, se observamos os resultados da regressão minceriana de 2015, temos que:
summary(mqo_2)
#Homens recebem salário horário R$1.388 maior;
#Pretos e pardos recebem salário R$0.735 menor e 
#Residentes da região NE salário R$0.906 menor. 
#Será que as duas últimas características mais do que compensam a primeira em termos estatísticos?
#Essa pergunta não é trivial, mesmo sendo que a resposta, com base nas estimativas pontuais, é claramente afirmativa.
#O valor da diferença das estimativas é:
1.388-0.735-0.906
#A razão para isso está em que as estimativas pontuais não aos valores verdadeiros ou populacionais dos parâmetros.
#É possível, pois, que, mesmo com a soma das estimtivas sendo negativa, a soma dos parâmetros seja não-negativa. 
#Daí por que é equivocado concluir, a partir da soma das pontuais que, na população brasileira como um todo, etnia e região
#quando combinadas, produzem efeito que compensa o efeito de gênero.
#É necessário testar.
#Antes disso, é preciso ter clareza quanto à pergunta-base do teste e quanto à função de parâmetros-alvo do teste.
#A pergunta: será que homem, preto ou pardo da região NE recebe salário maior do que mulher branca ou amarela de outra região do Brasil? 
#Responder a essa pergunta significa medir o efeito da seguinte diferença de expectativas condicionais:
#E[y|X,d_hom=1,d_etnia_pre_par=1,d_reg_2=1] - E[y|X,d_hom=0,d_etnia_pre_par=0,d_reg_2=0] = b(d_hom)+b(d_etnia_pre_par)+b(d_reg_2)
#Testar, pois, se a soma dos três coeficientes é nula consiste em testar se dois grupos sociais, 
#que diferem apenas nas três características (são equivalentes nas demais características), também diferem em remuneração.

###Especificando o teste
#Seja considerado, como referência, um teste com para duas restrições de combinação linear.
#A primeira restrição é a de que os três primeiros parâmetros somam zero.
#A segunda restrição é a de que o quarto e o quinto parâmetros somam zero.
#A matriz R é 2 x 26, pois há duas restrições:
#   |1 1 1 0 0 0 ... 0|
#R =|0 0 0 1 1 0 ... 0|
#O vetor q é 2 x 1 pois há duas restriçoes.
#    |0|
#q = |0|
#Adaptando para o caso em questão, há apenas uma restrição linear, b(d_hom)+b(d_etnia_pre_par)+b(d_reg_2) = 0
#Nesse caso, a matriz R é uma linha com valores unitários na posição dos coeficientes em questão e zero nas demais posições.
#Identificando as posições dos parâmetros
cbind(1:26,coef(mqo_2))
#As posições são: 6, 5, 22
#Gerando a matriz R
c(rep(0,4),1,1,rep(0,21-7+1),1,rep(0,4))
#Armazenando
mat_R<-t(c(rep(0,4),1,1,rep(0,21-7+1),1,rep(0,4)))
#Atenção: é preciso transpor
#Gerando o vetor q
matrix(0,nrow=2,ncol=1)
#Armazenando o vetor q
vec_q<-matrix(0,nrow=1,ncol=1)
#Aplicando o teste
linearHypothesis(mqo_2,hypothesis.matrix=mat_R,rhs=vec_q)

#Hypothesis: verificar se é a restrição que se deseja testar
#No caso, é a restrição visada, o que significado que matriz R e vetor q foram corretamente definidos.
#Nesse caso, o modelo restrito contém, ao invés das três dummies separadamente, a soma delas.
#Qual é o resultado do teste? O grupo social homens, pretos ou pardos, da região NE recebe remuneração
#estatisticamente distinta do grupo mulheres, brancas ou amarelas, de outra região?

#Notar que o resultado do teste não é informativo, em caso de rejeição de H0, quanto ao sinal do parâmetro
#correspondente à soma dos três coeficientes. Uma maneira de obter tal informação é aplicando o procedimento
#sugerido por Wooldridge no exercício 4.8 da 2a edição em inglês.
#Basta tomar b(d_hom)+b(d_etnia_pre_par)+b(d_reg_2) = d 
#De modo que b(d_hom) = d - b(d_etnia_pre_par)-b(d_reg_2)
#Introduzindo na FRP tem-se:
#Y = b(d_hom)*d_hom+b(d_etnia_pre_par)*d_etnia_pre_par + b(d_reg_2)*d_reg_2 + BX + u
#Com BX incorporando o intercepto, e os demais coeficientes multiplicados por respectivas explicativas.
#b(d_hom) = d - b(d_etnia_pre_par)-b(d_reg_2)
#Y = [d - b(d_etnia_pre_par)-b(d_reg_2)]*d_hom+b(d_etnia_pre_par)*d_etnia_pre_par + b(d_reg_2)*d_reg_2 + BX + u
#Y = d*d_hom+b(d_etnia_pre_par)*(d_etnia_pre_par-d_hom) + b(d_reg_2)*(d_reg_2-d_hom) + BX + u
#Ou seja, a regressão passa a incorporar duas subtrações de dummies.
#Estimando essa regressão:
#Atenção: é necessário incluir a função I(.) envolvendo operações matemáticas que componham a formula 
#Do contrário o R ignora as operações.
#Ou seja, temos de introduzir:
#I(d_etnia_pre_par-d_hom)
#I(d_reg_2-d_hom)
#Outra maneira de fazer, menos direta, é gerando as variáveis-operação:
#db_min_15$d_etnia_m_hom<-db_min_15$d_etnia_pre_par-db_min_15$d_hom
#db_min_15$d_reg_2_m_hom<-db_min_15$d_reg_2-db_min_15$d_hom
#E incluindo elas na fórmula

#Utilizando a primeira maneira
formula<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+I(d_etnia_pre_par-d_hom)+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+I(d_reg_2-d_hom)+d_reg_3+d_reg_4+d_reg_5+d_urbano
mqo_2_rl<-lm(formula=formula,data=db_min_15)
#Os resultados da estimação seguem abaixo.
summary(mqo_2_rl)
#O que se pode concluir: homens pretos ou pardos, residentes da região NE recebem remuneração maior ou menor do que mulheres brancas ou amarelas residentes em outras regiões?

#Realizando a versão manual do teste [[[Não será feito no vídeo lab.3]]]
#Hipóteses do teste
#H0: b(d_hom)+b(d_etnia_pre_par)+b(d_reg_2) = d = 0 vs H1: b(d_hom)+b(d_etnia_pre_par)+b(d_reg_2) = d != 0
#Basta, pois, introduzir d = 0 no modelo anterior que incorpora a transformação de parâmetros.
#Ou seja, utilizar a mesma fórmula anterior com I(d_etnia_pre_par-d_hom) substituindo d_etnia_pre_par e I(d_reg_2-d_hom) substituindo d_reg_2
#E com d_hom omitida.
formula_R<-sal_hor~anos_educ+exper_idade_trab+exper_idade_trab_sq+I(d_etnia_pre_par-d_hom)+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+I(d_reg_2-d_hom)+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Estimação
mqo_R<-lm(formula_R,data=db_min_15)
#Obtendo a SQR:
SQR_R<-sum(mqo_R$residuals^2)
#Obtendo SQR_IR
SQR_IR<-sum(mqo_2$residuals^2)
#Obtendo o número de explicativas-alvo do teste como diferença do número de parâmetros dos dois modelos
s<-length(coef(mqo_2))-length(coef(mqo_R))
s
#Número de graus de liberdade do denominador
gl_den<-nrow(db_min_15)-length(coef(mqo_1))
gl_den
#Calculando a estatística
#(1) {(SQR_R-SQR_IR)/s}/{(SQR_IR)/(N-K-1)}
f1<-((SQR_R-SQR_IR)/s)/(SQR_IR/gl_den)
f1
#Notar igualdade em relação ao contabilizado pelo comando "LinearHypothesis"
#p-valor
pf(f1,s,gl_den,lower.tail=F)
#O p-valor também é o mesmo, basta conferir abaixo.
linearHypothesis(mqo_2,hypothesis.matrix=mat_R,rhs=vec_q)


####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades
####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades
####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades####Interpretação: elasticidades e semi-elasticidades


###Regressões com variáveis em logaritmo
##Caso 1: variáveis dependente e independentes em forma logaritmica
#Nesse caso, os coeficientes são elasticidades.
#Seja considerada a equação minceriana para 2015 com salário horário e educação em forma logarítmica
#Para introduzir o log, basta utilizar a função log(1+x) diretamente na definição da fórmula
#Importante: como é possível que as variáveis assumam valores nulos e não está definido o logaritmo de um valor nulo,
#é sempre uma boa ideia utilizar a transformação log(1+x).
formula<-log(1+sal_hor)~log(1+anos_educ)+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Visualizando o coeficiente
coef(lm(formula=formula,data=db_min_15))[2]
#Qual é a leitura correta do valor numérico dessa estimativa pontual?
#Tem-se que 1% de aumento no nível educacional implica em 0.13% de aumento na remuneração.

#A base de tal interpretação está em que, neste caso:
#b = dy/dx . x/y
#E, pois, dy/y = b.x/dx. Tomando x/dx = 1/100, tem-se dy/y = b.1/100


##Caso 2: variável dependente em forma logarítmica e variável independente em forma original
#Nesse caso, os coeficientes são semi-elasticidades que expressam o aumento percentual da variável dependente
#resultante de um aumento unitário da variável independente.
#Na fórmula abaixo, apenas a variável dependente contém log
formula<-log(1+sal_hor)~anos_educ+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Visualizando o coeficiente
coef(lm(formula=formula,data=db_min_15))[2]
#Qual é a leitura correta do valor numérico dessa estimativa pontual?
#Toma-se por base a seção 6.2 da segunda edição em inglês do livro de Wooldridge.
#Tem-se que uma unidade a mais de educação, i.e., um ano de estudo a mais, implica em (0.032 x 100)% = 3.2% de aumento na remuneração.
#Notar que, nesse caso, a analogia direta com a leitura utilizada para a elasticidade não é válida.

#A base de tal interpretação está em que, neste caso:
#b = dy/dx . 1/y
#E, pois, dy/y = b.dx. Tomando dx= 1, tem-se dy/y = b

##Caso 3: variável dependente em forma original e variável independente em forma logarítmica
#Esse caso é menos comum em economia.
#Nesse caso, os coeficientes são semi-elasticidades que expressam o aumento unitário da variável dependente
#resultante de um aumento percentual da variável independente.
#Na fórmula abaixo, apenas a variável anos_educ contém log
formula<-sal_hor~log(1+anos_educ)+exper_idade_trab+exper_idade_trab_sq+d_etnia_pre_par+d_hom+d_ativ_agro+d_ativ_indus+d_ativ_const+d_ativ_social+d_ativ_serv+d_ativ_apub+d_ocup_adm+d_ocup_agro+d_ocup_com+d_ocup_serv+d_ocup_dirigente+d_ocup_cie_art+d_ocup_tec_med+d_ocup_rep_manut+d_ocup_armada+d_reg_2+d_reg_3+d_reg_4+d_reg_5+d_urbano
#Visualizando o coeficiente
coef(lm(formula=formula,data=db_min_15))[2]
#Qual é a leitura correta do valor numérico dessa estimativa pontual?
#Tem-se que, pois, um aumento de 1% do nível educacional resulta em um aumento do salário horário de 1.689/100 = 0.0169 ~ 2 centavos/hora. 
#Ou, alternativamente, um aumento de 100% do nível educacional, uma duplicação de tal nível, resulta em um aumento da remuneração horária em R$1.689.


#A base de tal interpretação está em que, neste caso:
#b = dy/dx . x
#E, pois, dy = b.dx/x. Tomando dx/x = 1/100, tem-se dy = b.1/100

#As três interpretações têm a mesma origem, a qual consiste em derivar os dois lados da FRP abaixo em função de x.
f(y) = b0 + b1g(x) + BX + u
#Sendo f(.) e g(.) funções logarítmicas ou identidade (f(x) = x), a depender de qual dos três casos acima é considerado.
#Para calcular tal derivada, recordar que y é função de x.

####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial
####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial
####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial####Álgebra matricial

###Operações matriciais
##O modelo lm gera as estimativas pontuais a partir da operação matricial que resulta da solução do problema de mínimos quadrados ordinários
#Tal operação matricial, conforme nota de aula 4, é a seguinte:
#b_mqo^ = [(X'X)^-1](X'Y)
#Sendo b_mqo^ o vetor N x 1 de estimadores para todos os parâmetros da FRP (intercepto e coeficientes);
#X = matriz N x (K+1) de variáveis explicativas com uma primeira coluna contendo apenas números unitários, para com isso incorporar o intercepto à estimação;
#Y = vetor N x 1 de valores da variável dependente. 
#É possível obter diretamente o vetor de estimativas pontuais realizando a operação matricial anterior no R.

#Definindo a matriz X
#Inicialmente-se seleciona-se estritamente as variáveis explicativas do modelo
#Para isso, basta utilizar o comando subset com opção select
mat_x<-subset(db_min_15,select=c(anos_educ,exper_idade_trab,exper_idade_trab_sq,d_etnia_pre_par,d_hom,d_ativ_agro,d_ativ_indus,d_ativ_const,d_ativ_social,d_ativ_serv,d_ativ_apub,d_ocup_adm,d_ocup_agro,d_ocup_com,d_ocup_serv,d_ocup_dirigente,d_ocup_cie_art,d_ocup_tec_med,d_ocup_rep_manut,d_ocup_armada,d_reg_2,d_reg_3,d_reg_4,d_reg_5,d_urbano))
#Agora é preciso adicionar o vetor com valores unitários
#Gerando um vetor 1 x N de 1's 
vec_1<-matrix(1,nrow=nrow(db_min_15),ncol=1)
#Acoplando horizontalmente mat_x e vec_1 
#importante: vec_1 tem de ser a primeira coluna da esquerda para a direita, obrigatoriamente.
mat_x<-cbind(vec_1,mat_x)
#Visualizando
View(mat_x)
#De fato o vetor de 1's está na primeira coluna
#Verificando dimensão da matriz X
dim(mat_x)
#Notar que há exatamente 26 colunas, exatamente o número de parâmetros do modelo até então estimado (contendo o intercepto)

#É necessário transfomar mat_x em um objeto matricial para poder fazer as operações matriciais.
mat_x<-as.matrix(mat_x)

#Definindo o vetor Y
vec_y<-as.matrix(db_min_15$sal_hor,nrow=nrow(db_min_15),ncol=1)
#Verificando dimensão do vetor Y
dim(vec_y)
#Notar que o número de linhas é o mesmo da matriz X

#Realizando operações matriciais
#O objetivo é calcular b_mqo^ = [(X'X)^-1](X'Y)
#Vamos subdividir em quatro operações

#Operação 1: X'X
#Notar que a matriz à esquerda é a transposição da matriz X. 
#A função t(.) transpõe no R.
#O operador %*% aplica o produto matricial. 
#Não se trata do produto escalar em que são multiplicados os elementos em posições equivalentes das duas matrizes.
op_1<-t(mat_x)%*% mat_x

#Operação 2: (X'X)^-1
#A função "solve" inverte matrizes no R. Essa é sem dúvida a operação que mais exige do computador.
op_2<-solve(op_1)

#Operação 3: (X'Y)
#Trata-se do produto matricial abaixo. Atenção: nunca alterar a ordem dos fatores de uma multiplicação matricial.
op_3<-t(mat_x) %*% vec_y
#Importante notar a transposição da matriz X, sem a qual a multiplicação seria impossível.
#Basta observar as dimensões de mat_x e vec_y abaixo
dim(mat_x)
dim(vec_y)
#Agora, com mat_x transposta:
dim(t(mat_x))
dim(vec_y)
#O número de colunas de mat_x transposta é equivalente ao número de linhas de vec_y, como é necessário.

#Operação 4: operação 2 x operação 3
#Recordando, b_mqo^ = (X'X)^-1(X'Y) = (op_2)(op_3) 
#Novamente trata-se de produto matricial
op_4<-op_2 %*% op_3

#Visualizando os resultados
op_4
#O que são exatamente os números associados a cada variável?
#O que significa o número associado a vec_1?

#Comparando com os coeficientes estimados por MQO
cbind(op_4,coef(mqo_2))

#Fazendo as quatro operações de uma só vez
solve(t(mat_x)%*%mat_x)%*%(t(mat_x)%*%vec_y)

#Notar o poder da álgebra matricial: com apenas uma operação matricial é possível obter de uma só vez 25 estimativas pontuais.
#E, em um computador pessoal com configuração média o resultado é obtido em menos de um segundo.
#Daí a importância de saber álgebra matricial.

#Obtendo a matrix de variância-covariância e as estimativas pontuais para os erros-padrão dos estimadores
#Sob as hipóteses do modelo clássico de regressão linear, a matriz de variância-covariância tem forma simples.
#Trata-se de: vcov = [(sigma^2)^](X'X)^-1
#Sendo [(sigma^2)^] a estimativa pontual para a variância do termo de perturbação. 
#Esta corresponde à soma dos quadrados dos resíduos dividida pelo respectivo número de graus de liberdade (N-K-1).
#Vamos calcular a partir de três operações

#Calculando [(sigma^2)^] = SQR/(N-K-1)
op_1<-sum(mqo_2$residuals^2)/(nrow(db_min_15)-length(coef(mqo_2)))

#Calculando (X'X)^-1
op_2<-solve(t(mat_x)%*%mat_x)

#Multiplicando op_1 e op_2
#Notar que, nesse caso, como [(sigma^2)^] é um escalar, deve-se utilizar o produto escalar "*"
op_3<-op_1*op_2
#Dimensão da matriz gerada
dim(op_3)
#Está correta a dimensão?

#Para comparar com a matriz vcov gerada pelo comando lm, vamos fazer dois passos.
#Passo 1: subtração elemento a elemento das duas matrizes
#A matriz vcov gerada pelo comando lm(.) fica armazenada em vcov(mqo_2), conforme laboratório 2.
#Para subtrair elemento a elemento, basta usar o sinal de subtração
vcov(mqo_2) - op_3
#Visualizando apropriadamente
View(vcov(mqo_2) - op_3)
#notar que as diferenças são desprezíveis, com ordem de magnitude de 10^-11 ou menor
#De fato, a diferença mínima é
min(vcov(mqo_2) - op_3)
#Com certeza, a operação matricial realizada é equivalente à utilizada pelo comando lm(.)
#A soma das diferenças elemento a elemento atesta isso
sum(vcov(mqo_2) - op_3)
#Ou seja, as diferenças somam um número desprezível.

#Passo 2: comparar a diagonal da matriz op_3 com os erros-padrão dos estimadores segundo comando lm(.)
#Faz sentido fazer iso?
#Extraindo a diagonal da matriz gerada
op_4<-diag(op_3)
cbind(manual=op_4,lm=diag(vcov(mqo_2)))

#Fica nítido pois a ausência de diferença entre as variância dos estimadores obtidas com as operações manuais e via lm(.)


