#
# Trabalho da disciplina Banco de Dados 2016.1 - 1a etapa, 4o entregavel
# Beatriz Santana, Junot Neto, Michell Felippe
# Predicao do atributo NOTA DA REDAÇÃO da base Microdados do ENEM 2012
# Algoritmo: kNN (regressao)
#

library(data.table)
library(FNN)	# pacote do kNN

enem2012 <- fread("ENEM_LEVE.csv")

#
#remocao de colunas (atributos)
#
enem2012[,c("NU_ANO"):= NULL]
enem2012[,c("COD_MUNICIPIO_INSC"):= NULL]
enem2012[,c("NO_MUNICIPIO_INSC"):= NULL]
enem2012[,c("TP_ESTADO_CIVIL"):= NULL]
enem2012[,c("IN_UNIDADE_HOSPITALAR"):= NULL]
enem2012[,c("IN_GESTANTE"):= NULL]
enem2012[,c("IN_LACTANTE"):= NULL]
enem2012[,c("IN_IDOSO"):= NULL]
enem2012[,c("IN_SABATISTA"):= NULL]
enem2012[,c("IN_BRAILLE"):= NULL]
enem2012[,c("IN_AMPLIADA"):= NULL]
enem2012[,c("IN_LEDOR"):= NULL]
enem2012[,c("IN_ACESSO"):= NULL]
enem2012[,c("IN_TRANSCRICAO"):= NULL]
enem2012[,c("IN_LIBRAS"):= NULL]
enem2012[,c("IN_LEITURA_LABIAL"):= NULL]
enem2012[,c("IN_MESA_CADEIRA_RODAS"):= NULL]
enem2012[,c("IN_MESA_CADEIRA_SEPARADA"):= NULL]
enem2012[,c("IN_APOIO_PERNA"):= NULL]
enem2012[,c("IN_GUIA_INTERPRETE"):= NULL]
enem2012[,c("IN_CERTIFICADO"):= NULL]
enem2012[,c("NO_ENTIDADE_CERTIFICACAO"):= NULL]
enem2012[,c("UF_ENTIDADE_CERTIFICACAO"):= NULL]
enem2012[,c("COD_MUNICIPIO_ESC"):= NULL]
enem2012[,c("NO_MUNICIPIO_ESC"):= NULL]
enem2012[,c("UF_ESC"):= NULL]
enem2012[,c("SIT_FUNC"):= NULL]
enem2012[,c("COD_MUNICIPIO_PROVA"):= NULL]
enem2012[,c("NO_MUNICIPIO_PROVA"):= NULL]
enem2012[,c("UF_MUNICIPIO_PROVA"):= NULL]
enem2012[,c("IN_PRESENCA_CN"):= NULL]
enem2012[,c("IN_PRESENCA_CH"):= NULL]
enem2012[,c("IN_PRESENCA_LC"):= NULL]
enem2012[,c("IN_PRESENCA_MT"):= NULL]
enem2012[,c("TX_RESPOSTAS_CN"):= NULL]
enem2012[,c("TX_RESPOSTAS_CH"):= NULL]
enem2012[,c("TX_RESPOSTAS_LC"):= NULL]
enem2012[,c("TX_RESPOSTAS_MT"):= NULL]
enem2012[,c("ID_PROVA_CN"):= NULL]
enem2012[,c("ID_PROVA_CH"):= NULL]
enem2012[,c("ID_PROVA_LC"):= NULL]
enem2012[,c("ID_PROVA_MT"):= NULL]
enem2012[,c("TP_LINGUA"):= NULL]
enem2012[,c("DS_GABARITO_CN"):= NULL]
enem2012[,c("DS_GABARITO_CH"):= NULL]
enem2012[,c("DS_GABARITO_LC"):= NULL]
enem2012[,c("DS_GABARITO_MT"):= NULL]

#
#modificando valores
#
enem2012$TP_ESCOLA[enem2012$TP_ESCOLA == "."] <- "0"
#enem2012$TP_ESCOLA<-factor(as.integer(enem2012$TP_ESCOLA))
enem2012$IN_TP_ENSINO[enem2012$IN_TP_ENSINO == "."] <- "0"
enem2012$IN_TP_ENSINO<-factor(as.integer(enem2012$IN_TP_ENSINO))
enem2012$ID_LOCALIZACAO[enem2012$ID_LOCALIZACAO == "."] <- "0"
enem2012$ID_LOCALIZACAO<-factor(as.integer(enem2012$ID_LOCALIZACAO))
enem2012$ID_DEPENDENCIA_ADM[enem2012$ID_DEPENDENCIA_ADM == "."] <- "0"
enem2012$ID_DEPENDENCIA_ADM<-factor(as.integer(enem2012$ID_DEPENDENCIA_ADM))

enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "P"] <- "0"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "B"] <- "1"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "T"] <- "2"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "N"] <- "3"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "I"] <- "4"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "A"] <- "5"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "H"] <- "6"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "C"] <- "7"
enem2012$IN_STATUS_REDACAO[enem2012$IN_STATUS_REDACAO == "F"] <- "8"
enem2012$IN_STATUS_REDACAO<-factor(as.integer(enem2012$IN_STATUS_REDACAO))

enem2012$NU_NOTA_COMP1[ enem2012$NU_NOTA_COMP1 == "."] <- "-1"
enem2012$NU_NOTA_COMP2[ enem2012$NU_NOTA_COMP2 == "."] <- "-1"
enem2012$NU_NOTA_COMP3[ enem2012$NU_NOTA_COMP3 == "."] <- "-1"
enem2012$NU_NOTA_COMP4[ enem2012$NU_NOTA_COMP4 == "."] <- "-1"
enem2012$NU_NOTA_COMP5[ enem2012$NU_NOTA_COMP5 == "."] <- "-1"
enem2012$NU_NOTA_REDACAO[ enem2012$NU_NOTA_REDACAO == "."] <- "-1" 
enem2012$NU_NT_CN[ enem2012$NU_NT_CN == "."] <- "-1" 
enem2012$NU_NT_CH[ enem2012$NU_NT_CH == "."] <- "-1" 
enem2012$NU_NT_MT[ enem2012$NU_NT_MT == "."] <- "-1" 
enem2012$NU_NT_LC[ enem2012$NU_NT_LC == "."] <- "-1" 

#
#transformando alguns atributos para numericos
#
enem2012 <- transform(enem2012, NU_NOTA_REDACAO = as.numeric(NU_NOTA_REDACAO), NU_NT_CN = as.numeric(NU_NT_CN), NU_NT_CH = as.numeric(NU_NT_CH),
NU_NT_MT = as.numeric(NU_NT_MT), NU_NT_LC = as.numeric(NU_NT_LC), NU_NOTA_COMP1 = as.numeric(NU_NOTA_COMP1), NU_NOTA_COMP2 = as.numeric(NU_NOTA_COMP2), NU_NOTA_COMP3 = as.numeric(NU_NOTA_COMP3), 
NU_NOTA_COMP4 = as.numeric(NU_NOTA_COMP4), NU_NOTA_COMP5 = as.numeric(NU_NOTA_COMP5))

enem2012 <- transform(enem2012, IN_BAIXA_VISAO = as.numeric(IN_BAIXA_VISAO), IN_DEFICIENCIA_FISICA = as.numeric(IN_DEFICIENCIA_FISICA), 
	IN_DISLEXIA = as.numeric(IN_DISLEXIA), TP_COR_RACA = as.numeric(TP_COR_RACA), ST_CONCLUSAO = as.numeric(ST_CONCLUSAO), IN_DEFICIT_ATENCAO = as.numeric(IN_DEFICIT_ATENCAO))

#
#remoçao de outliers
#
#participantes com mais de 60 anos 
enem2012 <- enem2012[ enem2012$IDADE < 60, ]
#notas na redaçao abaixo de 100 e acima de 900 sao pouco frequentes
enem2012 <- enem2012[ enem2012$NU_NOTA_REDACAO < 900, ]
enem2012 <- enem2012[ (enem2012$NU_NOTA_REDACAO == -1 | enem2012$NU_NOTA_REDACAO > 100), ]
#alunos de escola publica com notas na redacao maiores que 800
enem2012 <- enem2012[ !(enem2012$TP_ESCOLA == 0 & enem2012$NU_NOTA_REDACAO > 800), ]
#baixa visao == 1 e notas abaixo de 400 ou acima de 600
enem2012 <- enem2012[ !(enem2012$IN_BAIXA_VISAO == 1 & enem2012$NU_NOTA_REDACAO < 400 & enem2012$NU_NOTA_REDACAO > 600), ]
#deficiencia física == 1 e notas abaixo de 300 ou acima de 650
enem2012 <- enem2012[ !(enem2012$IN_DEFICIENCIA_FISICA == 1 & enem2012$NU_NOTA_REDACAO < 300 & enem2012$NU_NOTA_REDACAO > 650), ]
#deficit atencao == 1 e notas abaixo de 400 
enem2012 <- enem2012[ !(enem2012$IN_DEFICIT_ATENCAO == 1 & enem2012$NU_NOTA_REDACAO < 400), ]
#dislexia == 1 e notas abaixo de 500
enem2012 <- enem2012[ !(enem2012$IN_DISLEXIA == 1 & enem2012$NU_NOTA_REDACAO < 500), ]
#localizacao da escola == 2 (perimetro rural) e notas acima de 800
enem2012 <- enem2012[ !(enem2012$ID_LOCALIZACAO == 2 & enem2012$NU_NOTA_REDACAO > 800), ]
#tipo do ensino == 2 (ensino de jovens e adultos) e notas acima de 800
enem2012 <- enem2012[ !(enem2012$IN_TP_ENSINO == 2 & enem2012$NU_NOTA_REDACAO > 800), ]
#cor/raca == 2 ou 3 ou 4 e notas acima de 850
enem2012 <- enem2012[ !((enem2012$TP_COR_RACA == 2 | enem2012$TP_COR_RACA == 3 | enem2012$TP_COR_RACA == 4) & enem2012$NU_NOTA_REDACAO > 850), ]
#cor/raca == 5 e notas acima de 800
enem2012 <- enem2012[ !(enem2012$TP_COR_RACA == 5 & enem2012$NU_NOTA_REDACAO > 800), ]
#status conclusao == 4 e notas acima de 800
enem2012 <- enem2012[ !(enem2012$ST_CONCLUSAO == 4 & enem2012$NU_NOTA_REDACAO > 800), ]


#print(nrow(enem2012))
#print(names(enem2012))

#
# transformacoes na base - adapta-la para o algoritmo knn.reg
# e mais remocao de atributos...
#
enem2012[,c("NU_INSCRICAO"):= NULL]
enem2012[,c("PK_COD_ENTIDADE"):= NULL]
enem2012[,c("V1"):= NULL]
enem2012[,c("UF_INSC"):= NULL]

enem2012[,c("ANO_CONCLUIU"):= NULL]

#enem2012[,c("TP_SEXO"):= NULL]
enem2012[,c("IDADE"):= NULL] 
#enem2012[,c("ST_CONCLUSAO"):= NULL]
#enem2012[,c("TP_ESCOLA"):= NULL]
#enem2012[,c("IN_TP_ENSINO"):= NULL]
#enem2012[,c("TP_COR_RACA"):= NULL]
#enem2012[,c("IN_BAIXA_VISAO"):= NULL]
#enem2012[,c("IN_CEGUEIRA"):= NULL]
#enem2012[,c("IN_SURDEZ"):= NULL]
#enem2012[,c("IN_DEFICIENCIA_AUDITIVA"):= NULL]
#enem2012[,c("IN_SURDO_CEGUEIRA"):= NULL]
#enem2012[,c("IN_DEFICIENCIA_FISICA"):= NULL]
#enem2012[,c("IN_DEFICIENCIA_MENTAL"):= NULL]
#enem2012[,c("IN_DEFICIT_ATENCAO"):= NULL]
#enem2012[,c("IN_DISLEXIA"):= NULL]
#enem2012[,c("IN_AUTISMO"):= NULL]
#enem2012[,c("ID_DEPENDENCIA_ADM"):= NULL]
#enem2012[,c("ID_LOCALIZACAO"):= NULL]

#enem2012$ANO_CONCLUIU[enem2012$ANO_CONCLUIU == "."] <- 0
enem2012$NU_NOTA_COMP1[ enem2012$NU_NOTA_COMP1 == "-1"] <- 0
enem2012$NU_NOTA_COMP2[ enem2012$NU_NOTA_COMP2 == "-1"] <- 0
enem2012$NU_NOTA_COMP3[ enem2012$NU_NOTA_COMP3 == "-1"] <- 0
enem2012$NU_NOTA_COMP4[ enem2012$NU_NOTA_COMP4 == "-1"] <- 0
enem2012$NU_NOTA_COMP5[ enem2012$NU_NOTA_COMP5 == "-1"] <- 0
enem2012$NU_NOTA_REDACAO[ enem2012$NU_NOTA_REDACAO == "-1"] <- 0 
enem2012$NU_NT_CN[ enem2012$NU_NT_CN == "-1"] <- 0 
enem2012$NU_NT_CH[ enem2012$NU_NT_CH == "-1"] <- 0 
enem2012$NU_NT_MT[ enem2012$NU_NT_MT == "-1"] <- 0 
enem2012$NU_NT_LC[ enem2012$NU_NT_LC == "-1"] <- 0 

enem2012[,c("NU_NT_CN"):= NULL]
enem2012[,c("NU_NT_CH"):= NULL]
enem2012[,c("NU_NT_LC"):= NULL]
enem2012[,c("NU_NT_MT"):= NULL]
enem2012[,c("IN_STATUS_REDACAO"):= NULL]
enem2012[,c("NU_NOTA_COMP1"):= NULL]
enem2012[,c("NU_NOTA_COMP2"):= NULL]
enem2012[,c("NU_NOTA_COMP3"):= NULL]
enem2012[,c("NU_NOTA_COMP4"):= NULL]
enem2012[,c("NU_NOTA_COMP5"):= NULL]

#print(nrow(enem2012))


enem2012 <- transform(enem2012,TP_SEXO = as.numeric(TP_SEXO), ST_CONCLUSAO = as.numeric(ST_CONCLUSAO),
	TP_ESCOLA = as.numeric(TP_ESCOLA), IN_TP_ENSINO = as.numeric(IN_TP_ENSINO), IN_BAIXA_VISAO = as.numeric(IN_BAIXA_VISAO), IN_CEGUEIRA = as.numeric(IN_CEGUEIRA),
	IN_SURDEZ = as.numeric(IN_SURDEZ), IN_DEFICIENCIA_AUDITIVA = as.numeric(IN_DEFICIENCIA_AUDITIVA), IN_SURDO_CEGUEIRA = as.numeric(IN_SURDO_CEGUEIRA),
	IN_DEFICIENCIA_FISICA = as.numeric(IN_DEFICIENCIA_FISICA), IN_DEFICIENCIA_MENTAL = as.numeric(IN_DEFICIENCIA_MENTAL), IN_DEFICIT_ATENCAO = as.numeric(IN_DEFICIT_ATENCAO),
	IN_DISLEXIA = as.numeric(IN_DISLEXIA), IN_AUTISMO = as.numeric(IN_AUTISMO), ID_DEPENDENCIA_ADM = as.numeric(ID_DEPENDENCIA_ADM), ID_LOCALIZACAO = as.numeric(ID_LOCALIZACAO))

#normalização

#enem2012$IDADE <- (enem2012$IDADE-min(enem2012$IDADE))/(max(enem2012$IDADE)-min(enem2012$IDADE))
enem2012$ST_CONCLUSAO <- (enem2012$ST_CONCLUSAO-min(enem2012$ST_CONCLUSAO))/(max(enem2012$ST_CONCLUSAO)-min(enem2012$ST_CONCLUSAO))
enem2012$TP_ESCOLA <- (enem2012$TP_ESCOLA-min(enem2012$TP_ESCOLA))/(max(enem2012$TP_ESCOLA)-min(enem2012$TP_ESCOLA))
enem2012$IN_TP_ENSINO <- (enem2012$IN_TP_ENSINO-min(enem2012$IN_TP_ENSINO))/(max(enem2012$IN_TP_ENSINO)-min(enem2012$IN_TP_ENSINO))
enem2012$TP_COR_RACA <- (enem2012$TP_COR_RACA-min(enem2012$TP_COR_RACA))/(max(enem2012$TP_COR_RACA)-min(enem2012$TP_COR_RACA))
enem2012$ID_DEPENDENCIA_ADM <- (enem2012$ID_DEPENDENCIA_ADM-min(enem2012$ID_DEPENDENCIA_ADM))/(max(enem2012$ID_DEPENDENCIA_ADM)-min(enem2012$ID_DEPENDENCIA_ADM))
enem2012$ID_LOCALIZACAO <- (enem2012$ID_LOCALIZACAO-min(enem2012$ID_LOCALIZACAO))/(max(enem2012$ID_LOCALIZACAO)-min(enem2012$ID_LOCALIZACAO))
enem2012$NU_NOTA_REDACAO <- (enem2012$NU_NOTA_REDACAO-min(enem2012$NU_NOTA_REDACAO))/(max(enem2012$NU_NOTA_REDACAO)-min(enem2012$NU_NOTA_REDACAO))

write.csv(enem2012, "BaseEnemModificada-versaofinal.csv", row.names = TRUE)

#
# encontra o melhor k para ser usado no algoritmo knn
# utilizando a medida MAE (erro absoluto medio)
# baseado em: https://www.r-bloggers.com/using-knn-classifier-to-predict-whether-the-price-of-stock-will-increase/
#
index <- 1:nrow(enem2012)
testindex <- sample(index, trunc(length(index)/3))
testset <- enem2012[testindex,]
trainset <- enem2012[-testindex,]


testset_labels <- testset$NU_NOTA_REDACAO
trainset_labels <- trainset$NU_NOTA_REDACAO

# remocao do atributo alvo nos dois conjuntos (baseado em: https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.z_H7i2o)
testset[,c("NU_NOTA_REDACAO"):= NULL]
trainset[,c("NU_NOTA_REDACAO"):= NULL]

MAE <- rep(0, 10)
k <- 1:10
for(x in k){
	knnResult <- knn.reg(train=trainset, test=testset, y=trainset_labels, k=x)
	MAE[x] <- mean(abs(testset_labels - knnResult$pred))
	MAE[x] <- MAE[x]*900
}
MAE_labels <- format(round(MAE, 2), nsmall = 2)
plot(k, MAE, type = 'b',
     main= "Numero de vizinhos no kNN vs. Erro absoluto medio",
     xlab= "k (numero de vizinhos)",
     ylab= "MAE (erro absoluto medio)",
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

text(k, MAE, labels=MAE_labels, cex=0.7, pos=1)

#
# avaliacao do modelo preditivo usando 10-fold cross-validation juntamente com
# MAE (erro absoluto medio) e MSE (raiz quadrada do erro quadratico medio)
# divisao da base em 10 conjuntos: 9/10 para treinamento e 1/10 para teste (10 execucoes)
# media dos 10 valores obtidos do MAE
#

enem2012shuffled <- enem2012[sample(nrow(enem2012)),]	# redistribui os registros da base de forma aleatória
k_folds = 10
folds <- cut(seq(1, nrow(enem2012shuffled)), breaks=k_folds, labels=FALSE)

MAE <- rep(0, k_folds)
RMSE <- rep(0, k_folds)
for(i in 1:k_folds){
    testIndexes <- which(folds == i, arr.ind = TRUE)

    testset <- enem2012shuffled[testIndexes, ]
    trainset <- enem2012shuffled[-testIndexes, ]
    

    testset_labels <- testset$NU_NOTA_REDACAO
	trainset_labels <- trainset$NU_NOTA_REDACAO

	# remocao do atributo alvo nos dois conjuntos (baseado em: https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.z_H7i2o)
	testset[,c("NU_NOTA_REDACAO"):= NULL]
	trainset[,c("NU_NOTA_REDACAO"):= NULL]

    #
	# regressao usando knn
	# k = 10 (encontrado atraves do experimento explicado acima)
	#
	knnResult <- knn.reg(train=trainset, test=testset, y=trainset_labels, k=10)
	MAE[i] <- mean(abs(testset_labels - knnResult$pred))
	RMSE[i] <- sqrt(mean((testset_labels - knnResult$pred)^2))
}

MAE_media = sum(MAE)/k_folds*900
RMSE_media = sum(RMSE)/k_folds*900

print(MAE_media)
print(RMSE_media)







