library(stm)
library(ggplot2)
library(readxl)
library(SnowballC)
library(tm)
library(geometry)
library(rsvd)
library(Rtsne)
library(matrixStats)
library(caret)
library(lubridate)
library(igraph)
library(huge)
library(ggthemes)
library(tidytext)


## importando texto

discursos <- read_excel("discursos_alfa.xlsx")

discursos = subset(discursos, select = -c(...1, id))
discursos$Data <- as.Date(discursos$Data)
discursos$Day <- difftime(discursos$Data, as.Date("2006-12-31"), units = "days")

str(discursos)


### Pre processamento do texto

customstopwords = c("§§","casa","vexa","sr","presidente","revisão","srs","sras","orador","aqui","deputado","deputados","todos","ser","sessão","ordem","palavra","sra","2º","a","acerca","adeus","agora","ainda","algmas","algo","algumas","alguns","ali","além","ambos","ano","anos","antes","ao","aos","apenas","apoio","apontar","após","aquela","aquelas","aquele","aqueles","aqui","aquilo","as","assim","através","atrás","até","aí","baixo","bastante","bem","bom","breve","cada","caminho","catorze","cedo","cento","certamente","certeza","cima","cinco","coisa","com","como","comprido","conhecido","conselho","contra","corrente","custa","cá","da","daquela","daquele","dar","das","de","debaixo","demais","dentro","depois","desde","desligado","dessa","desse","desta","deste","deve","devem","deverá","dez","dezanove","dezasseis","dezassete","dezoito","dia","diante","direita","diz","dizem","dizer","do","dois","dos","doze","duas","dá","dão","dúvida","e","ela","elas","ele","eles","em","embora","enquanto","entre","então","era","essa","essas","esse","esses","esta","estado","estar","estará","estas","estava","este","estes","esteve","estive","estivemos","estiveram","estiveste","estivestes","estou","está","estás","estão","eu","exemplo","falta","fará","favor","faz","fazeis","fazem","fazemos","fazer","fazes","fazia","faço","fez","fim","final","foi","fomos","for","fora","foram","forma","foste","fostes","fui","geral","grande","grandes","grupo","hoje","horas","há","iniciar","inicio","ir","irá","isso","ista","iste","isto","já","lado","ligado","local","logo","longe","lugar","lá","maior","maioria","maiorias","mais","mal","mas","me","meio","menor","menos","meses","mesmo","meu","meus","mil","minha","minhas","momento","muito","muitos","máximo","mês","na","nada","naquela","naquele","nas","nem","nenhuma","nessa","nesse","nesta","neste","no","noite","nome","nos","nossa","nossas","nosso","nossos","nova","nove","novo","novos","num","numa","nunca","não","nível","nós","número","o","obra","obrigada","obrigado","oitava","oitavo","oito","onde","ontem","onze","os","ou","outra","outras","outro","outros","para","parece","parte","partir","pegar","pela","pelas","pelo","pelos","perto","pessoas","pode","podem","poder","poderá","podia","ponto","pontos","por","porque","porquê","posição","possivelmente","posso","possível","pouca","pouco","povo","primeira","primeiro","promeiro","próprio","próximo","puderam","pôde","põe","põem","qual","qualquer","quando","quanto","quarta","quarto","quatro","que","quem","quer","quero","questão","quieto","quinta","quinto","quinze","quê","relação","sabe","saber","se","segunda","segundo","sei","seis","sem","sempre","ser","seria","sete","seu","seus","sexta","sexto","sim","sistema","sob","sobre","sois","somente","somos","sou","sua","suas","são","sétima","sétimo","tal","talvez","também","tanto","tarde","te","tem","temos","tempo","tendes","tenho","tens","tentar","tentaram","tente","tentei","ter","terceira","terceiro","teu","teus","teve","tipo","tive","tivemos","tiveram","tiveste","tivestes","toda","todas","todo","todos","trabalhar","trabalho","treze","três","tu","tua","tuas","tudo","tão","têm","um","uma","umas","uns","usa","usar","vai","vais","valor","veja","vem","vens","ver","verdade","verdadeiro","vez","vezes","viagem","vindo","vinte","você","vocês","vos","vossa","vossas","vosso","vossos","vários","vão","vêm","vós","zero","à","às","área","é","és","último","nº")


processed <- textProcessor(discursos$Discurso, metadata = discursos,  
                           lowercase = TRUE,removestopwords = TRUE, removenumbers = TRUE,
                           removepunctuation = TRUE, ucp = FALSE, stem = TRUE,
                           wordLengths = c(2, Inf), sparselevel = 1, language = "portuguese",
                           verbose = TRUE, onlycharacter = TRUE, striphtml = TRUE,
                           customstopwords = customstopwords, custompunctuation = NULL, v1 = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, 
                     lower.thresh = 1000, upper.thresh = 210000, verbose = TRUE) 
out$meta$Day <- as.numeric(out$meta$Day)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


## modelo
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 0, prevalence =~  Oradores + Estado + Partidos + Presidente + vote_share_UF + partido_pres + manifesta + s(Day),
                 max.em.its = 100, data = out$meta, seed = 123,
                 init.type = "Spectral", verbose = TRUE)

windows()

plot.STM(First_STM,'summary', n=3, text.cex = 0.5, ylim = 0.06)

plot.STM(First_STM, "labels", topics=topicos, label="frex", n=5, width=55)

summary(First_STM)


topicQuality(First_STM, out$documents, xlab = "Semantic Coherence",
             ylab = "Exclusivity", labels = 1:ncol(First_STM$theta), M = 10, cex = 5)


labelTopics(First_STM, topics = NULL, n = 7, frexweight = 0.5)

# textos exemplos de cada tópico
exemplo39 = findThoughts(First_STM,texts=meta$Discurso, topics=39, n=3)$docs[[1]]
exemplo55 = findThoughts(First_STM,texts=meta$Discurso, topics=55, n=3)$docs[[1]]
exemplo53 = findThoughts(First_STM,texts=meta$Discurso, topics=53, n=4)$docs[[1]]

plotQuote(exemplo55,width=60, maxwidth=600, text.cex=.85)


corr = topicCorr(First_STM, method = "huge", cutoff = 0.01,
          verbose = TRUE)
plot(corr, topics = topicos, vlabels = NULL,
     layout = NULL, vertex.color = "red", vertex.label.cex = 0.75,
     vertex.label.color = "black", vertex.size = NULL)

## prevalence
str(meta)
topicos = c(1,4,5,6,7,8,9,11,14,15,19,20,21,23,31,33,34,37,39,46,48,53,55,56,58,59,60,64,66,67,70,71,72,73,74,75,76)
predict_topics<-estimateEffect(formula = topicos ~ Oradores + Estado + Partidos + Presidente + vote_share_UF + partido_pres + manifesta + s(Day), 
                               stmobj = First_STM, metadata = out$meta, 
                               uncertainty = "Global")

plot.estimateEffect(predict_topics)

efeitos_mani = summary(predict_topics,topics = c(4,7,9,11,15,19,31,33,48,53,55,56,58,59,60,70,72,75), nsim = 1000)
write.csv2(efeitos_mani$tables, "efeitos_mani.csv")
efeitos_total = summary(predict_topics,topics = topicos, nsim = 1000)
write.csv2(efeitos_total$tables, "efeitos_total.csv")

  
windows()
plot.estimateEffect(predict_topics, model=First_STM, cov.value1="1", cov.value2="0", covariate="manifesta", topics=topicos, method="difference",
                    nsims = 100, xlab="Efeito das manifestações", labeltype="custom", custom.labels=topicos,ci.level=.95)


plot(predict_topics)

windows()
par(mfrow = c(1,2))

# prevalence grafica
plot(predict_topics, "Day", method = "continuous", topics = c(53,39),
     model = First_STM, printlegend = TRUE, xaxt = "n", xlab = "Time (2007-2019)")
yearseq <- seq(from = as.Date("2007-01-01"),
               to = as.Date("2019-12-31"), by = "year")
axis(1,at = as.numeric(yearseq) - min(as.numeric(yearseq)),
     labels = c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))


# efeito de dummy
plot(predict_topics, "manifesta", method = "pointestimate", topics = 55,
     model = First_STM, printlegend = TRUE, xlab = "Time (2007-2019)")

