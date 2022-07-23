#LIBRERIE
library(tm)
library(syuzhet)
library(ggplot2)
library(wordcloud)
library(stopwords)
library(sentimentr)
stopwords("en")
stopwords("it")

x <- `Recensioni.amazon.(1)`
x

#COLONNA DELLE STELLE
x$Rating

#cancello cose inutili in rating per poter fare poi l'istogramma
stelle_pulite <- gsub(pattern = ",0 su 5 stelle", replacement = "",x$Rating)
stelle_pulite[530]
stelle_pulite[1]
#trasformo in numerico
numstelle <- as.numeric(stelle_pulite)

#trasformo in tabella 
tablestelle <- table(numstelle)
tablestelle

#istogramma della tabella delle stelle
barplot(tablestelle, ylab = "Numero stelle", xlab = "recensitori")

#diagramma a torta
cols <- c("yellow","green", "grey", "red", "brown")
labs <- c("76","34","28","70", "342")
labs1 <- c("1","2","3","4","5")
pie(tablestelle, main = "stelle",labels = labs1, col = cols)
legend(1.2, 1.0, cex = 1, legend = labs, fill = cols)



#COLONNA RECENSIONI
recensioni <- x$Review.Content
recensioni[3]
#trasformo tutte le recensioni in minuscolo
recensioni_pulito <- tolower(recensioni)
recensioni_pulito[1]

#trasformo in vCorpus
recensioni_pulito <- VCorpus(VectorSource(recensioni_pulito))
summary(recensioni_pulito)

#numero di caratteri nel corpus [1]
inspect(recensioni_pulito[1])

#rimozione della punteggiatura con stampa
recensioni_pulito <- tm_map(recensioni_pulito, removePunctuation)
recensioni_pulito[[1]]$content

#rimozione dei numeri con stampa
recensioni_pulito[[9]]$content
recensioni_pulito <- tm_map(recensioni_pulito, removeNumbers)
recensioni_pulito[[9]]$content
recensioni_pulito[[12]]$content


#rimozione stopwords ita con stampa
recensioni_pulito <- tm_map(recensioni_pulito, removeWords, stopwords("it"))   
recensioni_pulito[[1]]$content

#rimozione stopwords eng con stampa
recensioni_pulito <- tm_map(recensioni_pulito, removeWords, stopwords("en"))   
recensioni_pulito[[1]]$content

#rimozione parte flessa tramite lo stemming 
recensioni_pulito <- tm_map(recensioni_pulito, stemDocument)
recensioni_pulito[[1]]$content

#rimozione di spazi non necessari 
recensioni_pulito <- tm_map(recensioni_pulito,stripWhitespace)
recensioni_pulito[[1]]$content


#trasformazione del documento in matrice di termini
dtm <- DocumentTermMatrix(recensioni_pulito)   
dtm  

#osservazione frequenza delle parole 
freq <- colSums(as.matrix(dtm))   
freq

#termini che appaiono più volte nelle recensioni
findFreqTerms(dtm, lowfreq=50)


#trasformazione del documento delle frequenze in un dataset più ordinato
wf <- data.frame(word=names(freq), freq=freq)
wf

#grafico frequenze parole del dataset 
#grafico per parole che compaiono almeno 50 volte
p <- ggplot(subset(wf, freq>30), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 10))
p   

#tracciamento di parole con wordcloud
set.seed(10)
wordcloud(names(freq), freq, min.freq=20,max.words = 30)

#SENTIMENT DI RECENSIONI NON PULITE
sentimento_recensioni <- sentiment(x$Review.Content)
qplot(sentimento_recensioni$sentiment,   geom="histogram",binwidth=0.1,main="Review Sentiment Histogram")











#OSSERVAZIONE COLONNE DEL DATASET 
Recensioni_amazon$Name
Recensioni_amazon$Rating
Recensioni_amazon$`Review Title`
Recensioni_amazon$`Review Date`
Recensioni_amazon$`Review Content`




