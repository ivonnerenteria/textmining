######## Estudio Pastoral Jesuita 2018 ####

##### Text mining and WordCloud  ####

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("readxl")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("readxl")
# Establecer directorio de trabajo
setwd("C:/Users/Inmobiliaria/OneDrive/Ivonne/Data_jesuita")
#Cargar la base de datos
bd <- read_excel(choose.files())
# Análisis descriptivo
str(bd)
bd$Sexo    = factor(bd$Sexo)
bd$Cargo = factor(bd$Cargo)
bd$porc_catolicos <- as.numeric(as.character(bd$porc_catolicos))
bd$Num_lideres <- as.numeric(as.character(bd$Num_lideres))
summary(bd)
# Histograma de porcentaje de católicos
hist(bd$porc_catolicos, 
     main="Histograma del porcentaje de Católicos", 
     xlab="Porcentaje en la comunidad", 
     border="brown", 
     col="red"
     #,ylim=c(0,30), 
    # las=1, 
    # breaks=5, 
     #prob = TRUE
    )

#Caracter número de líderes
#bd$Num_lideres = factor(bd$Num_lideres)
#summary(bd$Num_lideres)

################## Text Mining Espiritualidad Ignaciana  ####
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice", 
                                    "espiritualidad", "ignaciana", "demas")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############    Text Mining Descripción de Acompañamiento   #######
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice","así",
                                    "acompanhamiento","acompanhante","jovenes"
                                    ,"traves" ,"ademas", "mass")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############    Text Mining Descripción de Fortaleza   #######
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice",
                                    "así","algún","través","también","además",
                                    "jovenes","mas", "demas")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############    Text Mining Descripción de Debilidad  #######
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice",
                                    "así","algún","través","también","además")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############    Text Mining Descripción de Expectativa  #######
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice",
                                    "así","algún","través","también","además",
                                    "mass","demas", "sino", "solo")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############    Text Mining Descripción de Forma de promover Líderes  #######
bd$For_promoLid = factor(bd$For_promoLid)
summary(bd$For_promoLid) #15 NA's

text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice",
                                    "así","algún","través","también","además",
                                    "promueve","promueven","creo",
                                    "lideres","liderazgo","xxx")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############    Text Mining Descripción de Motivación  #######
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice",
                                    "así","algún","través","también","además",
                                    "promueve","promueven",
                                    "lideres","liderazgo",
                                    "motiva","valoro",
                                    "sino","hecho")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 6,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

################## Text Mining Nombre de Comunidad y/o Apostolado ####
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("demás", "etc", "ende", "cada", "dice")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
# WordCloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))