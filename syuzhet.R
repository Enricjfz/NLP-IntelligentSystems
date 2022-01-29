library(quanteda)
library(syuzhet)
library("tokenizers.bpe")
library(utf8)
library(spacyr)
library(pander)
library(tm)


#codigo de tratamiento de texto y syuzhet

urlQuijoteGutenber <- "https://www.gutenberg.org/files/2000/2000-0.txt"
lines <- readLines(urlQuijoteGutenber,
                   encoding = "UTF-8") #It takes a few seconds
grep(pattern = "***", lines, fixed = TRUE) #Warning! Without fixed the regex is "\\*\\*\\*"

linesQ <- lines[25:37703]
length(linesQ) #37,679


grep(pattern = "En un lugar de",
     linesQ,
     fixed = TRUE) #Lines 1045 and 13513. The good one is the first one

linesQ <- linesQ[-c(1:1044)] #Remove the prologue
length(linesQ) #36,635

#Check character normalization. Specifically, the normalized composed form (NFC)
linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ) #0 means all right. The text is in NFC.
linesQ

stringQ <- paste(linesQ, collapse = "\n") #One big string
paragraphs <- unlist(strsplit(stringQ, "\\n\\n\\n"))#Warn! (1)strsplit returns a list,
# (2)escape \n and
# (3)by default, fixed=FALSE
#Using fixed=TRUE this should be
# "\n\n\n", fixed = TRUE
parEmpty <- which(paragraphs == "") #No empty paragraphs
#paragraphs <- paragraphs[-parEmpty]
length(paragraphs) # 128

substring(paragraphs[1], 1, 200)

#Testing the regex
gsub("[\n]{1,}", " ", c(par1="with one \nbut also\n",
                        par2="with a seq of \n\nlike this"
)
)
paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs) #wo = without
substring(paragraphswoNL[1], 1, 200)

paragraphs <- gsub("[ ]{2,}", " ", paragraphswoNL) #We reassign the varible paragraphs
substring(paragraphs[1], 1, 200)

#fin del preproceso, inicio syuzhet

s_v <- get_sentences(paragraphs)
class(s_v)
str(s_v)
head(s_v)
syuzhet_vector <- get_sentiment(s_v,method="nrc", language = "spanish")
sum(syuzhet_vector)
mean(syuzhet_vector)
summary(syuzhet_vector)
plot(
  syuzhet_vector, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
plot(
  syuzhet_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
percent_vals <- get_percentage_values(syuzhet_vector, bins = 10)
plot(
  percent_vals, 
  type="l", 
  main="Joyce's Portrait Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)
#Using the optional bins argument, you can control how many 
#sentences are included inside each percentage based chunk

percent_vals <- get_percentage_values(syuzhet_vector, bins = 20)
plot(
  percent_vals, 
  type="l", 
  main="Joyce's Portrait Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)
dct_values <- get_dct_transform(
  syuzhet_vector, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
simple_plot(syuzhet_vector)
nrc_data <- get_nrc_sentiment(s_v, language = "spanish", lowercase = TRUE)
trust_items <- which(nrc_data$anger > 0)
s_v[trust_items]
pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)
valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
#valence barplot
par(mfrow = c(1,2))
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1,
  col = c(1,2,3,4,5,6,7,8),
  main = "Emotions in Sample text", xlab="Percentage"
)
barplot(
  sort(colSums(prop.table(nrc_data[, 9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  col = c(1,2),
  main = "Emotions in Sample text", xlab="Percentage"
)
boxplot(nrc_data)
plot(valence)
