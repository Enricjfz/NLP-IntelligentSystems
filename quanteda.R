library(quanteda)
library(syuzhet)
library("quanteda.sentiment")
library("ggplot2")
data("data_dictionary_LSD2015", package = "quanteda.sentiment")
data("data_dictionary_NRC",package = "quanteda.sentiment")

#codigo de quanteda

cs = corpus(paragraphs)
tokens_lookup(tokens(paragraphs[3]), dictionary = data_dictionary_LSD2015, exclusive = FALSE)

polarity(data_dictionary_NRC)
polarity(data_dictionary_NRC) <- list(pos = "positive", neg = "negative")

sent_pres <- cs %>%
  textstat_polarity(data_dictionary_NRC)
sent_pres
ggplot(sent_pres) +
  geom_point(aes(x = reorder(doc_id, sentiment), y = sentiment)) +
  ylab("")  #no es muy legible

toks <- tokens(paragraphs, remove_punct = TRUE, remove_numbers = TRUE)
doc.tokens <- tokens_select(toks, stopwords('spanish'),selection='remove')
doc.tokens <- tokens_wordstem(doc.tokens)
doc.tokens <- tokens_tolower(doc.tokens)
valence(data_dictionary_LSD2015) <- list(positive = 1, negative = -1)
doc.dfm.final <- dfm(doc.tokens)
textstat_valence(toks, data_dictionary_LSD2015)
dfm_lookup(doc.dfm.final, data_dictionary_LSD2015)
tokens_lookup(toks, data_dictionary_LSD2015, exclusive = FALSE, 
              nested_scope = "dictionary")

View(kwic(doc.tokens, "amor", window = 3))
ggplot(doc.dfm.final)
tail(cs) %>%
  textstat_polarity(dictionary = data_dictionary_geninqposneg)

tail(cs) %>%
  textstat_valence(dictionary = data_dictionary_ANEW["arousal"])
sent_pol <- tail(cs, 25) %>%
  textstat_polarity(dictionary = data_dictionary_LSD2015)
sent_pol <- dplyr::mutate(sent_pol, polarity = sentiment)
sent_val <- tail(cs, 25) %>%
  textstat_valence(dictionary = data_dictionary_AFINN)

ggplot(data.frame(sent_pol, valence = sent_val$sentiment),
       aes(x = polarity, y = valence)) +
  geom_point()


tokens_lookup(toks, data_dictionary_NRC, nested_scope = "dictionary", 
              exclusive = FALSE)
polarity_q <-textstat_polarity(toks, data_dictionary_NRC)

plot(polarity_q$sentiment)

ggplot(polarity_q) +
  geom_point(aes(x = reorder(doc_id, sentiment), y = sentiment)) +
  ylab("")


