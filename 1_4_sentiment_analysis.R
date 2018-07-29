# Materiały:
  # https://www.r-bloggers.com/natural-language-processing-for-non-english-languages-with-udpipe/
  # http://nlp.ipipan.waw.pl/NLP-SEMINAR/071015.pdf
# + wyniki google "jezyk polski sentiment analysis"
library(stringr)
library(rvest)
library(tidyverse)
library(tm)
library(topicmodels)
library(tidytext)
top_n <- 4
wiersze_src <- VectorSource(wiersze_bez_stopu_vecs)
wiersze_corp <- VCorpus(wiersze_src)
wiersze_dtm <- DocumentTermMatrix(wiersze_corp,
                                  control = list(weighting = weightTf))
wiersze_lda <- LDA(wiersze_dtm, k = top_n)

wiersze_src2 <- VectorSource(wiersze_bez_liczebnikow_vecs)
wiersze_corp2 <- VCorpus(wiersze_src2)
wiersze_dtm2 <- DocumentTermMatrix(wiersze_corp2,
                                   control = list(weighting = weightTf))
wiersze_lda2 <- LDA(wiersze_dtm2, k = top_n)

wiersze_topics <- tidy(wiersze_lda, matrix = "beta")
ap_top_terms <- wiersze_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

wiersze_topics2 <- tidy(wiersze_lda2, matrix = "beta")
ap_top_terms2 <- wiersze_topics2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
