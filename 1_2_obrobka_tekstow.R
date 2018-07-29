library(stringr)
library(rvest)
library(tidyverse)
load("calosc.rda")
calosc <- filter(calosc, !is.na(wiersz))
wiersze <- as.list(calosc$wiersz)
# # Usunięcie nawiasów z przodu
# wiersze2 <- lapply(wiersze, function(x) x)
# Usunięcie znaków przestankowych
wiersze_bez_przestankowych <- lapply(wiersze, function(x) str_replace_all(x, "-", " "))
wiersze_bez_przestankowych <- lapply(wiersze_bez_przestankowych, function(x) str_replace_all(x, "[:punct:]+", ""))
# Sprowadzenie wszystkich literów do małych liter
wiersze_male_litery <- lapply(wiersze_bez_przestankowych, str_to_lower)
wiersze_male_litery[[100]]
# Podział na słowa ze zignorowaniem lematyzacji
wiersze_slowa <- lapply(wiersze_male_litery, function(x) str_split(x, " "))
# Lematyzacja
library(httr)
library(xml2)
getLemma <- function(t,u) {
  p <- list(lpmn="any2txt|wcrft2",text=t,user=u)
  s <- POST("http://ws.clarin-pl.eu/nlprest2/base/process", body = p, encode = "json", verbose())
  r <- httr::content(s, "text")
  r <- gsub('[[:punct:] ]+','',unlist(as_list(xml_find_all(read_xml(r),"//base"))))
  return(r[r != ""])
}
wiersze_lematyzacja <- vector("list", length(wiersze))
for(i in 1:length(wiersze)) {
  wiersze_lematyzacja[[i]] <- try(getLemma(wiersze_male_litery[[i]], "mateusz.staniak@math.uni.wroc.pl"))
}
save(wiersze_lematyzacja, file = "wiersze_lematyzacja.rda")
load("wiersze_lematyzacja.rda")
# Usunięcie słów stopu
stop_words <- unlist(jsonlite::read_json("https://raw.githubusercontent.com/stopwords-iso/stopwords-pl/master/stopwords-pl.json"))
# wiersze_bez_stopu <- lapply(wiersze_lematyzacja, function(x) x[!(x %in% stopwords)])
wiersze_bez_stopu_unikalne <- lapply(wiersze_lematyzacja, function(x) setdiff(x, stop_words))
wiersze_bez_stopu <- lapply(wiersze_lematyzacja, function(x) x[!(x %in% stop_words)])
save(wiersze_bez_stopu_unikalne, file = "wiersze_bez_stopu_unikalne.rda")
save(wiersze_bez_stopu, file = "wiersze_bez_stopu.rda")

library(wordcloud)
czeste_slowa <- as.data.frame(table(unlist(wiersze_bez_stopu))) %>%
  arrange(desc(Freq))
head(czeste_slowa, 20)
save(czeste_slowa, file = "czeste_slowa.rda")

liczebniki <- c("zero", "jeden", "dwa", "trzy", "cztery", "pięć", "sześć",
                "siedem", "osiem", "dziewięć", "dziesięć", "jedenaście",
                "dwanaście", "trzynaście", "czternaście", "piętnaście",
                "szesnaście", "siedemnaście", "osiemnaście", "dziewiętnaście",
                "dwadzieścia", "trzydzieści", "czterdzieści", "pięćdziesiąt",
                "sześćdziesiąt", "siedemdziesiąt", "osiemdziesiąt",
                "dziewięćdziesiąt", "sto", "tysiąc", "milion")
wiersze_bez_liczebnikow <- lapply(wiersze_bez_stopu,
                                  function(x) x[!(x %in% liczebniki)])

wiersze_bez_stopu_vecs <- sapply(wiersze_bez_stopu,
                                 function(x) paste(x, collapse = " "))
wiersze_bez_liczebnikow_vecs <- sapply(wiersze_bez_liczebnikow,
                                       function(x) paste(x, collapse = " "))

