library(stringr)
library(rvest)
library(tidyverse)
load("wiersze.rda")
load("tytuly.rda")
load("daty.rda")
# Usunięcie nawiasów z przodu
wiersze2 <- lapply(wiersze, function(x) x)
# Usunięcie znaków przestankowych
wiersze_bez_przestankowych <- lapply(wiersze2, function(x) str_replace_all(x, "-", " "))
wiersze_bez_przestankowych <- lapply(wiersze_bez_przestankowych, function(x) str_replace_all(x, "[:punct:]+", ""))
# Sprowadzenie wszystkich literów do małych liter
wiersze_male_litery <- lapply(wiersze_bez_przestankowych, str_to_lower)
wiersze_male_litery[[100]]
# Podział na słowa
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
proba_lemma <- getLemma(wiersze_male_litery[[1]], "mateusz.staniak@math.uni.wroc.pl")
proba_lemma
wiersze_lematyzacja <- vector("list", 732)
for(i in 80:732) {
  wiersze_lematyzacja[[i]] <- try(getLemma(wiersze_male_litery[[i]], "mateusz.staniak@math.uni.wroc.pl"))
}
save(wiersze_lematyzacja, file = "wiersze_lematyzacja.rda")
# Usunięcie słów stopu
stopwords <- c("a", "aby", "ale", "bardziej", "bardzo", "bez", "bo", "bowiem", "był", "była",
               "było", "były", "będzie", "co", "czy", "czyli", "dla", "dlatego", "do", "gdy",
               "gdzie", "go", "i", "ich", "im", "innych", "iż", "jak", "jako", "jednak", "jego",
               "jej", "jest", "jeszcze", "jeśli", "już", "kiedy", "kilka", "która", "które", "którego",
               "której", "który", "których", "którym", "którzy", "lub", "ma", "mi", "między", "mnie",
               "mogą", "może", "można", "na", "nad", "nam", "nas", "naszego", "naszych", "nawet", "nich",
               "nie", "nim", "niż", "o", "od", "oraz", "po", "pod", "poza", "przed", "przede", "przez",
               "przy", "również", "się", "sobie", "swoje", "są", "ta", "tak", "takie", "także", "tam",
               "te", "tego", "tej", "ten", "też", "to", "tu", "tych", "tylko", "tym", "u", "w", "we", "wiele",
               "wielu", "więc", "wszystkich", "wszystkim", "wszystko", "właśnie", "z", "za", "zawsze", "ze", "że")
# Pobrac wiekszy zestaw
stop_words_link <- "https://www.ranks.nl/stopwords/polish"
wiersze_bez_stopu <- lapply(wiersze_lematyzacja, function(x) x[!(x %in% stopwords)])
# wiersze_bez_stopu <- lapply(wiersze_lematyzacja1_79, function(x) setdiff(x, stopwords))
wiersze_bez_stopu[[2]]
library(wordcloud)
czeste_slowa <- as.data.frame(table(unlist(wiersze_bez_stopu))) %>%
  arrange(desc(Freq))
save(czeste_slowa, file = "czeste_slowa.rda")
### Czy daty są dobrze ściągnięte?
daty_tekst <- lapply(daty, html_text)
same_daty <- str_extract_all(daty_tekst, "[0-9]{4}")
tibble(x = unlist(daty_tekst)) -> proba
table(unlist(lapply(same_daty, length)))
save(daty_tekst, file = "daty_tekst.rda")
# same_daty[unlist(lapply(same_daty, length)) == 2]
# tytuly[unlist(lapply(same_daty, length)) == 2]
daty_z_na <- lapply(same_daty, function(x) if(length(x) == 0) NA else min(x))
daty_z_na <- unlist(daty_z_na, use.names = FALSE)
tworczosc <- tibble(tytul = tytuly, rok = daty_z_na)

tworczosc2 <- tworczosc %>%
  filter(tytul != "Mur",
         !str_detect(tworczosc$tytul, "wiadomości"),
         !str_detect(tworczosc$tytul, "wątpliwe"),
         !str_detect(tworczosc$tytul, "dramat na "),
         !str_detect(tworczosc$tytul, "Na rusztach"),
         !str_detect(tworczosc$tytul, "Słowo o Bruno"),
         !str_detect(tworczosc$tytul, "Estaca"),
         !str_detect(tworczosc$tytul, "The Stake"),
         !str_detect(tworczosc$tytul, "szkic"))


czy_brak_daty <- unlist(lapply(same_daty, function(x) length(x) == 0))
lista_linkow[czy_brak_daty]

wszystkie_nietypowe <- which(czy_brak_daty)[c(1:4, 6, 15, 16)]
do_usuniecia <- which(czy_brak_daty)[-c(1:4, 6, 15, 16)]

same_daty[do_usuniecia] <- NULL
tytuly[do_usuniecia] <- NULL



lista_linkow[do_usuniecia]
wiersze[do_usuniecia] <- NULL
tytuly[do_usuniecia] <- NULL
daty[do_usuniecia] <- NULL
# same_daty[czy_brak_daty] <- NA
# same_daty <- unlist(same_daty, use.names = FALSE)
grep("Niech", tytuly, value = T)
