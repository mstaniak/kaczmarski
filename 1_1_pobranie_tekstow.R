# Biblioteki. ----
library(tidyverse)
library(rvest)
library(stringr)
# Linki do wierszy ----
strona_glowna <- read_html("http://www.kaczmarski.art.pl/tworczosc/wiersze/")
lista_linkow <- strona_glowna %>%
  html_node(xpath = '//*[@id="post-2747"]/div/div/div/div/div/ul') %>%
  html_nodes("a") %>%
  html_attr("href")
save(lista_linkow, file = "lista_linkow.rda")
# Tytuły wierszy ----
tytuly <- strona_glowna %>%
  html_node(xpath = '//*[@id="post-2747"]/div/div/div/div/div/ul') %>%
  html_nodes("a") %>%
  html_text()
save(tytuly, file = "tytuly.rda")
# Wiersze ----
zakres <- 1:732
wiersze <- vector("list", max(zakres))
for(i in zakres) {
  strona_z_wierszem <- read_html(lista_linkow[i])
  wiersz <- strona_z_wierszem %>%
    html_node("div.fusion-text") %>%
    html_nodes("p") %>%
    html_text() %>%
    str_replace_all("\n", " ") %>%
    str_c(sep = " ", collapse = " ")
  wiersze[[i]] <- wiersz
}
save(wiersze, file = "wiersze.rda")
# Daty napisania wierszy ----
daty <- vector("list", max(zakres))
for(i in zakres) {
  data_proba <- read_html(lista_linkow[[i]])
  tmp <- data_proba %>%
    html_nodes(css = ".fusion-text")
  daty[[i]] <- tmp[2]
  cat(".")
}
save(daty, file = "daty.rda")
## Zamiana dat na tekst
daty_tekst <- lapply(daty, html_text)
save(daty_tekst, file = "daty_tekst.rda")
## Sam rok
daty_rok <- lapply(daty_tekst, function(x) str_extract_all(x, "[0-9]{4}"))
daty_rok_vec <- unlist(daty_rok, recursive = F)
daty_rok_vec <- lapply(daty_rok, function(x) x[1])
save(daty_rok, file = "daty_rok.rda")
## Zastąpienie podwójnych lat jedną datą, brakujących NA
# hist(unlist(sapply(daty_rok_vec, function(x) length(x[[1]]))))
# podwojne_lata <- daty_rok_vec[unlist(sapply(daty_rok_vec, function(x) length(x[[1]]) == 2))]
# which(sapply(daty_rok_vec, function(x) length(x[[1]]) == 2))
daty_rok_vec <- lapply(daty_rok_vec, function(x) {
  if(length(x[[1]]) > 0) {
    x[[1]][1]
  } else {
    list(NA)
  }
})
daty_rok_tib <- vector("list", 732)
for(i in 1:732) {
  daty_rok_tib[[i]] <- tibble(id = i, rok = daty_rok_vec[[i]][[1]])
}
lata_tib <- bind_rows(daty_rok_tib)
save(lata_tib, file = "lata_tib.rda")
# Połączenie informacji ----
wiersze_tib <- vector("list", 732)
for(i in 1:732) {
  wiersze_tib[[i]] <- tibble(id = i, wiersz = wiersze[[i]])
}
wiersze_tib <- bind_rows(wiersze_tib)
calosc <- left_join(lata_tib, wiersze_tib, by = "id")
tytuly_tib <- tibble(id = 1:732, tytul = tytuly)
calosc <- left_join(calosc, tytuly_tib, by = "id")
save(calosc, file = "calosc.rda")
