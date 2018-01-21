library(tidyverse)
library(rvest)
library(stringr)
library(wordcloud)

strona_glowna <- read_html("http://www.kaczmarski.art.pl/tworczosc/wiersze/")
lista_linkow <- strona_glowna %>%
  html_node(xpath = '//*[@id="post-2747"]/div/div/div/div/div/ul') %>%
  html_nodes("a") %>%
  html_attr("href")
save(lista_linkow, file = "lista_linkow.rda")

tytuly <- strona_glowna %>%
  html_node(xpath = '//*[@id="post-2747"]/div/div/div/div/div/ul') %>%
  html_nodes("a") %>%
  html_text()
save(tytuly, file = "tytuly.rda")

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

daty <- vector("list",  max(zakres))

for(i in zakres) {
  data_proba <- read_html(lista_linkow[[i]])
  tmp <- data_proba %>%
    html_nodes(css = ".fusion-text")
  daty[[i]] <- tmp[2] %>%
    html_nodes("p")
  cat(".")
}
save(daty, file = "daty.rda")

