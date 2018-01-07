library(tidyverse)
library(rvest)
library(stringr)
library(wordcloud)

strona_glowna <- read_html("http://www.kaczmarski.art.pl/tworczosc/wiersze/")
lista_linkow <- strona_glowna %>%
  html_node(xpath = '//*[@id="post-2747"]/div/div/div/div/div/ul') %>%
  html_nodes("a") %>%
  html_attr("href")

link <- lista_linkow[1]
strona_z_wierszem <- read_html(link)

wiersz <- strona_z_wierszem %>%
  html_node("div.fusion-text") %>%
  html_nodes("p") %>%
  html_text() %>%
  str_replace_all("\n", " ") %>%
  str_c(sep = " ", collapse = " ")


