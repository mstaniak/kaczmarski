library(stringr)
library(rvest)
library(tidyverse)
load("wiersze.rda")
load("tytuly.rda")
load("daty.rda")
# Usunięcie nawiasów z przodu
# Usunięcie znaków przestankowych
# Usunięcie nadmiarowych białych znaków
# Sprowadzenie wszystkich literów do małych liter
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
