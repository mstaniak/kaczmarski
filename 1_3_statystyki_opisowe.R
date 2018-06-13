# Biblioteki i dane
library(tidyverse)
library(wordclod)
library(archivist)
load("calosc.rda")
load("wiersze_bez_stopu_unikalne.rda")
load("wiersze_bez_stopu.rda")
# Dlugosc wiersza na przestrzeni lat
calosc <- filter(calosc, !is.na(wiersz))
calosc %>%
  mutate(dlugosc = sapply(wiersze_bez_stopu, length)) %>%
  group_by(rok) %>%
  summarise(dlugosc_wiersza = mean(dlugosc, na.rm = T)) %>%
  ggplot(aes(x = rok, y = dlugosc_wiersza)) +
    geom_point() +
    theme_bw()
# Bogactwo slownictwa
calosc %>%
  mutate(unikalne_slowa = sapply(wiersze_bez_stopu_unikalne, length)) %>%
  group_by(rok) %>%
  summarise(liczba_unikalnych = mean(unikalne_slowa, na.rm = T)) %>%
  ggplot(aes(x = rok, y = liczba_unikalnych)) +
  geom_point() +
  theme_bw()
calosc <- mutate(calosc, zlematyzowane_unikalne = wiersze_bez_stopu_unikalne,
                 zlematyzowane = wiersze_bez_stopu)
calosc %>%
  group_by(tytul) %>%
  summarise(top_word = head(as.data.frame(table(zlematyzowane)), 1)[, 1])
czestosci <- vector("list", length(wiersze_bez_stopu))
for(i in 1:723) {
  tmp <- as_tibble(table(wiersze_bez_stopu[[i]])) %>%
    rename(slowo = Var1, czestosc = n) %>%
    arrange(desc(czestosc))
  czestosci[[i]] <- tibble(id = rep(calosc$id[i], nrow(tmp)),
                           rok = rep(calosc$rok[i], nrow(tmp)),
                           tytul = rep(calosc$tytul[i], nrow(tmp)),
                           slowo = tmp$slowo,
                           czestosc = tmp$czestosc)
}
czestosci <- bind_rows(czestosci)
save(czestosci, file = "czestosci.rda")

top_n <- 2
czestosci %>%
  group_by(rok, slowo) %>%
  summarise(czestosc = sum(czestosc, na.rm = T)) %>%
  arrange(rok, desc(czestosc)) %>%
  group_by(rok) %>%
  summarise(top_word = head(slowo, top_n)[top_n],
            czestosc = head(czestosc, top_n)[top_n]) %>%
  ggplot(aes(x = rok, y = czestosc, label = top_word)) +
    geom_text() +
    theme_bw() +
    geom_point()
