rm(list = ls())
setwd("~")

library(rvest)
library(httr) #comunicacion con API
library(xml2)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(lubridate)
library(scales)
library(stringr)
library(ggrepel)

#Instrucciones del blog 
#https://medium.com/@jboscomendoza/webscrapping-apis-y-miner%C3%ADa-de-texto-con-r-an%C3%A1lisis-de-sentimientos-de-coheed-and-cambria-d0f24804da1d

#http://rpubs.com/jboscomendoza/coheed_and_cambria

# Información de canciones/albumes ----------------------------------------
#se utilizara musicbrainz para recuperar metadatos de las canciones

joy <- read_html("https://musicbrainz.org/release/0e7b387d-c6e1-4877-9826-4080e3a890eb")
brut <- read_html("https://musicbrainz.org/release/6d4e53b2-82d1-4154-907d-00f243284f2b")

#Joy as an act of resistance
cj <- joy %>% 
  html_nodes(css = "tbody tr bdi") %>% 
  html_text() %>% 
  data.frame() %>%
  tbl_df() %>%
  mutate_all(trimws)
cj <- cj[-c((12:19)),] #tenía los datos dell cover
colnames(cj) <- "cancion"

aj <- joy %>%
  html_nodes(css = ".releaseheader h1") %>%
  html_text()

fj <- joy %>% 
  html_nodes(css = ".release-date") %>%
  html_text()

joya <- cj %>% 
  mutate(
  album = aj,
  fecha = fj
)

#Brutalism
cb <- brut %>% 
  html_nodes(css = "tbody tr bdi") %>% 
  html_text() %>% 
  data.frame() %>%
  tbl_df() %>%
  mutate_all(trimws)
colnames(cb) <- "cancion"

ab <- brut %>%
  html_nodes(css = ".releaseheader h1") %>%
  html_text()

fb <- brut %>% 
  html_nodes(css = ".release-date") %>%
  html_text()

bruta <- cb %>% 
  mutate(
    album = ab,
    fecha = fb
  )

idles <- bind_rows(joya,bruta)

# Letras desde API --------------------------------------------------------

mi_api_key <- "efivyEwYfTBxOtsx1Z9Up89jmpfBbm4AeawnzPU3sIPeEq2esfWhAfi5NkIeSHoi"

cc_letras_lista <-  map(idles[["cancion"]], function(x){
    ruta <-  paste0(
      "https://orion.apiseeds.com/api/music/lyric/idles/",
      x,
      "?apikey=",
      mi_api_key
    )
    GET(url = ruta)
  })

#Esto fue para conseguir todas las letras
#No se pueden utilizar así directamente

mis_letras_df <- cc_letras_lista %>%
  map(~content(., as = "text", encoding = "UTF-8")) %>%
  map(~ifelse(grepl("error|Bad Request|html", .), NA, .)) %>%
  map(function(x) {
    if(!is.na(x)) {
      y <- fromJSON(x)
      c(cancion = y$result$track$name,
        letra = y$result$track$text) %>%
        gsub("[[:cntrl:]]", " ", .) %>%
        gsub("\\[.*?\\]", " ", .) %>%
        gsub("[\"]", " ", .) %>% 
        trimws()
    } else {
      c(cancion = NA, letra = NA)
    }
  }) %>%
  do.call(what = bind_rows)
#No todas las canciones tienen letras en la API

mis_letras_df[2,1] <- "Never Fight a Man With a Perm"
mis_letras_df[3,1] <- "I’m Scum"
#Estan las letras, pero no coinciden con el df original, por eso se cambio

id_tot <- idles %>% 
  left_join(mis_letras_df, by = "cancion")

id_tot <- id_tot %>% 
  mutate(
    fecha = ymd(fecha),
    album = reorder(as.factor(album), fecha)
  )

# Analisis de sentimientos ------------------------------------------------
#El blog solo revisa 5 emociones básicas

idles_tokens <- id_tot %>% 
  unnest_tokens(input = "letra", output = "word") %>% 
  inner_join(., get_sentiments(lexicon = "nrc"), by = "word") %>%  
  filter(!sentiment %in% c("positive", "negative", "trust", "surprise", "anticipation"))

idles_tokens %>% 
  group_by(sentiment) %>% 
  count(word, sort = T) %>% 
  top_n(15) %>% 
  ggplot() +
  aes(word, n, fill = sentiment) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  theme(legend.position = "none")

idles_tokens %>% 
  group_by(album, cancion) %>% 
  count(sentiment) %>% 
  mutate(prop = n / sum(n)) %>% 
  group_by(sentiment) %>% 
  top_n(5) %>% 
  ggplot() +
  aes(sentiment, prop, color = sentiment) +
  geom_point() +
  geom_text_repel(aes(label = paste0(cancion, "\n", album)), 
            vjust = -.3, size = 3) +
  scale_y_continuous(limits = c(0.15, 0.6)) +
  theme_minimal() +
  theme(legend.position = "none") 


graficar_cancion <- function(sentimiento, cantidad = 7) {
  idles_tokens %>% 
    group_by(album, cancion) %>% 
    count(sentiment) %>% 
    mutate(prop = n / sum(n)) %>% 
    group_by(sentiment) %>% 
    top_n(cantidad) %>% 
    filter(sentiment == sentimiento) %>%
    mutate(cancion = paste0(cancion, "\n(", album, ")"),
           cancion = reorder(cancion, prop)) %>%
    ggplot() +
    aes(cancion, prop) +
    geom_col(position = "dodge", fill = "#bb88ff") +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(family = "serif")) +
    coord_flip() +
    labs(title = paste0("IDLES\nSongs with more ", sentimiento),
         x = "Song (Album)", y = "") +
    scale_y_continuous(limits = c(0, .6), expand = c(0, 0), label = percent_format())
}


unique(idles_tokens$sentiment) %>% 
  map(graficar_cancion)

