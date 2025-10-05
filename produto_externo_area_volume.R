library(tidyverse)
library(colorspace)
library(grid)


### Cálculo e representação de volume
visualza_volumes<- function(altura,largura, profundidade){
  #Produto exterior para cálculo da área de todas as combinações de largura e profundidade
  
  areas<- outer(largura, profundidade)
  
  df_areas<- 
    as_tibble(areas) %>%
    mutate(largura = row_number()) %>%
    pivot_longer(cols = -largura,
                 names_to = "profundidade",
                 values_to = "area",
                 names_prefix = "V") %>%
    mutate(profundidade = as.numeric(profundidade))
  
  
  #Gera uma matriz de area e volume
  
  volumes<- 
    outer(df_areas$area, altura) 
  
  
  head(volumes)
  df_volumes<-
    as_tibble(volumes) 
  
  
  #GEração de dataframe com todas as dimensões
  df_area_volume<-
    df_areas %>%
    bind_cols(df_volumes) %>%
    pivot_longer(cols= NCOL(df_areas)+1:NCOL(df_volumes),
                 names_to = "altura",
                 values_to = "volume",
                 names_prefix = "V") %>%
    mutate(altura = as.numeric(altura))
  
  titulo<- paste0("L x W x H: ", format(NROW(df_area_volume), big.mark = ".") ," combinações de volume")
  
  #Visualização
  df_area_volume %>%
    ggplot(aes(x= largura, y= profundidade)) +
    geom_tile(aes(fill = volume) ) +
    scale_fill_continuous_sequential(palette = "Inferno") +
    theme_light() +
    theme(panel.background = element_rect(fill = "darkgreen"),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "cm"),
          axis.ticks.margin = unit(0, "lines"),
          strip.background = element_blank(), 
          strip.text =element_blank(),
          text = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          plot.background = element_rect(fill= "darkgreen"),
          plot.title = element_text(family = "Noto Sans", colour = "white", size =14, face = "bold"),
          plot.subtitle = element_text(family = "Noto Sans", colour = "white", size =12, face = "italic"),
          plot.caption = element_text(family = "Noto Sans", colour = "white", size =7, face = "italic")
    ) +
    facet_wrap(altura~.) +
    labs(
      title =  titulo,
      subtitle = "Aplicações de álgebra linear",
      caption = "Fernando Barbalho 2025"
    )
}

#Vetor de altura
altura<- 1:6

#Vetor de largura
largura<- 1:50

#vetor de profundidade
profundidade<- 1:50


visualza_volumes(altura, largura, profundidade)


x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8; names(y) <- paste(y,":", sep = "")
outer(y, x, "^")

outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table: 
mult_3_dim<-  x %o% x %o% y[1:3]

vol_outer<- largura %o% profundidade %o% altura



largura<- 1:10
profundidade <-1:10

altura <-1:2

names(largura) <- largura

names(altura) <- altura

names(profundidade) <- profundidade

vol_outer<- largura %o% profundidade %o% altura

area_outer<- largura %o% profundidade


  

df_fab_areas<- 
  fab %>%
  rownames_to_column(var = "largura") %>%
  pivot_longer(cols = -largura,
               names_to = "profundidade",
               values_to = "area",
               names_prefix = "V") %>%
  mutate(profundidade = as.numeric(profundidade))


fab<- as.data.frame(vol_outer, row.names = largura)

df_fab_vol<-
  fab %>%
  rownames_to_column(var = "largura") %>%
  pivot_longer(cols = -largura,
               names_to = "profundidade",
               values_to = "area",
               names_prefix = "V") %>%
  separate(profundidade, into = c("profundidade","altura")) %>%
  mutate(profundidade = as.numeric(profundidade),
         altura = as.numeric(altura),
         largura =as.numeric(largura))

  