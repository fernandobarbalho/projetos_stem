library(tidyverse)
library(colorspace)
library(grid)

### Cálculo e representação de volume

altura<- 3 #Altura 


#Vetor de largura
largura<- 1:100



#vetor de profundidade
profundidade<- 1:100

#Produto exterior para cálculo da área de todas as combinações de largura e profundidade

areas<- outer(largura, profundidade)

# diagonal<- diag(1:5)
# 
# cross(areas,diagonal)
# crossprod(areas, diagonal)

#geração de dataframe de quatro dimenssões
df_area_volume<-
  map_dfr(1:NROW(largura) , function(i){
    map_dfr(1:NROW(profundidade) , function(j){
      print(i)
      print(j)
      tibble(largura = largura[i], profundidade = profundidade[j], altura = altura, area = areas[i,j], volume = areas[i,j] * altura)
    })
  })


df_area_volume %>%
  ggplot(aes(x= largura, y= profundidade)) +
  geom_tile(aes(fill = volume) ) +
  scale_fill_continuous_sequential(palette = "Inferno") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.ticks.margin = unit(0, "lines"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black"))


df_area_volume %>%
  ggplot(aes(x= largura, y= profundidade)) +
  geom_tile(aes(fill = area) ) +
  scale_fill_continuous_sequential(palette = "Inferno") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.ticks.margin = unit(0, "lines"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black"))

