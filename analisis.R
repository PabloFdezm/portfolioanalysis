##### Trabajo R

library(readxl)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(tidyr)

##### Datos

vivienda <- read_excel("C:/Users/pablo/Downloads/Vivienda.xlsx", col_types = c("date", "numeric"))

ce <- read_excel("C:/Users/pablo/Downloads/Comercio Exterior.xlsx", col_types = c("date", "numeric"))

comerciales <- read_excel("C:/Users/pablo/Downloads/Comerciales.xlsx", col_types = c("date", "numeric"))

consumo <- read_excel("C:/Users/pablo/Downloads/Consumo.xlsx", col_types = c("date", "numeric"))

#####

data <- Reduce(function(x,y) merge(x,y, all = T), list(vivienda, ce, comerciales, consumo))
colnames(data) <- c('Periodo', 'Vivienda', 'Comercio_Exterior', 'Comerciales', 'Consumo')

p <- data %>%
  filter(Periodo >= '2018-01-01') %>%
  mutate(Periodo = as.Date(Periodo)) %>%
  pivot_longer(-Periodo, names_to = 'class') %>%
  ggplot(aes(x = Periodo))+
  geom_line(aes(y = value, colour = class))

p <- p + scale_y_continuous(
    #breaks = c(0,2,4,6,8,10,12),
    sec.axis = sec_axis(~.*1)
  )

p <- p + scale_x_continuous(
  breaks = c(seq(as.Date('2018-01-01'), length.out=8, by='6 month')),
  labels = paste0(rep(c('Ene','Jul'),4), '-', rep(c(18:21),each=2))
)

p <- p + theme(
  panel.background = element_rect(colour='white',fill = 'white'),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = 'grey'),
  panel.grid.minor.y = element_line(colour = 'grey'),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_line(colour = 'grey'),
  axis.line.x = element_line(colour = 'grey'),
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.title = element_blank()
)

p <- p + labs(
  x= '',
  y= ''
)
  
p <- p + scale_fill_continuous(labels = c('Comerciales', 'Comercio Exterior', 'Consumo', 'Vivienda'))

p
