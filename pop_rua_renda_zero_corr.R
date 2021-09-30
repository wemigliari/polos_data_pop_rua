library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

pop_rua_pob_zero <- read_xls("/Users/wemigliari/Documents/R/tabelas/pop_rua_serie_pobreza_corr.xls")
pop_rua_pob_zero <- data.frame(pop_rua_pob_zero)

rects <- data.frame(xstart = as.POSIXct('2020-09-01'), 
                    xend = as.POSIXct('2030-01-01'))
data <- c(as.POSIXct('2013-12-01'), as.POSIXct('2022-04-01'))
pop <- c(1000, 1000)
label <- c("Dados Amostrais","Dados Projetados")
df <-data.frame(data,pop,label)


ggplot(pop_rua_pob_zero) + 
  geom_line(aes(Período, familias_situacao_rua), color = "steelblue", size = 0.2) + 
  geom_line(aes(Período, familias_renda_zero), color = "black", size = 0.2) +
  theme_bw() +
  labs(title="",
       y = "", x = "", caption = "Fonte: CadÚnico. Elaborado por Migliari, W. (2021).") +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -Inf, ymax = Inf), alpha = 0.05) +
  geom_text(data = df, aes(x = data, y = pop, label = label), color = "darkgray",
            size = 3, vjust = 0, hjust = -0.5)

#####

ggplot(pop_rua_pob_zero, aes(Período, familias_situacao_rua)) + 
  theme_bw() + 
  geom_line(data=subset(pop_rua_pob_zero, Período>="2020-09-01"), aes(linetype='Dados Estimados')) +
  geom_line(data=subset(pop_rua_pob_zero, Período<="2020-09-01"), aes(linetype='Dados Amostrais')) +
  scale_linetype_manual('População em Situação de Rua', values=c(1,2)) +
  geom_line(aes(Período, familias_renda_zero)) +
  labs(title="",
       y = "", x = "", caption = "Fonte: CadÚnico. Elaborado por Migliari, W. (2021).") 


#####

x <- c(as.POSIXct('2025-12-01'), as.POSIXct('2022-04-01'))
y <- c(12000, 32000)
label <- c("População Situação de Rua","Renda Zero")
df <-data.frame(data,pop,label)


ggplot(data = pop_rua_pob_zero, aes(x = Período, y = familias_situacao_rua, 
                                    fill = Dados, shape = Dados)) +
  geom_point(size = 3) +
  geom_point(aes(Período, familias_renda_zero), size = 1) +
  scale_shape_manual(values = c(24, 21)) +
  theme_bw() +
  scale_fill_manual(
    values = c("gray", "#EDB4B5")) +
  labs(title="",
       y = "", x = "", caption = "Fonte: CadÚnico, Dados sobre Belo Horizonte, Minas Gerais. Elaborado por Migliari, W. (2021).")

#####

ggplot(subset(pop_rua_pob_zero,ID %in% c("Estimate" , "Real"))) + 
  geom_line(aes(pop_rua_pob_zero$Período, 
                pop_rua_pob_zero$familias_situacao_rua, group=ID, colour=ID))


