library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

pop_rua_pob_zero <- read_xls("/Users/wemigliari/Documents/R/tabelas/pop_rua_serie_pobreza_corr.xls")
pop_rua_pob_zero <- data.frame(pop_rua_pob_zero)

ggplot(pop_rua_pob_zero, aes(pop_rua_pob_zero$Período)) + 
  geom_line(aes(y = pop_rua_pob_zero$familias_situacao_rua, colour = "População de Rua")) + 
  geom_line(aes(y = pop_rua_pob_zero$familias_renda_zero, colour = "Renda Zero")) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") 


ggplot(pop_rua_pob_zero, aes(pop_rua_pob_zero$Período,
                             pop_rua_pob_zero$familias_situacao_rua, color = "Situação de Rua")) +
  geom_line(aes(linetype=eval(pop_rua_pob_zero$familias_situacao_rua>="2020-09-01"))) +
geom_line(aes(y = pop_rua_pob_zero$familias_renda_zero, colour = "Renda Zero")) +
theme_bw()
  
#scale_linetype_manual('type of line', values=c(1,2))
  
ggplot(pop_rua_pob_zero, aes(Período, familias_situacao_rua, group=ID)) +
  stat_summary(data=subset(pop_rua_pob_zero,Período %in% c("Estimate","Real")), aes(linetype='solid line')) +
  stat_summary(data=subset(pop_rua_pob_zero,Período %in% c("Estimate","Real")),  aes(linetype='dashed line'))


