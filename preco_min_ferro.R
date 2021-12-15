library(readxl)
library(dplyr)



test <- library(readxl)

test <- read_excel("Documents/R/tabelas/commodities_price_wb3.xlsx", 
                   col_types = c("numeric", "numeric", "numeric"))

test<- test[-c(1:37), ]


####

library(ggplot2)
library(zoo)
library(hrbrthemes)


####Graph 1

colors <- c("Valor Nominal" = "red", "Valor Real" = "gold")

p <- ggplot(test, aes(x=year)) +
  geom_line(aes(y = iron_price_nom, color = "Valor Nominal"), size = 1.3, linetype = "dotted") + 
  geom_line(aes(y = iron_ore_real, color = "Valor Real"), size = 1.3, linetype = "dotted") + 
  annotate("rect", xmin = 2015, xmax = 2020, ymin = 0, ymax = 180,
           alpha = .2) +
  xlab("") +
  theme_ipsum() +
  theme(plot.caption = element_text(size = 10)) +
  scale_x_yearmon(format="%Y %m", n=20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  annotate(geom="text", x=2015, y=120, label="Rompimento da Barragem do Fundão, Mariana", 
           angle = 90,
           size=2,
           color = "#3E3E3E") +
  annotate(geom="text", x=2019, y=120, label="Rompimento da Barragem da Mina Córrego do Feijão, Brumadinho", 
           angle = 90,
           size=2,
           color = "#3E3E3E") +
  annotate(geom="text", x=1997, y=120, label="Privatização da Companhia Vale do Rio Doce", 
           angle = 90,
           size=2,
           color = "#3E3E3E") +
  geom_vline(xintercept=2015.4, linetype="dotted", color = "red") +
  geom_vline(xintercept=2019.4, linetype="dotted", color = "red") +
  geom_vline(xintercept=1997.4, linetype="dotted", color = "red") +
  labs(title="Gráfico A. Preço da Tonelada de Ferro no Mercado Internacional 1990-2020",
       y = "Preço por Tonelada (US$)", caption = "Fonte: Banco Mundial. Elaborado por Migliari, W. (2021).", color = "") +
  scale_color_manual(values = colors)
  
p

#####Boxplot

test2 <- read_excel("Documents/R/tabelas/commodities_price_wb3.xlsx", 
                    col_types = c("text", "numeric", "numeric"))
test2<- test2[-c(1:37), ]

### Combining multiple columns

library(tidyr)

colnames(test2)[c(2:3)] <- c("Valor Nominal - Boxplot A","Valor Real - Boxplot B")

test3<- test2 %>% gather(iron_price_nom, iron_ore_real, -year) #### https://www.r-bloggers.com/2016/11/use-r-to-combine-multiple-columns-of-data-into-a-single-column-spread-out-across-rows/

colnames(test3)[c(2:3)] <- c("Valores", "quant")


library(ggplot2)
library(viridis)

test3 %>%
  ggplot(aes(x = Valores, y=quant, fill=Valores)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_boxplot() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("") +
  labs(y = "Valores Nominais e Reais das Ações (US$)", caption = "Fonte: Banco Mundial. Elaborado por Migliari, W. (2021).")


####

### Cronbach's Alpha in R

library(psych)


## 1

test_alpha <- read_excel("Documents/R/tabelas/commodities_price_wb3.xlsx", 
                   col_types = c("numeric", "numeric", "numeric"))
test_alpha<- test_alpha[-c(1:37), -c(1)]

test_a <- data.frame(test_alpha)
mean(test_a$iron_price_nom)
sd(test_a$iron_price_nom)
mean(test_a$iron_ore_real)
sd(test_a$iron_ore_real)

alpha(test_a)
cronbach.alpha(test_a, CI=TRUE)



## 2

test_a2 <- read_excel("Documents/R/tabelas/commodities_price_wb.xlsx", 
                      col_types = c("text", "numeric"),
                      sheet = "Data_Mes")

test_a2 <- test_a2[-c(1:553),]
test_a2 <- data.frame(test_a2)

alpha(test_a2) ### https://stats.stackexchange.com/questions/21515/how-to-compute-cronbachs-alpha-with-only-one-measurement-per-subject

library(reshape2)
test_a22 <- dcast(test_a2, year ~ iron_price_nom)
library(ltm)
cronbach.alpha(test_a22[,-1]) # remove SubjectID in first column

#### Coefficients

coef(lm(shares2$Price ~ shares2$High, data = shares2))

###

library("PerformanceAnalytics")

corr <- read_excel("Documents/R/tabelas/commodities_price_wb.xlsx", 
                      sheet = "Price_Quant", col_types = c("text", "numeric", "numeric"))

class(corr)
corr <- data.frame(corr)

library(jtools) 

model <- lm(corr$Preço..ton..métrica.~corr$Quantidade, data=corr)


library(GGally)


ggpairs(corr[,2:3], columnLabels = c("Preço Minério de Ferro (dólar)", "Quantidade Média Vendida (ton. métrica)")) +
  theme(strip.background = element_rect(fill = "white"))









