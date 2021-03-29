pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)
pacman::p_load(devtools, statsr,goftest, nortest, dgof,EnvStats)
install.packages('DiceDesign')
library(DiceDesign)
library(utils)

## CRIANDO O DATA FRAME----

set.seed(77)
media <- sample(c(100:200),1)
dp <- media*0.1
matriz <- replicate(n = 1000,
                    expr = rnorm(n = 15, mean = media, sd = dp))
matriz <- data.frame(matriz)


### USANDO KOLMOGOROV----

estatisticas <- vector()

for (i in 1:1000) {
  estatisticas[i] <- gofTest(matriz[,i],test = "ks",distribution = "norm",param.list = list(mean=media,sd=dp))[[6]]
}

estatisticas <- unlist(estatisticas)
estatisticas
max(estatisticas)
min(estatisticas)
summary(estatisticas)


estatisticas_1 <- data.frame(id = 1:1000,estatisticas)

estatisticas_2 <- cut(estatisticas, 
                      breaks = seq(0.08, 0.48, 0.04),
                      labels = c('0.08 a 0.12',
                                 '0.12 a 0.16',
                                 '0.16 a 0.20',
                                 '0.20 a 0.24',
                                 '0.24 a 0.28', 
                                 '0.28 a 0.32',
                                 '0.32 a 0.36',
                                 '0.36 a 0.40',
                                 '0.40 a 0.44',
                                 '0.44 a 0.48'))

estatisticas_2 <- data.frame(estatisticas_2)

estatisticas_2 <- estatisticas_2 %>% group_by(estatisticas_2) %>%
  summarise(freq = n()) %>% 
  mutate(freq_relativa = round((freq/sum(freq)),4)) %>% 
  mutate(freq_acumulada = cumsum(freq_relativa))

estatisticas_2

ggplot(estatisticas_1, aes(x=estatisticas)) + 
  geom_histogram(colour="white", fill="#00688B",breaks = seq(0.08, 0.48, 0.04))+
  labs(x="Estimativas da estatística do Teste de Kolmogorov-Smirnov", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("kol1000.png", width = 158, height = 93, units = "mm")

quantile(estatisticas_1$estatisticas,0.8)
quantile(estatisticas_1$estatisticas,0.85)
quantile(estatisticas_1$estatisticas,0.9)
quantile(estatisticas_1$estatisticas,0.95)
quantile(estatisticas_1$estatisticas,0.975)
quantile(estatisticas_1$estatisticas,0.99)

unif.test.quantile("ks",15,0.2)
unif.test.quantile("ks",15,0.15)
unif.test.quantile("ks",15,0.1)
unif.test.quantile("ks",15,0.05)
unif.test.quantile("ks",15,0.025)
unif.test.quantile("ks",15,0.01)


#### USANDO LILLIEFORS----

estatisticas_lillie <- vector()


for (i in 1:1000) {
  estatisticas_lillie[i] <- gofTest(matriz[,i],test = "lillie",distribution = "norm")[[6]]
}
estatisticas_lillie <- unlist(estatisticas_lillie)


max(estatisticas_lillie)
min(estatisticas_lillie)
summary(estatisticas_lillie)



estatisticas_lillie_1 <- data.frame(id = 1:1000,estatistica = estatisticas_lillie)

estatisticas_lillie_2 <- cut(estatisticas_lillie, 
                             breaks = seq(0.058, 0.308, 0.025),
                             labels = c('0.058 a 0.083',
                                        '0.083 a 0.108',
                                        '0.108 a 0.133',
                                        '0.133 a 0.158',
                                        '0.158 a 0.183', 
                                        '0.183 a 0.208',
                                        '0.208 a 0.233',
                                        '0.233 a 0.258',
                                        '0.258 a 0.283',
                                        '0.283 a 0.308'))



estatisticas_lillie_2 <- data.frame(estatisticas_lillie_2)

estatisticas_lillie_2 <- estatisticas_lillie_2 %>% group_by(estatisticas_lillie_2) %>%
  summarise(freq = n()) %>% 
  mutate(freq_relativa = round((freq/sum(freq)),4)) %>% 
  mutate(freq_acumulada = cumsum(freq_relativa))
estatisticas_lillie_2

ggplot(estatisticas_lillie_1, aes(x=estatistica)) + 
  geom_histogram(colour="white", fill="#00688B",breaks = seq(0.058, 0.308, 0.025))+
  labs(x="Estimativas da estatística do Teste de Kolmogorov-Smirnov(LillieFors)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("lill1000.png", width = 158, height = 93, units = "mm")

quantile(estatisticas_lillie_1$estatistica,0.8)
quantile(estatisticas_lillie_1$estatistica,0.85)
quantile(estatisticas_lillie_1$estatistica,0.9)
quantile(estatisticas_lillie_1$estatistica,0.95)
quantile(estatisticas_lillie_1$estatistica,0.975)
quantile(estatisticas_lillie_1$estatistica,0.99)



### ESCOLHENDO 5 AMOSTRAS----


amostras_5 <- matriz[ , 1:5]

gofTest(amostras_5$X1,test = "sw")
gofTest(amostras_5$X2,test = "sw")
gofTest(amostras_5$X3,test = "sw")
gofTest(amostras_5$X4,test = "sw")
gofTest(amostras_5$X5,test = "sw")

gofTest(amostras_5$X1,test = "ad")
gofTest(amostras_5$X2,test = "ad")
gofTest(amostras_5$X3,test = "ad")
gofTest(amostras_5$X4,test = "ad")
gofTest(amostras_5$X5,test = "ad")
