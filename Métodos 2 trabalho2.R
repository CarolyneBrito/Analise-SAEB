pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)
pacman::p_load(devtools, statsr)


banco <- read_csv("C:/Users/DELL/Downloads/amostra_190085720.csv")
banco$SEXO <- recode(banco$SEXO, "A" = "Masculino","B"="Feminino")
banco$AREA <- recode(banco$AREA, "1" = "Capital","2"="Interior")


z95 <- qnorm(0.975)
medialp <- mean(banco$NOTA_LP)
mediamt <- mean(banco$NOTA_MT)
propareainterior <- (nrow(banco[banco$AREA == "Interior", ])/2000)
propsexofeminino <- (nrow(banco[banco$SEXO == "Feminino", ])/2000)



#L�NGUA PORTUGUESA
#INTERVALOS PARA A MEDIA DE LP (AMOSTRAS TAMANHO 30)

amostras30 <- banco %>% rep_sample_n(size = 30, reps = 50, replace = FALSE) #AMOSTRAS TAMANHO 30
amostras100 <- banco %>% rep_sample_n(size = 100, reps = 50, replace = TRUE) #AMOSTRAS TAMANHO 100

notaslpamostra30 <- amostras30 %>% summarise(lower = mean(NOTA_LP) - z95 * (sd(NOTA_LP) / sqrt(30)), 
                                               upper = mean(NOTA_LP) + z95 * (sd(NOTA_LP) / sqrt(30)))

notaslpamostra30 <- notaslpamostra30  %>%
  mutate(legenda = ifelse(lower < medialp & upper > medialp, "Cont�m", "N�o Cont�m"))

bancolp30 <- data.frame(lpid = c(1:50, 1:50),
                         lpintervalo = c(notaslpamostra30$lower, notaslpamostra30$upper),
                         legenda = c(notaslpamostra30$legenda, notaslpamostra30$legenda))

ggplot(data = bancolp30, aes(x = lpintervalo, y = lpid, 
                              group = lpid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) + 
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras L�ngua Portuguesa") +
  geom_vline(xintercept = medialp, color = "#000000") + 
  annotate(x = medialp,y=-3, label = expression(mu == 253.88) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("iclp30.png", width = 158, height = 93, units = "mm")


# INTERVALOS PARA A M�DIA DE NOTA DE PORTUGU�S (AMOSTRAS TAMANHO 100)

notaslpamostra100 <- amostras100 %>% summarise(lower = mean(NOTA_LP) - z95 * (sd(NOTA_LP) / sqrt(30)), 
                                               upper = mean(NOTA_LP) + z95 * (sd(NOTA_LP) / sqrt(30)))


notaslpamostra100 <- notaslpamostra100  %>%
  mutate(legenda = ifelse(lower < medialp & upper > medialp, "Cont�m", "N�o Cont�m"))

bancolp100 <- data.frame(lpid = c(1:50, 1:50),
                         lpintervalo = c(notaslpamostra100$lower, notaslpamostra100$upper),
                         legenda = c(notaslpamostra100$legenda, notaslpamostra100$legenda))

ggplot(data = bancolp100, aes(x = lpintervalo, y = lpid, 
                              group = lpid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) + 
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras L�ngua Portuguesa") +
  geom_vline(xintercept = medialp, color = "#000000") + 
  annotate(x = medialp,y=-3, label = expression(mu == 253.88) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icpt100.png", width = 158, height = 93, units = "mm")


#MATEM�TICA
#INTERVALOS PARA MEDIA DE MT (AMOSTRAS TAMNHO 30)

notasmtamostra30 <- amostras30 %>% summarise(lower = mean(NOTA_MT) - z95 * (sd(NOTA_MT) / sqrt(30)), 
                                               upper = mean(NOTA_MT) + z95 * (sd(NOTA_MT) / sqrt(30)))

notasmtamostra30 <- notasmtamostra30  %>%
  mutate(legenda = ifelse(lower < mediamt & upper > mediamt, "Cont�m", "N�o Cont�m"))

bancomt30 <- data.frame(mtid = c(1:50, 1:50),
                         mtintervalo = c(notasmtamostra30$lower, notasmtamostra30$upper),
                         legenda = c(notasmtamostra30$legenda, notasmtamostra30$legenda))

ggplot(data = bancomt30, aes(x = mtintervalo, y = mtid, 
                              group = mtid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) + 
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras Matem�tica") +
  geom_vline(xintercept = mediamt, color = "#000000") + 
  annotate(x = mediamt,y=-3, label = expression(mu == 252.342) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icmt30.png", width = 158, height = 93, units = "mm")



# INTERVALOS PARA MT (AMOSTRAS TAMANHO 100)

notasmtamostra100 <- amostras100 %>% summarise(lower = mean(NOTA_MT) - z95 * (sd(NOTA_MT) / sqrt(30)), 
                                                 upper = mean(NOTA_MT) + z95 * (sd(NOTA_MT) / sqrt(30)))

notasmtamostra100 <- notasmtamostra100  %>%
  mutate(legenda = ifelse(lower < mediamt & upper > mediamt, "Cont�m", "N�o Cont�m"))

bancomt100 <- data.frame(mtid = c(1:50, 1:50),
                          mtintervalo = c(notasmtamostra100$lower, notasmtamostra100$upper),
                          legenda = c(notasmtamostra100$legenda, notasmtamostra100$legenda))

ggplot(data = bancomt100, aes(x = mtintervalo, y = mtid, 
                               group = mtid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) + 
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras Matem�tica") +
  geom_vline(xintercept = mediamt, color = "#000000") + 
  annotate(x = mediamt,y=-3, label = expression(mu == 252.342) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icmt100.png", width = 158, height = 93, units = "mm")



#AREA
#INTERVALOS PARA PROPOR�AO �REA INTERIOR (AMOSTRA TAMANHO 30)

amostras30n <- amostras30[amostras30$AREA == "Interior", ]

propinteamostra30 <- amostras30n %>% summarise(lower = (length(replicate)/30) - z95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)),
                                                  upper = (length(replicate)/30) + z95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)))

propinteamostra30 <- propinteamostra30  %>%
  mutate(legenda = ifelse(lower < propareainterior & upper > propareainterior, "Cont�m", "N�o Cont�m"))

bancointerior30 <- data.frame(intid = c(1:50, 1:50),
                               intintervalo = c(propinteamostra30$lower, propinteamostra30$upper),
                               legenda = c(propinteamostra30$legenda, propinteamostra30$legenda))

ggplot(data = bancointerior30, aes(x = intintervalo, y = intid, 
                                    group = intid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) +
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras �rea Interior") +
  geom_vline(xintercept = propareainterior, color = "black") + 
  annotate(x = propareainterior,y=-3, label = expression(mu == 0.829) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icarea30.png", width = 158, height = 93, units = "mm")


#INTERVALOS PARA PROPOR��O AREA INTERIOR (AMOSTRAS TAMANHO 100)

amostras100n <- amostras100[amostras100$AREA == "Interior", ]

propinteamostra100 <- amostras100n %>% summarise(lower = (length(replicate)/100) - z95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)),
                                                 upper = (length(replicate)/100) + z95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)))

propinteamostra100 <- propinteamostra100  %>%
  mutate(legenda = ifelse(lower < propareainterior & upper > propareainterior, "Cont�m", "N�o Cont�m"))

bancointerior100 <- data.frame(intid = c(1:50, 1:50),
                               intintervalo = c(propinteamostra100$lower, propinteamostra100$upper),
                               legenda = c(propinteamostra100$legenda, propinteamostra100$legenda))

ggplot(data = bancointerior100, aes(x = intintervalo, y = intid, 
                                    group = intid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) +
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras �rea Interior") +
  geom_vline(xintercept = propareainterior, color = "black") + 
  annotate(x = propareainterior,y=-3, label = expression(mu == 0.829) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icarea100.png", width = 158, height = 93, units = "mm")



#SEXO
#INTERVALO DE PROPOR��O PARA SEXO FEMININO (AMOSTRAS TAMANHO 30)

amostras30f <- amostras30[amostras30$SEXO == "Feminino", c(1, 11) ]

amostras30f <- na.omit(amostras30f)

propsexoamostra30 <- amostras30f %>% summarise(lower = (length(replicate)/30) - z95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)),
                                                  upper = (length(replicate)/30) + z95 * sqrt(((length(replicate)/30)*(1 - (length(replicate)/30))/30)))

propsexoamostra30 <- propsexoamostra30  %>%
  mutate(legenda = ifelse(lower < propsexofeminino & upper > propsexofeminino, "Cont�m", "N�o Cont�m"))

bancosexo30 <- data.frame(sexoid = c(1:50, 1:50),
                           sexointervalo = c(propsexoamostra30$lower, propsexoamostra30$upper),
                           legenda = c(propsexoamostra30$legenda, propsexoamostra30$legenda))

ggplot(data = bancosexo30, aes(x = sexointervalo, y = sexoid, 
                                group = sexoid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) +
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras Sexo Feminino") +
  geom_vline(xintercept = propsexofeminino, color = "black") + 
  annotate(x = propsexofeminino,y=-3, label = expression(mu == 0.525) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icsexo30.png", width = 158, height = 93, units = "mm")


#INTERVALOS PARA PROPOR��O SEXO FEMININO (AMOSTRAS TAMANHO 100)

amostras100f <- amostras100[amostras100$SEXO == "Feminino", c(1, 11) ]

amostras100f <- na.omit(amostras100f)

propsexoamostra100 <- amostras100f %>% summarise(lower = (length(replicate)/100) - z95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)),
                                                    upper = (length(replicate)/100) + z95 * sqrt(((length(replicate)/100)*(1 - (length(replicate)/100))/100)))

propsexoamostra100 <- propsexoamostra100  %>%
  mutate(legenda = ifelse(lower < propsexofeminino & upper > propsexofeminino, "Cont�m", "N�o Cont�m"))

bancosexo100 <- data.frame(sexoid = c(1:50, 1:50),
                            sexointervalo = c(propsexoamostra100$lower, propsexoamostra100$upper),
                            legenda = c(propsexoamostra100$legenda, propsexoamostra100$legenda))

ggplot(data = bancosexo100, aes(x = sexointervalo, y = sexoid, 
                                 group = sexoid, color = legenda)) +
  geom_point(size = 1.6) +  
  geom_line(size = 0.7) +
  scale_colour_manual(name="O intervalo cont�m o par�metro?", values = c("#1874CD", "#EE6363"))+
  labs(x="Intervalos de Confian�a", y="Amostras Sexo Feminino") +
  geom_vline(xintercept = propsexofeminino, color = "black") + 
  annotate(x = propsexofeminino,y=-3, label = expression(mu == 0.525) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
ggsave("icsexo100.png", width = 158, height = 93, units = "mm")

