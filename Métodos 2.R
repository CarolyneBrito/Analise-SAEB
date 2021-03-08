amostra <- read_csv("C:/Users/DELL/Downloads/amostra_190085720.csv")

library(tidyverse)
library(ggplot2)
library(plyr)
library(scales)

amostra[is.na(amostra)] <- "Sem dados"
View(amostra)

#Variáveis Qualitativas

#Sexo 
amostra$SEXO <- recode(amostra$SEXO, "A" = "Masculino","B"="Feminino")

contagem <- amostra %>%
  group_by(SEXO) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(SEXO)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)


ggplot(contagem, aes(x = factor(""), y = Prop , fill = factor(SEXO))) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar(theta = "y")+
  scale_x_discrete()+
  scale_fill_manual(name = "Sexo", values = c("#FF3030","#1874CD", "#FF6600")) +
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(legend.position = "right") +
  geom_text(aes(x = 1, y = posicao, label = paste0(Prop, "%")), color = "black",size=6.50001)
ggsave("sexo.png", width = 158, height = 93, units = "mm")

table(amostra$SEXO)

#Computador
amostra$COMPUTADOR <- recode(amostra$COMPUTADOR,"A"="Não tem","B"="Um","C"="Dois","D"="Três","E"="Quatro ou mais")
d <- amostra %>%
  mutate(nome=fct_relevel(COMPUTADOR,"Não tem","Um","Dois","Três","Quatro ou mais","Sem dados"))

ggplot(d,aes(x = nome)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#00688B", width=0.9) + 
  geom_text(aes(y = prop.table(..count..) * 100 + 3.5, 
                label = paste0(gsub("\\.",",",round(prop.table(..count..) * 100,2)), '%')), 
            stat = 'count', vjust=0, size = 4) +
  labs(x="Posse de computadores", y="%") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("comp.png", width = 158, height = 93, units = "mm")
table(amostra$COMPUTADOR)

#Local
amostra$LOCALIZACAO <- recode(amostra$LOCALIZACAO,"1"="Urbana","2"="Rural")
ggplot(amostra, aes(x = LOCALIZACAO)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#00688B", width=0.6) + 
  geom_text(aes(y = prop.table(..count..) * 100 + 3.5, 
                label = paste0(gsub("\\.",",",round(prop.table(..count..) * 100,2)), '%')), 
            stat = 'count', vjust=0, size = 4) +
  labs(x="Área", y="%") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("loc.png", width = 158, height = 93, units = "mm")
table(amostra$LOCALIZACAO)

#Raça/Cor
amostra$RACA_COR <- recode(amostra$RACA_COR,"A"="Branca","B"="Preta", "C"="Parda", "D"="Amarela", "E"="Indígena","F"="Não quero declarar")
ggplot(amostra, aes(x = reorder(RACA_COR, RACA_COR, function(x)-length(x)))) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#00688B", width=0.9) + 
  geom_text(aes(y = prop.table(..count..) * 100 + 3, 
                label = paste0(gsub("\\.",",",round(prop.table(..count..) * 100,2)), '%')), 
            stat = 'count', vjust=0, size = 4) +
  labs(x="Raça/Cor", y="%") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("raca.png", width = 158, height = 93, units = "mm")
table(amostra$RACA_COR)

#Reunioes escolares
amostra$REUNIÕES_ESCOLARES <- recode(amostra$REUNIÕES_ESCOLARES,"A"="Sempre ou quase sempre","B"="De vez em quando", "C"="Nunca ou quase nunca")
ggplot(amostra, aes(x = reorder(REUNIÕES_ESCOLARES, REUNIÕES_ESCOLARES, function(x)-length(x)))) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#00688B", width=0.6) + 
  geom_text(aes(y = prop.table(..count..) * 100 + 3.5, 
                label = paste0(gsub("\\.",",",round(prop.table(..count..) * 100,2)), '%')), 
            stat = 'count', vjust=0, size = 4) +
  labs(x="Frequência à reunião de pais", y="%") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.1),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ggsave("reuniao.png", width = 158, height = 93, units = "mm")
table(amostra$REUNIÕES_ESCOLARES)


#Variáveis Quantitativas 

#Notas de portugues
ggplot(amostra, aes(x=NOTA_LP)) + geom_histogram(aes(y=..density..),colour="white", fill="#00688B", binwidth = 17.5)+
  labs(x="Notas de Português", y="") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  stat_function(fun = dnorm, args = list(mean=mean(amostra$NOTA_LP),sd=sd(amostra$NOTA_LP)),lwd=1)

ggsave("lp.png", width = 158, height = 93, units = "mm")


ndf <- select(amostra, NOTA_LP) %>%
  mutate(LP=cut(NOTA_LP,breaks = c(0,125,150,175,200,225,250,275,300,325,350)))
table(ndf$LP)

stem(amostra$NOTA_LP)

summary(amostra$NOTA_LP)
sd(amostra$NOTA_LP)

ggplot(amostra, aes(x=factor(""), y=NOTA_LP)) +
  geom_boxplot(fill=c("#00688B"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Notas em Língua Portuguesa")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("boxlp.png", width = 158, height = 93, units = "mm")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(amostra$NOTA_LP)

g1<-function(x){  ## coeficiente de assimetria
  n<-length(x)
  s<-sd(x)
  m<-mean(x)
  n/((n-1)*(n-2))*sum((x-m)^3)/s^3
}
g1(amostra$NOTA_LP)
#Valores próximos de zero sugerem simetria. Valores negativos sugerem assimetria a esquerda e valores positivos sugerem assimetria a direita.

#curtose
cut<-function(x){ 
  c <- (quantile(x,0.75) - quantile(x,0.25))/(2*(quantile(x,0.9) - quantile(x,0.1)))
  return(c)
}
cut(amostra$NOTA_LP)

#Notas de Matemática
ggplot(amostra, aes(x=NOTA_MT)) + geom_histogram(aes(y=..density..),colour="white", fill="#00688B", binwidth = 17.5)+
  labs(x="Notas de Matemática", y="") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=8.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  stat_function(fun = dnorm, args = list(mean=mean(amostra$NOTA_LP),sd=sd(amostra$NOTA_LP)),lwd=1)
ggsave("mat.png", width = 158, height = 93, units = "mm")

stem(amostra$NOTA_MT)
ndf <- select(amostra, NOTA_MT) %>%
  mutate(MT=cut(NOTA_MT,breaks = c(0,125,150,175,200,225,250,275,300,325,350)))
table(ndf$MT)

summary(amostra$NOTA_MT)
sd(amostra$NOTA_MT)
(sd(amostra$NOTA_MT))^2

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(amostra$NOTA_MT)


ggplot(amostra, aes(x=factor(""), y=NOTA_MT)) +
  geom_boxplot(fill=c("#00688B"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Notas em Matemática")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("boxmat.png", width = 158, height = 93, units = "mm")
stem(amostra$NOTA_MT)

g1<-function(x){  ## coeficiente de assimetria
  n<-length(x)
  s<-sd(x)
  m<-mean(x)
  n/((n-1)*(n-2))*sum((x-m)^3)/s^3
}
g1(amostra$NOTA_MT)
#Valores próximos de zero sugerem simetria. Valores negativos sugerem assimetria a esquerda e valores positivos sugerem assimetria a direita.

#curtose
cut<-function(x){ 
  c <- (quantile(x,0.75) - quantile(x,0.25))/(2*(quantile(x,0.9) - quantile(x,0.1)))
  return(c)
}
cut(amostra$NOTA_MT)


