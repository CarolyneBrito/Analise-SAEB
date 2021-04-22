pacman::p_load(purrr,ggplot2, dplyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)
pacman::p_load(devtools, statsr,goftest, nortest, dgof,leaflet,leaflet.extras, BSDA, gridExtra,reshape2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

assim<-function(x){
  (quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(quantile(x,0.75)-quantile(x,0.25))
}
#Valores próximos de zero sugerem simetria. Valores negativos sugerem assimetria a esquerda e valores positivos sugerem assimetria a direita.

#curtose 0,263=meso <lepto >plati
curt<-function(x){ 
  c <- (quantile(x,0.75) - quantile(x,0.25))/(2*(quantile(x,0.9) - quantile(x,0.1)))
  return(c)
}

set.seed(7)
banco <- read_csv("C:/Users/DELL/Downloads/amostra_190085720.csv")
amostra30 <- banco %>% rep_sample_n(size = 30, reps = 1, replace = FALSE)
amostra100 <- banco %>% rep_sample_n(size = 100, reps = 1, replace = FALSE)

amostra30$LOCALIZACAO <- recode(amostra30$LOCALIZACAO,"1"="Urbana","2"="Rural")
amostra100$LOCALIZACAO <- recode(amostra100$LOCALIZACAO,"1"="Urbana","2"="Rural")
amostra30$DEPENDENCIA_ADM <- recode(amostra30$DEPENDENCIA_ADM,"1" = "Federal", "2" = "Estadual","3"="Municipal", "4"="Privada")
amostra100$DEPENDENCIA_ADM <- recode(amostra100$DEPENDENCIA_ADM,"1" = "Federal", "2" = "Estadual","3"="Municipal", "4"="Privada")



#1----
#Matematica pela Localizaçao----

#Amostra 30
dfM30 <- data.frame(
  "notasmatematica" = c(amostra30$NOTA_MT, amostra30$NOTA_MT),
  "localizacao" = c(amostra30$LOCALIZACAO,rep("Total", 30) )
)
dfM30$localizacao <- recode(dfM30$localizacao, "1" = "Urbana", "2" = "Rural")
ggplot(data = dfM30) +
  geom_boxplot(aes(x = reorder(localizacao, localizacao, function(x)+length(x)), y = notasmatematica, fill = localizacao))+
  scale_fill_manual(name="Localização", values=c("#A9E3C4","#082B20","#F7B736"))+
  labs(x="Localização", y="Notas de Matemática") +
  theme_bw()
ggsave("amostra30mtloc.png", width = 158, height = 93, units = "mm")

urbana30 <- amostra30[amostra30$LOCALIZACAO == "Urbana",]
rural30 <- amostra30[amostra30$LOCALIZACAO == "Rural", ]

assim(amostra30$NOTA_MT)
curt(amostra30$NOTA_MT)

gofTest(amostra30$NOTA_MT,test = "sw")
var.test(urbana30$NOTA_MT, rural30$NOTA_MT, alternative = "greater")
ks.test(urbana30$NOTA_MT, rural30$NOTA_MT, alternative = "less")
t.test(urbana30$NOTA_MT, rural30$NOTA_MT, var.equal = TRUE, alternative = "greater")
wilcox.test(urbana30$NOTA_MT, rural30$NOTA_MT, alternative = "greater")


summary(urbana30$NOTA_MT)
summary(rural30$NOTA_MT)
var(urbana30$NOTA_MT)
var(rural30$NOTA_MT)
sd(urbana30$NOTA_MT)
sd(rural30$NOTA_MT)
getmode(urbana30$NOTA_MT)
getmode(rural30$NOTA_MT)

#Amostra 100 (mat)
#Matematica pela Localizaçao
dfM100 <- data.frame(
  "notasmatematica" = c(amostra100$NOTA_MT, amostra100$NOTA_MT),
  "localizacao" = c(amostra100$LOCALIZACAO,rep("Total", 100) )
)
dfM100$localizacao <- recode(dfM100$localizacao, "1" = "Urbana", "2" = "Rural")
ggplot(data = dfM100) +
  geom_boxplot(aes(x = reorder(localizacao, localizacao, function(x)+length(x)), y = notasmatematica, fill = localizacao))+
  scale_fill_manual(name="Localização", values=c("#A9E3C4","#082B20","#F7B736"))+
  labs(x="Localização", y="Notas de Matemática")+
  theme_bw() 
ggsave("amostra100mtloc.png", width = 158, height = 93, units = "mm")


urbana100 <- amostra100[amostra100$LOCALIZACAO == "Urbana",]
rural100 <- amostra100[amostra100$LOCALIZACAO == "Rural", ]


assim(amostra100$NOTA_MT)
curt(amostra100$NOTA_MT)


gofTest(amostra100$NOTA_MT,test = "sw")
var.test(urbana100$NOTA_MT, rural100$NOTA_MT, alternative = "greater")
ks.test(urbana100$NOTA_MT, rural100$NOTA_MT, alternative = "less")
t.test(urbana100$NOTA_MT, rural100$NOTA_MT, var.equal = TRUE, alternative = "greater")
wilcox.test(urbana100$NOTA_MT, rural100$NOTA_MT, alternative = "greater")


summary(urbana100$NOTA_MT)
summary(rural100$NOTA_MT)
var(urbana100$NOTA_MT)
var(rural100$NOTA_MT)
sd(urbana100$NOTA_MT)
sd(rural100$NOTA_MT)
getmode(urbana100$NOTA_MT)
getmode(rural100$NOTA_MT)



#Portugues e Dependencia ----
#Amostra 30
dfP30 <- data.frame(
  "notaslp" = c(amostra30$NOTA_LP, amostra30$NOTA_LP),
  "dependencia" = c(amostra30$DEPENDENCIA_ADM,rep("Total", 30) )
)
dfP30$dependencia <- recode(dfP30$dependencia, "1" = "Federal", "2" = "Estadual","3"="Municipal", "4"="Privada")
ggplot(data = dfP30) +
  geom_boxplot(aes(x = reorder(dependencia, dependencia, function(x)+length(x)), y = notaslp, fill = dependencia))+
  scale_fill_manual(name="Dependência", values=c("#A9E3C4","#F7B736","#082B20"))+
  labs(x="Dependência", y="Notas de Português") +
  theme_bw()
ggsave("amostra30lpdep.png", width = 158, height = 93, units = "mm")


estadual30 <- amostra30[amostra30$DEPENDENCIA_ADM == "Estadual",]
municipal30 <- amostra30[amostra30$DEPENDENCIA_ADM == "Municipal", ]

assim(amostra30$NOTA_LP)
curt(amostra30$NOTA_LP)

gofTest(amostra30$NOTA_LP,test = "sw")
var.test(estadual30$NOTA_LP, municipal30$NOTA_LP, alternative = "greater")
ks.test(estadual30$NOTA_LP, municipal30$NOTA_LP)
t.test(estadual30$NOTA_LP, municipal30$NOTA_LP, var.equal = TRUE)
wilcox.test(estadual30$NOTA_LP, municipal30$NOTA_LP)


summary(estadual30$NOTA_MT)
summary(municipal30$NOTA_MT)
var(estadual30$NOTA_MT)
var(municipal30$NOTA_MT)
sd(estadual30$NOTA_MT)
sd(municipal30$NOTA_MT)
getmode(estadual30$NOTA_MT)
getmode(municipal30$NOTA_MT)



#Amostra 100
#Portugues e dependencia
dfP100 <- data.frame(
  "notaslp" = c(amostra100$NOTA_LP, amostra100$NOTA_LP),
  "dependencia" = c(amostra100$DEPENDENCIA_ADM,rep("Total", 100) )
)
dfP100$dependencia <- recode(dfP100$dependencia, "1" = "Federal", "2" = "Estadual","3"="Municipal", "4"="Privada")
ggplot(data = dfP100) +
  geom_boxplot(aes(x = reorder(dependencia, dependencia, function(x)+length(x)), y = notaslp, fill = dependencia))+
  scale_fill_manual(name="Dependência", values=c("#A9E3C4","#F7B736","#082B20"))+
  labs(x="Dependência", y="Notas de Língua Portuguesa")+
  theme_bw()
ggsave("amostra100lpdep.png", width = 158, height = 93, units = "mm")


estadual100 <- amostra100[amostra100$DEPENDENCIA_ADM == "Estadual",]
municipal100 <- amostra100[amostra100$DEPENDENCIA_ADM == "Municipal", ]

assim(amostra100$NOTA_LP)
curt(amostra100$NOTA_LP)

gofTest(amostra100$NOTA_LP,test = "sw")
var.test(estadual100$NOTA_LP, municipal100$NOTA_LP, alternative = "greater")
ks.test(estadual100$NOTA_LP, municipal100$NOTA_LP)
t.test(estadual100$NOTA_LP, municipal100$NOTA_LP, var.equal = TRUE)
wilcox.test(estadual100$NOTA_LP, municipal100$NOTA_LP)


summary(estadual100$NOTA_LP)
summary(municipal100$NOTA_LP)
var(estadual100$NOTA_LP)
var(municipal100$NOTA_LP)
sd(estadual100$NOTA_LP)
sd(municipal100$NOTA_LP)
getmode(estadual100$NOTA_LP)
getmode(municipal100$NOTA_LP)



#Questao 2----
SIGN.test(amostra30$NOTA_LP, amostra30$NOTA_MT) #teste dos sinais
wilcox.test(amostra30$NOTA_LP, amostra30$NOTA_MT, paired=TRUE) #teste de postos com sinais de wilcoxon
t.test(amostra30$NOTA_LP, amostra30$NOTA_MT, paired=TRUE) #teste t para amostras pareadas

datalp <- amostra30 %>%
  select(NOTA_LP) %>%
  mutate(materia="Português")
colnames(datalp)[2] <- "nota"

datamt <- amostra30 %>%
  select(NOTA_MT) %>%
  mutate(materia="Matemática")
colnames(datamt)[2] <- "nota"

box <- rbind(datamt,datalp)

ggplot(box, aes(x=materia, y=nota)) +
  geom_boxplot(fill=c("#A9E3C4","#F7B736"), width = 0.5) +
  labs(x="Matéria", y="Notas") +
  stat_summary(fun=mean, geom="point", shape=18, size=2) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ggsave("mtlp.png", width = 158, height = 93, units = "mm")

