pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)
pacman::p_load(devtools, statsr,goftest, nortest, dgof,EnvStats)
pacman::p_load(dplyr, tidyverse,PMCMR, goftest, DescTools, ggplot2, tidyr)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


banco <- read_csv("C:/Users/DELL/Downloads/amostra_190085720.csv")
banco <- na.omit(banco)

set.seed(2)
amostra <- banco %>% rep_sample_n(size = 500, reps = 1, replace = FALSE)

View(amostra)

amostra$USO_TEMPO_TELAS <- recode(amostra$USO_TEMPO_TELAS,
                                  "A" = "Menos de 1 hora",
                                  "B" = "Entre 1 e 2 horas",
                                  "C" = "Entre 2 e 3 horas",
                                  "D" = "Mais de 3 horas",
                                  "E" = "Menos de 1 hora")
amostra$REGIAO <- recode(amostra$REGIAO,
                         "1" = "Norte",
                         "2" = "Nordeste",
                         "3" = "Sudeste",
                         "4" = "Sul",
                         "5" = "Centro-Oeste")



#Nota mat e regiao -----

#JÁ FOI TESTADA A NORMALIDADE EM OUTRA ATIVIDADE (ASSUME NORMALIDADE)
#Box-plot confirma normalidade
ggplot(amostra, aes(x=factor(""), y=NOTA_MT)) +
  geom_boxplot(fill=c("#00688B"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=18, size=2)+
  labs(x="", y="Notas de Matemática")+
  theme_bw()
ggsave("mattotal.png", width = 158, height = 93, units = "mm")

#TESTANDO A VARIÂNCIA (BARTLETT)
bartlett.test(amostra$NOTA_MT ~ amostra$REGIAO) #variâncias iguais

#Medidas de posição

norte <- amostra[amostra$REGIAO == "Norte",]
nordeste <- amostra[amostra$REGIAO == "Nordeste",]
sudeste <- amostra[amostra$REGIAO == "Sudeste",]
sul <- amostra[amostra$REGIAO == "Sul",]
centro <- amostra[amostra$REGIAO == "Centro-Oeste",]


summary(norte$NOTA_MT)
summary(nordeste$NOTA_MT)
summary(sudeste$NOTA_MT)
summary(sul$NOTA_MT)
summary(centro$NOTA_MT)

sd(norte$NOTA_MT)
sd(nordeste$NOTA_MT)
sd(sudeste$NOTA_MT)
sd(sul$NOTA_MT)
sd(centro$NOTA_MT)

getmode(norte$NOTA_MT)
getmode(nordeste$NOTA_MT)
getmode(sudeste$NOTA_MT)
getmode(sul$NOTA_MT)
getmode(centro$NOTA_MT)



#Comparando medias
anova_mat <- aov(amostra$NOTA_MT ~ amostra$REGIAO)
summary(anova_mat) #Com nível de significância de 5%, rejeita H0.

#Teste de Comparação Multipla de Médias
pairwise.t.test(amostra$NOTA_MT, amostra$REGIAO, p.adjust.method = "bonferroni" )

#Teste de Kruskall-Wallis
kruskal.test(amostra$NOTA_MT ,amostra$REGIAO)

#Teste de Comparação Múltipla de Médias
pairwise.wilcox.test(amostra$NOTA_MT ,amostra$REGIAO, p.adjust.method = "bonferroni")

titulos <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
ggplot(amostra, aes(x=factor(REGIAO, levels = titulos), y=NOTA_MT)) +
  geom_boxplot(fill=c("#00688B"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=18, size=2)+
  labs(x="Método", y="Nota") +
  theme_bw()
ggsave("matregioes.png", width = 158, height = 93, units = "mm")





#Nota pt e uso de tela ----

#JÁ FOI TESTADA A NORMALIDADE EM OUTRA ATIVIDADE (ASSUME NORMALIDADE)
#Box-plot confirma normalidade

ggplot(amostra, aes(x=factor(""), y=NOTA_LP)) +
  geom_boxplot(fill=c("#00688B"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=18, size=2)+
  labs(x="", y="Notas")+
  theme_bw()
ggsave("lttotal.png", width = 158, height = 93, units = "mm")


#TESTANDO A VARIÂNCIA (BARTLETT)
bartlett.test(amostra$NOTA_LP ~ amostra$USO_TEMPO_TELAS) #variâncias iguais

#Medidas de posição

menos1 <- amostra[amostra$USO_TEMPO_TELAS == "Menos de 1 hora",]
entre12 <- amostra[amostra$USO_TEMPO_TELAS == "Entre 1 e 2 horas",]
entre23 <- amostra[amostra$USO_TEMPO_TELAS == "Entre 2 e 3 horas",]
mais3 <- amostra[amostra$USO_TEMPO_TELAS == "Mais de 3 horas",]


summary(menos1$NOTA_LP)
summary(entre12$NOTA_LP)
summary(entre23$NOTA_LP)
summary(mais3$NOTA_LP)

sd(menos1$NOTA_LP)
sd(entre12$NOTA_LP)
sd(entre23$NOTA_LP)
sd(mais3$NOTA_LP)

getmode(menos1$NOTA_LP)
getmode(entre12$NOTA_LP)
getmode(entre23$NOTA_LP)
getmode(mais3$NOTA_LP)



#Comparando medias
anova_lp <- aov(amostra$NOTA_LP ~ amostra$USO_TEMPO_TELAS)
summary(anova_lp) #Com nível de significância de 5%, aceita H0.
#Logo, afirma-se que não existe diferença.

#Teste de Comparação Multipla de Médias
pairwise.t.test(amostra$NOTA_LP, amostra$USO_TEMPO_TELAS, p.adjust.method = "bonferroni" )

#Teste de Kruskall-Wallis
kruskal.test(amostra$NOTA_LP ,amostra$USO_TEMPO_TELAS)

#Teste de Comparação Múltipla de Médias
pairwise.wilcox.test(amostra$NOTA_LP ,amostra$USO_TEMPO_TELAS, p.adjust.method = "bonferroni")

ordem <- c("Menos de 1 hora", "Entre 1 e 2 horas", "Entre 2 e 3 horas", "Mais de 3 horas")
ggplot(amostra, aes(x=factor(USO_TEMPO_TELAS, levels = ordem), y=NOTA_LP)) +
  geom_boxplot(fill=c("#00688B"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=18, size=2)+
  labs(x="Método", y="Nota") +
  theme_bw()
ggsave("lpuso.png", width = 158, height = 93, units = "mm")