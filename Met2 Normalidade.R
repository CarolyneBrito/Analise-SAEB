pacman::p_load(purrr, plyr, tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer, xtable, readxl,lubridate, janitor, e1071, moments)
pacman::p_load(devtools, statsr,goftest, nortest)
pacman::p_load(goftest,EnvStats,base)

banco <- read_csv("C:/Users/DELL/Downloads/amostra_190085720.csv")

set.seed(1000)
amostra30 <- banco %>% rep_sample_n(size = 30, reps = 1, replace = FALSE) #AMOSTRA TAMANHO 30
amostra100 <- banco %>% rep_sample_n(size = 100, reps = 1, replace = FALSE) #AMOSTRA TAMANHO 100

# Questão 1----
# NORMALIDADE NOTAS DE PORTUGUÊS  n = 100

max(amostra100$NOTA_LP)
min(amostra100$NOTA_LP)
tab_int <- function(vet, vec){
  tab <- cut(vet, breaks = vec) %>%
    table () %>% as.data.frame()
  return(tab)
}
tabelalp100 <- tab_int(amostra100$NOTA_LP,c(0,125,150,175,200,225,250,275,300,325,350))
tabelalp100
sum(tabelalp100$Freq)
summary(amostra100$NOTA_LP)

amostra100 %>% ggplot(aes(x=NOTA_LP)) + 
  geom_histogram(breaks = c(100,125,150,175,200,225,250,275,300,325,350), 
                 aes(col=I("white"), y = ..density..), 
                 fill = "#00688B") + 
  scale_x_continuous(breaks = c(100,125,150,175,200,225,250,275,300,325,350)) +
  labs(x='Notas de Português', 
       y="Densidade") + 
  theme_classic() + 
  theme(text = element_text(family = 'serif', size = 12))+ 
  stat_function(fun=dnorm,
                color="#464e51",
                size=0.4,
                args=list(mean=mean(banco$NOTA_LP), 
                          sd=sd(banco$NOTA_LP)))
ggsave("amostra100lp.png", width = 158, height = 93, units = "mm")

gofTest(amostra100$NOTA_LP, test = "chisq", distribution = "norm", estimate.params = T,cut.points = c(0,150,175,200,225,250,275,350))

# Questão 2----
# Notas de Português

max(amostra30$NOTA_LP)
min(amostra30$NOTA_LP)
tabelalp30 <- tab_int(amostra30$NOTA_LP,c(0,125,150,175,200,225,250,275,300,325,350))
tabelalp30
sum(tabelalp30$Freq)
summary(amostra30$NOTA_LP)


gofTest(amostra30$NOTA_LP,test = "sw")
shapiro.test(amostra30$NOTA_LP)
gofTest(amostra30$NOTA_LP,test = "lillie",distribution = "norm")
lillie.test(amostra30$NOTA_LP)
gofTest(amostra30$NOTA_LP,test = "ad")
ad.test(amostra30$NOTA_LP)


# Notas de Matemática

max(amostra30$NOTA_MT)
min(amostra30$NOTA_MT)
tabelamt30 <- tab_int(amostra30$NOTA_MT,c(0,125,150,175,200,225,250,275,300,325,350))
tabelamt30
sum(tabelamt30$Freq)
summary(amostra30$NOTA_MT)

gofTest(amostra30$NOTA_MT,test = "sw")
shapiro.test(amostra30$NOTA_MT)
gofTest(amostra30$NOTA_MT,test = "lillie",distribution = "norm")
lillie.test(amostra30$NOTA_MT)
gofTest(amostra30$NOTA_MT,test = "ad")
ad.test(amostra30$NOTA_MT)

