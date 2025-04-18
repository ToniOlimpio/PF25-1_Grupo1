source("rdocs/source/packages.R")
install.packages("tidyverse")
install.packages("psych")
library(tidyverse)
library(readr)
library(psych)
inflacao <- read_csv("inflacao.csv")
inflacao_analises <- inflacao[inflacao$ano > 2001,]
inflacao_analises <- inflacao_analises[inflacao_analises$ano < 2023,]

################
## Analise 1 ##
##############

analise_1 <- data.frame(inflacao_analises[,2], inflacao_analises[,7], inflacao_analises[,22], inflacao_analises[,24])
analise_1[,3] <- format(analise_1[,3], nsmall = 2)
analise_1[,3] <- as.numeric(analise_1[,3])
unlist(analise_1)
names(analise_1)[1] <- "ano" 
names(analise_1)[2] <- "ipca_acumulado_doze_meses" 
names(analise_1)[3] <- "selic_meta" 
names(analise_1)[4] <- "juros_reais"


#####
ggplot(analise_1) +
  geom_line(aes(x = ano, y = ipca_acumulado_doze_meses, color = "IPCA (Doze Meses)"),linewidth = 1) +
  geom_point(aes(x = ano, y = ipca_acumulado_doze_meses), size = 1) +
  geom_line(aes(x = ano, y = selic_meta, color = "Taxa SELIC"),linewidth = 1) +
  geom_point(aes(x = ano, y = selic_meta), size = 1)+
  geom_line(aes(x = ano, y = juros_reais, color = "Juros"), linewidth = 1) +
  geom_point(aes(x = ano, y = juros_reais), size = 1,)+
  labs(x = "Ano", y = "Valor") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")


################
## Análise 2 ##
##############

analise_2 <- data.frame(inflacao_analises[,7], inflacao_analises[,22])
describe(analise_2$ipca_acumulado_doze_meses)
describe(analise_2$selic_meta)
correlacao <- cor(analise_2$ipca_acumulado_doze_meses, analise_2$selic_meta, method = "pearson")


#####

marmota <- ggplot(analise_2) +
  aes(x = selic_meta, y = ipca_acumulado_doze_meses) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Taxa Selic",
    y = "Inflação em 12 meses"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")