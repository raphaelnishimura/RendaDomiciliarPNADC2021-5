# Distribuicao de renda domiciliar em faixas de SM para pessoas com 16 anos ou mais da PNADC 2021 5a entrevista

# 1. carrea pacotes ----
library(PNADcIBGE)
library(tidyverse)
library(survey)
library(srvyr)


# 2. opcoes do pacote survey ----
options(survey.lonely.psu = "adjust")


# 3. baixa dados da PNADC 2021 - 5a entrevista ----
pnadc_2021.5 <- get_pnadc(year = 2021, interview = 5, vars = c("Estrato","UPA","V1032","V2009","VD5010"), 
                          defyear = 2021, design = FALSE)

# 4. calcula distribuicao de renda domiciliar por faixas de SM para pessoas com 16 anos ou mais ---- 
pnadc_2021.5 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 2021 = R$1100)
    renda < (1100 * 2) ~ 1, 
    renda >= (1100 * 2) & renda < (1100 * 5) ~ 2,
    renda >= (1100 * 5) ~ 3),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3), labels = c("Ate 2 SM", "De 2 ate 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  # subset(UF == "São Paulo") %>%  # filtra UF (tirar do comentário se quiser obter estimativas para uma UF em específico)
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))