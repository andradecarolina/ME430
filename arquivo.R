library(caret)
library(tidyverse)
library(ggplot2)
library(pillar)
library(ggcorrplot)
library(knitr)
library(kableExtra)

set.seed(160717)
options(pillar.sigfig = 5)


# leitura de dados --------------------------------------------------------
#rm(list = ls())

## questionario
Quest <- read.csv2('quest.txt',stringsAsFactors = FALSE) %>% as.tibble()
#### modificar tipos das duas primeiras colunas e consertar '' para 'N' (não)
#### respostas 0 como NA
Quest <- Quest %>% mutate(empct = as.integer(empct)) %>%
  mutate(APROVA1 = ifelse(APROVA1!='',APROVA1,'N')) %>%
  mutate(APROVA1 = factor(APROVA1, levels = c('S','N'))) %>%
  mutate_at(vars(starts_with('Q')),funs(ifelse(.==0,NA,.))) %>%
  rename(EMPCT = empct) %>%
  arrange(EMPCT)

## opções
Opc <- read.csv2('Opcoes.csv',stringsAsFactors = FALSE) %>% as.tibble()
#### tem 5199 NAs em opc2 que correspondem a '' em opc2d
#### transformar esses '' em NA também
Opc <- Opc %>% mutate(opc2d = ifelse(opc2d!='',opc2d,NA)) %>%
  arrange(EMPCT)

## convocados-matriculados
Conv <- read.csv2('ConvocadosMatriculados.csv',stringsAsFactors = FALSE) %>% as.tibble()
#### transformar a coluna matriculado para 'S' se sim e 'N' c.c.; factor
Conv <- Conv %>% mutate(MATRICULADO = ifelse(MATRICULADO!='',MATRICULADO,'N')) %>%
  mutate(MATRICULADO = factor(MATRICULADO,levels = c('S','N'))) %>%
  arrange(EMPCT)

## respostas fase1
Fase1 <- read.csv2('Fase1TipoQ.csv',stringsAsFactors = FALSE) %>% as.tibble() %>%
  arrange(EMPCT)

# dados extra -------------------------------------------------------------

## ver se os numeros em EMPCT são únicos --- resultado tem q ser zero
#### !!! em Conv existem 716 EMPCTs que estão duplicados
#### !!! inconsistência está na coluna MATRICULADO
Conv %>% add_count(EMPCT) %>% filter(n==2) %>% tally
#### criar Conv corrigido com informaçao de CONVOCADO para todos os EMPCTs
#### CONVOCADO = 'S' se foi convocado e 'N' caso contrário (engloba os faltantes tb!)
corrConv <- Conv %>% select(-MATRICULADO) %>% distinct() %>%
  right_join(select(Fase1,EMPCT), by = 'EMPCT') %>%
  mutate(CONVOCADO = ifelse(is.na(CONVOCADO),'N',CONVOCADO)) %>%
  mutate(CONVOCADO = factor(CONVOCADO, levels = c('S','N')))

## criar dadosgeral que comportem o questionário, opções de curso, informações da
## fase 1 (sem as questões), e a convocação.
dadosgeral <- Fase1 %>% select(-starts_with('Q'),-starts_with('R')) %>%
  left_join(corrConv, by = 'EMPCT') %>%
  right_join(Opc, by = 'EMPCT') %>%
  left_join(Quest, by = 'EMPCT')

## criar dadosP contendo apenas os presentes na fase1
dadosP <- dadosgeral %>% filter(SIT2=='P')

# expl (helena) -----------------------------------------------------------


## qtde de dados faltantes (NA) no banco de dados
colSums(is.na(dadosgeral))
colSums(is.na(dadosP))

## vendo a quandidade de entradas faltantes (respostas 0) nas perguntas do questionario
Quest %>% sapply(., table)

## cálculo da correlação entre as perguntas do questionário e a nota da fase1
## desconsiderar questões 8 a 10 <são optativas pelo questionário>
dadosP_filter <- dadosP %>% select(-TPROVA2, -SIT2, -CONVOCADO, -starts_with('o'))
dadosP_filter <- dadosP_filter %>%  filter_at(vars(starts_with('Q'),-Q8,-Q9,-Q10), all_vars(. != 0))

library(ggcorrplot)
ggcorrplot(cor(dadosP_filter[c(2,4:51)]), type = "lower", tl.cex = 7, tl.srt = 75) +
  labs(title = "Correlação entre a pontuação e as respostas \nao questionário")

# valores da correlação entre pontuação e cada pergunta do questionário
cor(dadosP_filter[2],dadosP_filter[4:51])

# na pergunta Q14 é onde encontramos o valor mais alto de correlação entre a nota e a pergunta
cor(dadosP_filter[2],dadosP_filter[4:51]) %>% max 

# correlação entre o lugar que foi cursado o ensino médio e se frequentou cursinho 
cor(dadosP_filter["Q4"],dadosP_filter["Q7"])

## violin plots com boxplots
# de cada resposta da Q7 (cursinho) com a nota da fase1
dadosP_filter %>% ggplot(aes(x=Q7, y=TOTAL, group=Q7)) + geom_violin() + geom_boxplot(width=.1)
# de cada resposta da Q7 (renda) com a nota da fase1
dadosP_filter %>% ggplot(aes(x=Q14, y=TOTAL, group=Q14)) + geom_violin() + geom_boxplot(width=.1)

# nota Fase 1 vs aprovados na Fase 1
dadosP %>% ggplot(aes(x=APROVA1,y=TOTAL,color=APROVA1)) + geom_violin() +
  geom_boxplot(width=0.1) + theme_minimal() + 
  labs(x = "Aprovação na primeira fase", y="Pontos na primeira fase", title = "Violin plot da pontuação por \nsituação na primeira fase") + theme(legend.position="none")
# nota Fase 1 vs convocados
dadosP %>% ggplot(aes(x=CONVOCADO,y=TOTAL,color=CONVOCADO)) + geom_violin() +
  geom_boxplot(width=0.1) + theme_minimal() + 
  labs(x = "Aprovação na segunda fase", y="Pontos na primeira fase", title = "Violin plot da pontuação por \nsituação na segunda fase") + theme(legend.position="none")


## Tabela de frequência, proporção e número de convocados por área de conhecimento
#classes de cursos
artes <- c(26,25,64,23,22, 70, 701, 703, 705, 706, 707, 708, 709, 710, 711, 712, 713, 721, 722, 723, 724, 725, 726, 727)
biologicas <- c(6,100,27,45,21,63,58,46,15,107,14)
humanas <- c(109,110, 17,47,16,44,75,30,54,55,19,7,57,56,18,20,38,86)
exatas <- c(48,42,36,83,73,87,8,89,12,13,43,34,49,101,102,88,11,41,108,10,9,39,2,4,53,40, 29, 1, 28, 51, 5, 50, 94, 188, 62, 37)
profis <-c(200)

#criando nova coluna com a área escolhida pelo candidato
dadosP_area <- dadosP %>% 
  select(EMPCT, TOTAL, CONVOCADO, starts_with('o')) %>% 
  mutate (tipoCurso = case_when(opc1 %in% artes ~ "artes",
                                opc1 %in% biologicas ~ "bio",
                                opc1 %in% humanas ~ "hum",
                                opc1 %in% exatas ~ "exatas",
                                opc1 %in% profis ~ "profis"))
library(knitr)
library(kableExtra)

dadosP_area$tipoCurso %>% table() %>% as.data.frame %>% 
  cbind(dadosP_area$tipoCurso %>% table %>% prop.table %>% round(3) %>% as.data.frame %>% select(., Prop = Freq)) %>% 
  left_join(dadosP_area[c("CONVOCADO", "tipoCurso")] %>% table %>% as.tibble() %>% filter(CONVOCADO == "S") %>% select(-CONVOCADO), 
            by = c("." = "tipoCurso")) %>%
  kable("latex", caption = "Tabela de frequência, proporção e número de convocados por área de conhecimento") %>%
  kable_styling()



# pt1 ---------------------------------------------------------------------

## média e variância das notas em dadosP
dadosP %>% summarise(mean=mean(TOTAL),var=var(TOTAL))

## tirar amostra piloto de 5% do total
piloto <- dadosP %>% sample_frac(0.05)
## subtrair piloto de dados
dados <- dadosP %>% setdiff(piloto)

#descritivos de piloto e dados
piloto %>% summarise(mean=mean(TOTAL),var=var(TOTAL))
dados %>% summarise(mean=mean(TOTAL),var=var(TOTAL))

## qtde de alunos
N <- nrow(dados)

## estimativa de variancia --- variancia de piloto
s2 <- var(piloto$TOTAL)

## AAS SEM reposição --- tamanho da amostra
tol <- 1 #tolerância de um ponto na estimação
z_gamma <- qnorm(0.975) # alpha = 0.05
n <- ceiling( 1/( tol^2/((z_gamma^2)*(s2)) + 1/N ) )

## seleção da amostra
amsA2 <- dados %>% sample_n(n)

## media estimada
nota_mhat_A2 <- mean(amsA2$TOTAL)

#limpar variaveis pt1
#rm(list=setdiff(ls(),c('Conv','Opc','Quest','Fase1','corrConv')))

# pt2 ---------------------------------------------------------------------

## criar dadosQ4 a partir de dadosgeral, excluindo não respondentes Q4
## --- empct, resposta Q4, Convocado
#### dados <- Quest %>% filter(Q4 != 0) %>% select(EMPCT,Q4) %>%
left_join(corrConv, by = 'EMPCT')
dadosQ4 <- dadosgeral %>% drop_na(Q4)
prop.table(table(dados$Q4,dados$CONVOCADO),1)

## gerando variavel y = 1 se Q4==1 e 0 caso contrario
dadosQ4 <- dadosQ4 %>% mutate(y = ifelse(Q4==1,1,0))

## proporção e variância das Q4 --- escola publica em dados
dadosQ4 %>% summarise(prop=mean(y),var=var(y))

## tirar amostra piloto de 5% do total 
piloto <- dados %>% sample_frac(0.05)
## subtrair piloto de dadosQ4
dados <- dadosQ4 %>% setdiff(piloto)
prop.table(table(dados$y));prop.table(table(piloto$y))

#descritivos de piloto e dados
piloto %>% summarise(prop=mean(y),var=var(y))
dados %>% summarise(prop=mean(y),var=var(y))

## qtde de alunos
N <- nrow(dados)

## estimativa de variancia --- variancia de piloto
## se quiser versão conservativa usar s2 = 1/4
s2 <- var(piloto$y)

## AAS SEM reposição --- tamanho da amostra
tol <- 0.01 #tolerância na estimação
z_gamma <- qnorm(0.975) # alpha = 0.05
n <- ceiling( N / ( (N-1)*tol^2 / ( (z_gamma^2)*(s2)) + 1 ) )

## seleção da amostra
amsA2 <- dados %>% sample_n(n)

## media estimada
escola_phat_A2 <- mean(amsA2$y)

#limpar variaveis pt2
#rm(list=setdiff(ls(),c('Conv','Opc','Quest','Fase1','corrConv')))


# pt3 ---------------------------------------------------------------------
## Q32C quartos existentes --- a resposta 4 ou + vai ser contabilizada como
## 4 quartos somente; a resposta 5 significa não tem!!!

## criar dadosQ32C a partir de dados geral, excluindo não respondentes Q32C
## --- empct, resposta Q32C
#### separar por convocação não muda as proporções
# dadosQ32C <- Quest %>% filter(Q32C != 0) %>% select(EMPCT,Q32C)
dadosQ32C <- dadosgeral %>% drop_na(Q32C)

## recodificar resposta 5 (não tem) como sendo 0
## dessa forma o problema se torna estimar o total de Q32C
dadosQ32C <- dadosQ32C %>% mutate(Q32C = ifelse(Q32C==5,0,Q32C))

## média e variância das Q32C --- qtde de quartos em dados
dadosQ32C %>% summarise(total=sum(Q32C),var=var(Q32C))

## tirar amostra piloto de 5% do total 
piloto <- dadosQ32C %>% sample_frac(0.05)
## subtrair piloto de dadosQ32C
dados <- dadosQ32C %>% setdiff(piloto)
prop.table(table(dados$Q32C));prop.table(table(piloto$Q32C))

#descritivos de piloto e dados
piloto %>% summarise(total=sum(Q32C),var=var(Q32C))
dados %>% summarise(total=sum(Q32C),var=var(Q32C))

## qtde de alunos
N <- nrow(dados)

## estimativa de variancia --- variancia de piloto
s2 <- var(piloto$Q32C)

## AAS SEM reposição --- tamanho da amostra
tol <- 1000 #tolerância na estimação
z_gamma <- qnorm(0.975) # alpha = 0.05
n <- ceiling( N / ( tol^2 / ( (z_gamma^2)*(s2)*N) + 1 ) )

## seleção da amostra
amsA2 <- dados %>% sample_n(n)

## media estimada
quartos_that_A2 <- ceiling(N*mean(amsA2$Q32C))

#limpar variaveis pt3
#rm(list=setdiff(ls(),c('Conv','Opc','Quest','Fase1','corrConv')))


# deprecated --------------------------------------------------------------
#ind <- dados %>% pull(APROVA1) %>% createDataPartition(p=0.05) %>% pluck(1)



# estratificando ------------------------------------------------------------

## criar dadosgeral que comportem o questionário, opções de curso, informações da
## fase 1 (sem as questões), e a convocação.
dadosgeral <- Fase1 %>% select(-starts_with('Q'),-starts_with('R')) %>%
  left_join(corrConv, by = 'EMPCT') %>%
  right_join(Opc, by = 'EMPCT') %>%
  left_join(Quest, by = 'EMPCT')

## op1 seria o nome da coluna q tem o número da primeira opção de curso
library(magrittr)
dadosgeral %<>% mutate (tipoCurso = case_when(opc1 %in% artes ~ "artes",
                                              opc1 %in% biologicas ~ "bio",
                                              opc1 %in% humanas ~ "hum",
                                              opc1 %in% exatas ~ "exatas",
                                              opc1 %in% profis ~ "profis"))

# pt1 estr -------------------------------------------------------------------------------------------
#### tamanho da amostra por estratificação
## calcular media, variancia , e peso amostral de cada estrato 
##substituir Estrato em group_by() pelo nome da coluna em factor  (é importante q seja transformada em factor na hora de usar o método createDataPartition logo mais) q representa os estratos
## Y seria a variável a ser estimada

## tirar amostra piloto de 5% do total mantendo proporção em APROVA1
piloto <- dadosgeral %>% sample_frac(0.05)
## subtrair piloto de dados
dadosgeral <- dadosgeral %>% setdiff(piloto)

N <- nrow(dadosgeral)
piloto_h <- piloto %>% group_by(as.factor(tipoCurso)) %>% 
  summarise(m_h=mean(TOTAL),var_h = var(TOTAL), N_h=n()) %>% mutate(W_h = N_h/N) %>% ungroup()

## criar AE_2pr (AASs de AE proporcional)de Y estratificada por Estrato
library(caret)
## calcular tamanho n de AEpr --- usar os mesmos tol e z_gamma 
## definidos na seção correspondente
tol <- 1 #tolerância na estimação
z_gamma <- qnorm(0.975) # alpha = 0.05

## novo s2 e tamanho da amostra n
s2 <- sum(piloto_h$var_h*piloto_h$W_h)
n <- ceiling( 1 / (tol^2 / ( (z_gamma^2)*(s2)) + 1/N ) )

## índices para gerar amostra, onde Estrato é o nome da variável q representa os estratos
ind <- dadosgeral %>% select(tipoCurso) %>% mutate(tipoCurso = as.factor(tipoCurso)) %>% pull() %>% createDataPartition(p=n/N) %>% pluck(1)

##seleção da amostra AEpr sem repetição
amsAE2pr <- dadosgeral %>% slice(ind)
amsAE2pr_h <- amsAE2pr %>% group_by(tipoCurso) %>% summarise(m_h=mean(TOTAL),var_h = var(TOTAL), n_h=n()) %>% mutate(w_h = n_h/n) %>% ungroup()

## calcular média amostral AE2pr
nota_mhat_AE2pr <- sum(amsAE2pr_h$m_h * amsAE2pr_h$w_h)



# pt2 estr -------------------------------------------------------------------------------------------
N <- nrow(dados)
piloto_h <- piloto %>% group_by(Estrato) %>% summarise(prop_h=mean(Y),var_h = var(Y), N_h=n()) %>% mutate(W_h = N_h/N) %>% ungroup()

## criar AE_2pr (AASs de AE proporcional)de Y estratificada por Estrato
library(caret)
## calcular tamanho n de AEpr --- usar os mesmos tol e z_gamma 
## definidos na seção correspondente
tol <- 0.01 #tolerância na estimação
z_gamma <- qnorm(0.975) # alpha = 0.05

## calculo de n_total por AE2pr
## novo s2 e tamanho da amostra n
s2 <- sum(piloto_h$var_h*piloto_h$W_h)
n <- ceiling( N / ( (N-1)*tol^2 / ( (z_gamma^2)*(s2)) + 1 ) )

## índices para gerar amostra, onde Estrato é o nome da variável q representa os estratos
ind <- dados %>% pull(Estrato) %>% createDataPartition(p=n/N) %>% pluck(1)

##seleção da amostra AEpr sem repetição
amsAE2pr <- dados %>% slice(ind)
amsAE2pr_h <- amsAE2pr %>% group_by(Estrato) %>% summarise(prop_h=mean(Y),var_h = var(Y), n_h=n()) %>% mutate(w_h = n_h/n) %>% ungroup()

## calcular total amostral AE2pr
escola_phat_AE2pr <- sum(amsAE2pr_h$prop_h * amsAE2pr_h$w_h)



# pt3 estr -------------------------------------------------------------------------------------------

N <- nrow(dados)
piloto_h <- piloto %>% group_by(Estrato) %>% summarise(m_h=mean(Y),var_h = var(Y), N_h=n()) %>% mutate(W_h = N_h/N) %>% ungroup()

## criar AE_2pr (AASs de AE proporcional)de Y estratificada por Estrato
library(caret)
## calcular tamanho n de AEpr --- usar os mesmos tol e z_gamma 
## definidos na seção correspondente
tol <- 1000 #tolerância na estimação
z_gamma <- qnorm(0.975) # alpha = 0.05

## calculo de n_total por AE2pr
## novo s2 e tamanho da amostra n
s2 <- sum(piloto_h$var_h*piloto_h$W_h)
n <- ceiling( N / (tol^2 / ( N*(z_gamma^2)*(s2)) + 1 ) )

## índices para gerar amostra, onde Estrato é o nome da variável q representa os estratos
ind <- dados %>% pull(Estrato) %>% createDataPartition(p=n/N) %>% pluck(1)

##seleção da amostra AEpr sem repetição
amsAE2pr <- dados %>% slice(ind)
amsAE2pr_h <- amsAE2pr %>% group_by(Estrato) %>% summarise(t_h=mean(Y),var_h = var(Y), n_h=n()) %>% mutate(w_h = n_h/n) %>% ungroup()

## calcular total amostral AE2pr
quartos_mhat_AE2pr <- sum(amsAE2pr_h$m_h * amsAE2pr_h$w_h)
quartos_that_AE2pr <- ceiling(N*quartos_mhat_AE2pr)


