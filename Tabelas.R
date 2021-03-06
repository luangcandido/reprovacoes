####### Tabela 1

Tabela1 <- UFF_VAGASXREPROVA��ES %>% 
    group_by(Semestre) %>% 
    summarise("TOTAL" = length(Taxa_Reprova��o))

Tabela1[,3] <- UFF_VAGASXREPROVA��ES %>% 
  group_by(Semestre) %>% 
  filter(Taxa_Reprova��o > 0.5) %>%
  summarise("RM" = length(Taxa_Reprova��o)) %>%
  select(RM)

Tabela1 <- Tabela1 %>%
  mutate("P_R.M" = round(RM/TOTAL, 3))

colnames(Tabela1) <- c("Semestre", "Total de Disciplinas", "Disciplinas com T.R > 50%", "Participa��o %")

save(Tabela1, file="Tabela1.RData")

####### Tabela 2

## Filtrando as disciplinas com T. Reprova��o > 0.5 e Reprova��es > 10.
PIORES_DISCIPLINAS <- UFF_VAGASXREPROVA��ES %>%
  filter(Taxa_Reprova��o > 0.5) %>%
  filter(Reprova��es > 10)

## Selecionando as disciplinas que se adequaram no crit�rio acima
## por pelo menos 2 per�odos dos �ltimos 4.
PIORES_DISCIPLINAS2 <- as.data.frame(table(PIORES_DISCIPLINAS$C�digo_Disciplina))
PIORES_RESULTADOS2 <- PIORES_DISCIPLINAS2 %>% filter(Freq > 1)
PIORES_RESULTADOS2 <- PIORES_RESULTADOS2[,1]

## Mantendo do df original apenas os dados relativos �s piores disciplinas.
UFF_PIORES_DISCIPLINAS <- UFF_VAGASXREPROVA��ES %>%
  filter(C�digo_Disciplina %in% PIORES_RESULTADOS2)

## Agregando o resultado para o intervalo 2017.1 a 2018.2
UFF_PIORES_DISCIPLINAS_AGREGADO <- UFF_PIORES_DISCIPLINAS %>% 
  group_by(Departamento, C�digo_Disciplina, Nome_Disciplina) %>%
  summarise("Reprova��es" = sum(as.numeric(Reprova��es)),
            "Vagas_Utilizadas" = sum(as.numeric(Vagas_Utilizadas))) %>%
  mutate("Taxa_Reprova��o" = round(Reprova��es/Vagas_Utilizadas, 2))

## Criando a Tabela
Tabela2 <- UFF_PIORES_DISCIPLINAS_AGREGADO[,-1] %>% 
  mutate("Rank" = round(dense_rank(-Reprova��es), 0)) %>% 
  arrange(desc(Reprova��es))

Tabela2 <- Tabela2[1:50,]

save(Tabela2, file="Tabela2.RData")