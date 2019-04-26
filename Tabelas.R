####### Tabela 1

Tabela1 <- UFF_VAGASXREPROVAÇÕES %>% 
    group_by(Semestre) %>% 
    summarise("TOTAL" = length(Taxa_Reprovação))

Tabela1[,3] <- UFF_VAGASXREPROVAÇÕES %>% 
  group_by(Semestre) %>% 
  filter(Taxa_Reprovação > 0.5) %>%
  summarise("RM" = length(Taxa_Reprovação)) %>%
  select(RM)

Tabela1 <- Tabela1 %>%
  mutate("P_R.M" = round(RM/TOTAL, 3))

colnames(Tabela1) <- c("Semestre", "Total de Disciplinas", "Disciplinas com T.R > 50%", "Participação %")

save(Tabela1, file="Tabela1.RData")

####### Tabela 2

## Filtrando as disciplinas com T. Reprovação > 0.5 e Reprovações > 10.
PIORES_DISCIPLINAS <- UFF_VAGASXREPROVAÇÕES %>%
  filter(Taxa_Reprovação > 0.5) %>%
  filter(Reprovações > 10)

## Selecionando as disciplinas que se adequaram no critério acima
## por pelo menos 2 períodos dos últimos 4.
PIORES_DISCIPLINAS2 <- as.data.frame(table(PIORES_DISCIPLINAS$Código_Disciplina))
PIORES_RESULTADOS2 <- PIORES_DISCIPLINAS2 %>% filter(Freq > 1)
PIORES_RESULTADOS2 <- PIORES_RESULTADOS2[,1]

## Mantendo do df original apenas os dados relativos às piores disciplinas.
UFF_PIORES_DISCIPLINAS <- UFF_VAGASXREPROVAÇÕES %>%
  filter(Código_Disciplina %in% PIORES_RESULTADOS2)

## Agregando o resultado para o intervalo 2017.1 a 2018.2
UFF_PIORES_DISCIPLINAS_AGREGADO <- UFF_PIORES_DISCIPLINAS %>% 
  group_by(Departamento, Código_Disciplina, Nome_Disciplina) %>%
  summarise("Reprovações" = sum(as.numeric(Reprovações)),
            "Vagas_Utilizadas" = sum(as.numeric(Vagas_Utilizadas))) %>%
  mutate("Taxa_Reprovação" = round(Reprovações/Vagas_Utilizadas, 2))

## Criando a Tabela
Tabela2 <- UFF_PIORES_DISCIPLINAS_AGREGADO[,-1] %>% 
  mutate("Rank" = round(dense_rank(-Reprovações), 0)) %>% 
  arrange(desc(Reprovações))

Tabela2 <- Tabela2[1:50,]

save(Tabela2, file="Tabela2.RData")