## Juntando UFF_VAGAS_AGREGADO e UFF_REPROVAÇÕES_AGREGADO por disciplina e semestre.
UFF_VAGASXREPROVAÇÕES <- inner_join(UFF_VAGAS_AGREGADO, UFF_REPROVAÇÕES_AGREGADO,
                                    by = c("Código_Disciplina", "Semestre"))
UFF_VAGASXREPROVAÇÕES <- UFF_VAGASXREPROVAÇÕES %>% group_by(Código_Disciplina, Semestre)
UFF_VAGASXREPROVAÇÕES <- UFF_VAGASXREPROVAÇÕES %>% 
  mutate("Taxa_Reprovação" = round(Reprovações/Vagas_Utilizadas, 2))

UFF_VAGASXREPROVAÇÕES <- UFF_VAGASXREPROVAÇÕES %>% 
  filter(between(Taxa_Reprovação, 0, 1))

## Salvando para uso no Markdown.
save(UFF_VAGASXREPROVAÇÕES, file="UFF_VAGASXREPROVAÇÕES.RData")