## Juntando UFF_VAGAS_AGREGADO e UFF_REPROVA��ES_AGREGADO por disciplina e semestre.
UFF_VAGASXREPROVA��ES <- inner_join(UFF_VAGAS_AGREGADO, UFF_REPROVA��ES_AGREGADO,
                                    by = c("C�digo_Disciplina", "Semestre"))
UFF_VAGASXREPROVA��ES <- UFF_VAGASXREPROVA��ES %>% group_by(C�digo_Disciplina, Semestre)
UFF_VAGASXREPROVA��ES <- UFF_VAGASXREPROVA��ES %>% 
  mutate("Taxa_Reprova��o" = round(Reprova��es/Vagas_Utilizadas, 2))

UFF_VAGASXREPROVA��ES <- UFF_VAGASXREPROVA��ES %>% 
  filter(between(Taxa_Reprova��o, 0, 1))

## Salvando para uso no Markdown.
save(UFF_VAGASXREPROVA��ES, file="UFF_VAGASXREPROVA��ES.RData")