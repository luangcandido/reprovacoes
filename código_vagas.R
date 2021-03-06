####### C�DIGO DA TABELA DE VAGAS
## Carregando os pacotes necess�rios.
library(rvest)
library(dplyr)
library(stringr)
library(scales)

## Definindo o working directory.
setwd("C:/Users/Fernanda/Documents/Reprova��es")

## Definindo os nomes dos departamentos coletados.
## Os nomes precisam ser coerentes com os nomes das p�ginas html 
## que foram baixadas do site da UFF e que cont�m os dados.
departamentos <- c("vqi_depqu�micaVR", "vps_deppsicologiaVR",
                   "vmt_depengenhariametal�rgica", "vmd_depmultidisciplinarVR",
                   "vma_depmatem�ticaVR", "vfi_depf�sicaVR",
                   "vep_depengenhariadeprodu��oVR", "vem_depengenhariamec�nicaVR",
                   "vea_depengenhariadeagroneg�cios", "vdi_depdireitoVR",
                   "vco_depcontabilidadeVR", "vce_depci�nciasexatas", 
                   "vad_depadministra��oeadministra��op�blicaVR", "tur_depurbanismo",
                   "tet_depengenhariadetelecomunica��es", "ter_depengenhariaagr�colaemeioambiente",
                   "teq_depengenhariaqu�micaedepetr�leo", "tep_depengenhariadeprodu��o",
                   "tem_depengenhariamec�nica", "tee_depengenhariael�trica",
                   "tec_depengenhariacivil", "tdt_depdesenhot�cnico",
                   "tcc_depci�nciadacomputa��o", "tar_deparquitetura", 
                   "stt_depturismo", "ste_depempreendedorismoegest�o",
                   "stc_depcontabilidade", "sta_depadministra��o",
                   "ssn_depservi�osocial", "sse_depsociedadeeduca��oeconhecimento",
                   "ssc_depservi�osocialcampos", "sfp_depfundamentospedag�gicos",
                   "sfc_depfundamentosdaci�nciasdasociedade", "sen_depeconomia",
                   "sdv_depdireitoprivado", "sdp_depdireitoprocessual", 
                   "sdb_depdireitop�blico", "rps_deppsicologia",
                   "rir_depinterdisciplinardeRO", "ren_depenfermagemRO",
                   "reg_depengenharia", "rcn_depci�nciasdanatureza",
                   "rcm_depcomputa��o", "rae_departeseestudosculturais", 
                   "peb_depci�nciasexatasbiol�gicasedaterra", "pde_depengenhariadeprodu��opetr�polis",
                   "pch_depci�nciashumanas", "mzo_depzootecnia",
                   "mtc_deptecnologiafarmac�utica", "mta_deptecnologiadosalimentos",
                   "msv_depsa�decoletivaveterin�riaesa�dep�blica", "msm_deppsiquiatriaesa�demental",
                   "mpt_deppatologia", "mps_depplanejamentoemsa�de",
                   "mot_depodontot�cnica", "moc_depodontocl�nica", 
                   "mns_depnutri��osocial", "mnd_depnutri��oedi�tica",
                   "mmo_depmorfologia", "mmi_depmaternoinfanti", 
                   "mip_depmicrobiologiaeparasitologia", "mfl_depfisiologiaefarmacologia",
                   "mfe_depfundamentosdeenfermagemeadm", "mep_depenfermagemmaternoinfantilpsiqui�trica",
                   "mem_depenfermagemm�dicocir�rgica", "meb_depepidemologiaebioestat�stica",
                   "mdm_depadministra��omaca�", "mdi_depdireitomaca�",
                   "mcv_deppatologiaecl�nicaveterin�ria", "mct_depcontabilidademaca�",
                   "mcg_depcirurgiageraleespecializada", "mbo_depbromatologia", 
                   "maf_depfarm�ciaeadmfarmac�utica", "gso_depsociologiaemetodologiaemcs",
                   "gsi_deppsicologia", "grc_depgeografiacampos", 
                   "gqo_depqu�micaorg�nica", "gqi_depqu�micainorg�nica",
                   "gqa_depqu�micaanal�tica", "gne_depneurobiologia",
                   "gma_depmatem�ticaaplicada", "gle_depletrasestrangeirasmodernas",
                   "glc_depletrascl�ssicasevern�culas", "gim_depimunobiologia",
                   "ght_dephist�ria", "ggo_depgeologiaegeof�sica",
                   "ggm_depgeometria", "gge_depgeografia",
                   "gfq_depf�sicoqu�mica", "gfl_depfilosofia",
                   "gfi_depf�sica", "get_depestat�stica",
                   "geo_depgeoqu�mica", "gec_depestudosculturaisem�dia", 
                   "gcv_depcinemaevideo", "gcp_depci�nciapol�tica",
                   "gco_depcomunica��osocial", "gcm_depbiologiacelularemolecular", 
                   "gcl_depci�nciadalinguagem", "gci_depci�nciadainforma��o", 
                   "gbm_depbiologiamarinha", "gbg_depbiologiageral",
                   "gat_departe", "gap_depantropologia",
                   "gan_depan�lise", "gag_depan�lisegeoambiental",
                   "ffe_depforma��oespec�fica", "fef_depforma��oespec�ficaemfonoaudiologia",
                   "fcb_depci�nciasb�sicas", "dsp_depseguran�ap�blica", 
                   "dgp_depgeografiaepolp�blicas", "dei_depestudosestrat�gicoserela��esinternacionais",
                   "ded_depeduca��oangra", "dda_depdireitoaplicado", "dcj_depci�nciasjudici�rias",
                   "cps_deppsicologiacampos", "coc_depci�nciassociaiscampos",
                   "cht_dephist�riacampos", "cec_depci�nciasecon�micascampos")

## Criando o data.frame-base onde ser�o inseridas as tabelas 
## extra�das do htm de cada departamento.
UFF_VAGAS <- data.frame()

## Loop respons�vel por extrair as tabelas e os dados de interesse das p�ginas do site da UFF 
## (4 por departamento, 1 para cada semestre entre 17.1 e 18.2)
for(i in departamentos){
  ## Carregamento do .html no R
  pagevagas20171 <- read_html(paste(i, sep = "", "_20171.html"))
  ## Extra��o da tabela contida no site.
  tablevagas20171 <- html_table(pagevagas20171, fill = TRUE)
  ## Filtragem da primeira coluna do tablevagas20171, que n�o � de interesse.
  tablevagas20171 <- tablevagas20171[2]
  ## Transforma��o do formato da tabela para df e introdu��o de
  ## uma nova coluna para identificar o semestre dos dados coletados.
  tablevagas20171 <- mutate(as.data.frame(tablevagas20171), "SEMESTRE" = 2017.1)
  ## Filtragem da linha 1 e das colunas 5 e 6 do df.
  tablevagas20171 <- tablevagas20171[-1,c(-5,-6)]
  ## Ajuste do nome das colunas.
  colnames(tablevagas20171) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  ## Cria��o de uma nova coluna identificando apenas o c�digo da disciplina.
  ## Essa coluna ser� usada posteriormente como crit�rio na uni�o das tabelas 
  ## de vagas e de reprova��es.
  tablevagas20171 <- mutate(tablevagas20171, "C�digo_Disciplina" = word(tablevagas20171$Disciplina, 1))

  ## Reprodu��o dos passos acima para o semestre 2017.2
  pagevagas20172 <- read_html(paste(i, sep = "", "_20172.html"))
  tablevagas20172 <- html_table(pagevagas20172, fill = TRUE)
  tablevagas20172 <- tablevagas20172[2]
  tablevagas20172 <- mutate(as.data.frame(tablevagas20172), "SEMESTRE" = 2017.2)
  tablevagas20172 <- tablevagas20172[-1,c(-5,-6)]
  colnames(tablevagas20172) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  tablevagas20172 <- mutate(tablevagas20172, "C�digo_Disciplina" = word(tablevagas20172$Disciplina, 1))
  
  ## Reprodu��o dos passos acima para o semestre 2018.1
  pagevagas20181 <- read_html(paste(i, sep = "", "_20181.html"))
  tablevagas20181 <- html_table(pagevagas20181, fill = TRUE)
  tablevagas20181 <- tablevagas20181[2]
  tablevagas20181 <- mutate(as.data.frame(tablevagas20181), "SEMESTRE" = 2018.1)
  tablevagas20181 <- tablevagas20181[-1,c(-5,-6)]
  colnames(tablevagas20181) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  tablevagas20181 <- mutate(tablevagas20181, "C�digo_Disciplina" = word(tablevagas20181$Disciplina, 1))
  
  ## Reprodu��o dos passos acima para o semestre 2018.2
  pagevagas20182 <- read_html(paste(i, sep = "", "_20182.html"))
  tablevagas20182 <- html_table(pagevagas20182, fill = TRUE)
  tablevagas20182 <- tablevagas20182[2]
  tablevagas20182 <- mutate(as.data.frame(tablevagas20182), "SEMESTRE" = 2018.2)
  tablevagas20182 <- tablevagas20182[-1,c(-5,-6)]
  colnames(tablevagas20182) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  tablevagas20182 <- mutate(tablevagas20182, "C�digo_Disciplina" = word(tablevagas20182$Disciplina, 1))
  
  ## Uni�o vertical dos dados semestrais de cada departamento.
  table_final <- bind_rows(tablevagas20171, tablevagas20172, tablevagas20181, tablevagas20182)
  ## Cria��o de uma nova coluna para identificar o departamento cujos dados foram levantados.
  table_final <- mutate(table_final, "Departamento" = i)
  
  ## Uni�o vertical do df-base com o df de cada departamento.
  ## Df UFF_VAGAS � resultado da uni�o vertical dos diferentes semestres
  ## de um mesmo departamento e dos diferentes departamentos ao longo do per�odo de observa��o.
  UFF_VAGAS <- bind_rows(UFF_VAGAS, table_final)
}

## Agrega��o das UFF_VAGAS por disciplina. 
## Obs: a UFF_VAGAS mostra as vagas oferecidas por mat�ria por curso. 
## Ex: UFF_VAGAS detalha quantas vagas de PEC II foram pra Economia, quantas para CS e etc.
UFF_VAGAS_AGREGADO <- UFF_VAGAS %>% 
  group_by(Departamento, C�digo_Disciplina, Disciplina, Semestre) %>%
  summarise("Vagas_Ofertadas" = sum(as.numeric(Vagas_Ofertadas)),
            "Vagas_Utilizadas" = sum(as.numeric(Vagas_Utilizadas))) %>%
  ## Cria��o da coluna de "aproveitamento", rela��o entre vagas ofertadas e utilizadas por cada mat�ria.
  mutate("Aproveitamento" = as.numeric(round((Vagas_Utilizadas/Vagas_Ofertadas), 2)))

## Buscando dados em que haja incoer�ncia entre a sigla do departamento presente na coluna C�digo_Disciplina
## e a sigla do departamento presente na coluna "Departamento".
## Uma incoer�ncia entre esses dados aponta que, provavelmente, o html de um determinado departamento
## foi baixado, mas identificado com um nome de departamento errado.
ACHANDO_ERROS <- UFF_VAGAS_AGREGADO %>% 
  mutate("DEPTOxC�DIGO" = word(C�digo_Disciplina, 1, sep = "0") == toupper(word(Departamento, 1, sep = "_")))