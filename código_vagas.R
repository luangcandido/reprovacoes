####### CÓDIGO DA TABELA DE VAGAS
## Carregando os pacotes necessários.
library(rvest)
library(dplyr)
library(stringr)
library(scales)

## Definindo o working directory.
setwd("C:/Users/Fernanda/Documents/Reprovações")

## Definindo os nomes dos departamentos coletados.
## Os nomes precisam ser coerentes com os nomes das páginas html 
## que foram baixadas do site da UFF e que contém os dados.
departamentos <- c("vqi_depquímicaVR", "vps_deppsicologiaVR",
                   "vmt_depengenhariametalúrgica", "vmd_depmultidisciplinarVR",
                   "vma_depmatemáticaVR", "vfi_depfísicaVR",
                   "vep_depengenhariadeproduçãoVR", "vem_depengenhariamecânicaVR",
                   "vea_depengenhariadeagronegócios", "vdi_depdireitoVR",
                   "vco_depcontabilidadeVR", "vce_depciênciasexatas", 
                   "vad_depadministraçãoeadministraçãopúblicaVR", "tur_depurbanismo",
                   "tet_depengenhariadetelecomunicações", "ter_depengenhariaagrícolaemeioambiente",
                   "teq_depengenhariaquímicaedepetróleo", "tep_depengenhariadeprodução",
                   "tem_depengenhariamecânica", "tee_depengenhariaelétrica",
                   "tec_depengenhariacivil", "tdt_depdesenhotécnico",
                   "tcc_depciênciadacomputação", "tar_deparquitetura", 
                   "stt_depturismo", "ste_depempreendedorismoegestão",
                   "stc_depcontabilidade", "sta_depadministração",
                   "ssn_depserviçosocial", "sse_depsociedadeeducaçãoeconhecimento",
                   "ssc_depserviçosocialcampos", "sfp_depfundamentospedagógicos",
                   "sfc_depfundamentosdaciênciasdasociedade", "sen_depeconomia",
                   "sdv_depdireitoprivado", "sdp_depdireitoprocessual", 
                   "sdb_depdireitopúblico", "rps_deppsicologia",
                   "rir_depinterdisciplinardeRO", "ren_depenfermagemRO",
                   "reg_depengenharia", "rcn_depciênciasdanatureza",
                   "rcm_depcomputação", "rae_departeseestudosculturais", 
                   "peb_depciênciasexatasbiológicasedaterra", "pde_depengenhariadeproduçãopetrópolis",
                   "pch_depciênciashumanas", "mzo_depzootecnia",
                   "mtc_deptecnologiafarmacêutica", "mta_deptecnologiadosalimentos",
                   "msv_depsaúdecoletivaveterináriaesaúdepública", "msm_deppsiquiatriaesaúdemental",
                   "mpt_deppatologia", "mps_depplanejamentoemsaúde",
                   "mot_depodontotécnica", "moc_depodontoclínica", 
                   "mns_depnutriçãosocial", "mnd_depnutriçãoediética",
                   "mmo_depmorfologia", "mmi_depmaternoinfanti", 
                   "mip_depmicrobiologiaeparasitologia", "mfl_depfisiologiaefarmacologia",
                   "mfe_depfundamentosdeenfermagemeadm", "mep_depenfermagemmaternoinfantilpsiquiátrica",
                   "mem_depenfermagemmédicocirúrgica", "meb_depepidemologiaebioestatística",
                   "mdm_depadministraçãomacaé", "mdi_depdireitomacaé",
                   "mcv_deppatologiaeclínicaveterinária", "mct_depcontabilidademacaé",
                   "mcg_depcirurgiageraleespecializada", "mbo_depbromatologia", 
                   "maf_depfarmáciaeadmfarmacêutica", "gso_depsociologiaemetodologiaemcs",
                   "gsi_deppsicologia", "grc_depgeografiacampos", 
                   "gqo_depquímicaorgânica", "gqi_depquímicainorgânica",
                   "gqa_depquímicaanalítica", "gne_depneurobiologia",
                   "gma_depmatemáticaaplicada", "gle_depletrasestrangeirasmodernas",
                   "glc_depletrasclássicasevernáculas", "gim_depimunobiologia",
                   "ght_dephistória", "ggo_depgeologiaegeofísica",
                   "ggm_depgeometria", "gge_depgeografia",
                   "gfq_depfísicoquímica", "gfl_depfilosofia",
                   "gfi_depfísica", "get_depestatística",
                   "geo_depgeoquímica", "gec_depestudosculturaisemídia", 
                   "gcv_depcinemaevideo", "gcp_depciênciapolítica",
                   "gco_depcomunicaçãosocial", "gcm_depbiologiacelularemolecular", 
                   "gcl_depciênciadalinguagem", "gci_depciênciadainformação", 
                   "gbm_depbiologiamarinha", "gbg_depbiologiageral",
                   "gat_departe", "gap_depantropologia",
                   "gan_depanálise", "gag_depanálisegeoambiental",
                   "ffe_depformaçãoespecífica", "fef_depformaçãoespecíficaemfonoaudiologia",
                   "fcb_depciênciasbásicas", "dsp_depsegurançapública", 
                   "dgp_depgeografiaepolpúblicas", "dei_depestudosestratégicoserelaçõesinternacionais",
                   "ded_depeducaçãoangra", "dda_depdireitoaplicado", "dcj_depciênciasjudiciárias",
                   "cps_deppsicologiacampos", "coc_depciênciassociaiscampos",
                   "cht_dephistóriacampos", "cec_depciênciaseconômicascampos")

## Criando o data.frame-base onde serão inseridas as tabelas 
## extraídas do htm de cada departamento.
UFF_VAGAS <- data.frame()

## Loop responsável por extrair as tabelas e os dados de interesse das páginas do site da UFF 
## (4 por departamento, 1 para cada semestre entre 17.1 e 18.2)
for(i in departamentos){
  ## Carregamento do .html no R
  pagevagas20171 <- read_html(paste(i, sep = "", "_20171.html"))
  ## Extração da tabela contida no site.
  tablevagas20171 <- html_table(pagevagas20171, fill = TRUE)
  ## Filtragem da primeira coluna do tablevagas20171, que não é de interesse.
  tablevagas20171 <- tablevagas20171[2]
  ## Transformação do formato da tabela para df e introdução de
  ## uma nova coluna para identificar o semestre dos dados coletados.
  tablevagas20171 <- mutate(as.data.frame(tablevagas20171), "SEMESTRE" = 2017.1)
  ## Filtragem da linha 1 e das colunas 5 e 6 do df.
  tablevagas20171 <- tablevagas20171[-1,c(-5,-6)]
  ## Ajuste do nome das colunas.
  colnames(tablevagas20171) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  ## Criação de uma nova coluna identificando apenas o código da disciplina.
  ## Essa coluna será usada posteriormente como critério na união das tabelas 
  ## de vagas e de reprovações.
  tablevagas20171 <- mutate(tablevagas20171, "Código_Disciplina" = word(tablevagas20171$Disciplina, 1))

  ## Reprodução dos passos acima para o semestre 2017.2
  pagevagas20172 <- read_html(paste(i, sep = "", "_20172.html"))
  tablevagas20172 <- html_table(pagevagas20172, fill = TRUE)
  tablevagas20172 <- tablevagas20172[2]
  tablevagas20172 <- mutate(as.data.frame(tablevagas20172), "SEMESTRE" = 2017.2)
  tablevagas20172 <- tablevagas20172[-1,c(-5,-6)]
  colnames(tablevagas20172) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  tablevagas20172 <- mutate(tablevagas20172, "Código_Disciplina" = word(tablevagas20172$Disciplina, 1))
  
  ## Reprodução dos passos acima para o semestre 2018.1
  pagevagas20181 <- read_html(paste(i, sep = "", "_20181.html"))
  tablevagas20181 <- html_table(pagevagas20181, fill = TRUE)
  tablevagas20181 <- tablevagas20181[2]
  tablevagas20181 <- mutate(as.data.frame(tablevagas20181), "SEMESTRE" = 2018.1)
  tablevagas20181 <- tablevagas20181[-1,c(-5,-6)]
  colnames(tablevagas20181) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  tablevagas20181 <- mutate(tablevagas20181, "Código_Disciplina" = word(tablevagas20181$Disciplina, 1))
  
  ## Reprodução dos passos acima para o semestre 2018.2
  pagevagas20182 <- read_html(paste(i, sep = "", "_20182.html"))
  tablevagas20182 <- html_table(pagevagas20182, fill = TRUE)
  tablevagas20182 <- tablevagas20182[2]
  tablevagas20182 <- mutate(as.data.frame(tablevagas20182), "SEMESTRE" = 2018.2)
  tablevagas20182 <- tablevagas20182[-1,c(-5,-6)]
  colnames(tablevagas20182) <- c("Disciplina", "Curso", "Vagas_Ofertadas", "Vagas_Utilizadas", "Semestre")
  tablevagas20182 <- mutate(tablevagas20182, "Código_Disciplina" = word(tablevagas20182$Disciplina, 1))
  
  ## União vertical dos dados semestrais de cada departamento.
  table_final <- bind_rows(tablevagas20171, tablevagas20172, tablevagas20181, tablevagas20182)
  ## Criação de uma nova coluna para identificar o departamento cujos dados foram levantados.
  table_final <- mutate(table_final, "Departamento" = i)
  
  ## União vertical do df-base com o df de cada departamento.
  ## Df UFF_VAGAS é resultado da união vertical dos diferentes semestres
  ## de um mesmo departamento e dos diferentes departamentos ao longo do período de observação.
  UFF_VAGAS <- bind_rows(UFF_VAGAS, table_final)
}

## Agregação das UFF_VAGAS por disciplina. 
## Obs: a UFF_VAGAS mostra as vagas oferecidas por matéria por curso. 
## Ex: UFF_VAGAS detalha quantas vagas de PEC II foram pra Economia, quantas para CS e etc.
UFF_VAGAS_AGREGADO <- UFF_VAGAS %>% 
  group_by(Departamento, Código_Disciplina, Disciplina, Semestre) %>%
  summarise("Vagas_Ofertadas" = sum(as.numeric(Vagas_Ofertadas)),
            "Vagas_Utilizadas" = sum(as.numeric(Vagas_Utilizadas))) %>%
  ## Criação da coluna de "aproveitamento", relação entre vagas ofertadas e utilizadas por cada matéria.
  mutate("Aproveitamento" = as.numeric(round((Vagas_Utilizadas/Vagas_Ofertadas), 2)))

## Buscando dados em que haja incoerência entre a sigla do departamento presente na coluna Código_Disciplina
## e a sigla do departamento presente na coluna "Departamento".
## Uma incoerência entre esses dados aponta que, provavelmente, o html de um determinado departamento
## foi baixado, mas identificado com um nome de departamento errado.
ACHANDO_ERROS <- UFF_VAGAS_AGREGADO %>% 
  mutate("DEPTOxCÓDIGO" = word(Código_Disciplina, 1, sep = "0") == toupper(word(Departamento, 1, sep = "_")))