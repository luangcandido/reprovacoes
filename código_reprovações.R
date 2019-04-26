####### CÓDIGO DA TABELA DE REPROVAÇÕES
## Código análogo ao de vagas.

## Definindo os nomes dos cursos coletados.
## Os nomes precisam ser coerentes com os nomes das páginas html 
## que foram baixadas do site da UFF e que contém os dados.
cursos <- c("116jornalismo", "115engenhariadeproduçaoPETROPOLIS",
            "114educaçaodocampoPADUA", "113engenhariadeproduçaoEAD",
            "108segurançapubicaEAD", "107processosgerenciais",
            "105cienciascontabeisMACAE", "104administraçaoMACAE",
            "103politicaspublicasANGRA", "102segurançapublica", 
            "101geografiaANGRA", "100artes",
            "99antropologia", "98sociologia",
            "96desenhoindustrial", "95cienciaambiental",
            "94hotelaria", "93cienciasnaturaisPADUA",
            "92computaçãoPADUA", "91psicologiaVR",
            "90direitoVR", "87letrasEAD",
            "86administraçaopublicaEAD", "85cienciasatuariais",
            "84direitoMACAE", "83sistemasdeinformação",
            "82historiaCAMPOS", "82fisicaPADUA",
            "81psicologiaCAMPOS", "80fonoaudiologiaNF",
            "79biomedicinaNF", "78quimicaVR",
            "77matematicaVR", "76fisicaVR",
            "75cienciascontabeisVR", "74administraçaopublicaVR",
            "73enfermagemRO", "72empreendedorismoeinovaçao",
            "71sistemasdecomputaçaoEAD", "70matematicaEAD",
            "69psicologiaRO", "68cienciassociaisCAMPOS",
            "67geografiaCAMPOS", "66cienciaseconomicasCAMPOS",
            "65pedagogiaPADUA", "64serviçosocialRO",
            "63engenhariadeproducaoRO", "62procultRO",
            "61odontologiaNF", "60cienciadacomputaçaoRO",
            "59relaçoesinternacionais", "58filosofia", 
            "57cinemaeaudiovisual", "56engenhariaderecursoshidricosemeioambiente",
            "55educaçaofisica", "54estatistica",
            "53administraçaoVR", "52engenhariadeagronegocios",
            "51engenhariadepetroleo", "50geofisica",
            "49estudosdemidia", "48biomedicina",
            "47turismo", "46engenhariamecanicaVR",
            "45engenhariadeproducaoVR", "44cienciasbiologicas",
            "43engenhariaagricolaeambiental", "42engenhariadeproduçao",
            "41engenhariadetelecomunicacoes", "40engenhariamecanica",
            "39engenhariametalurgicaVR", "38engenhariaeletrica",
            "37engenhariacivil", "36serviçosocialCAMPOS",
            "35matematicaPADUA", "34enfermagem", 
            "33procult", "32pedagogiaANGRA",
            "31cienciadacomputaçao", "30comunicaçaosocial",
            "29quimicaindustrial", "28quimica",
            "27engenhariaquimica", "26arquiteturaeurbanismo",
            "25fisica", "24psicologia",
            "23administraçao", "22cienciascontabeis",
            "21letras", "20matematica",
            "18medicinaveterinaria", "17odontologia",
            "16medicina", "15farmacia",
            "14arquivologia", "10pedagogia",
            "9nutriçao", "7direito",
            "6serviçosocial", "5cienciassociais", 
            "4cienciaseconomicas", "3geografia",
            "2historia", "1bibioteconomia")

## Criando o data.frame-base onde serão inseridas as tabelas 
## extraídas das páginas da UFF.
UFF_REPROVAÇÕES <- data.frame()

## Criando um df para achar erros.
ACHANDO_ERROS <- data.frame()

## Loop básico responsável por extrair os dados de interesse das página do site da UFF 
## (4 por curso, 1 para cada Semestre entre 17.1 e 18.2)
## Detalhe: enquanto na página de vagas ofertadas por departamento aparecem todas matérias ofertadas,
## no caso das reprovações os dados estavam divididos em diferentes páginas.
## Todavia, baixamos apenas a primeira página de cada curso. Em alguns casos, como jornalismo, isso engloba 
## o total de matérias que reprovaram pessoas do curso (algumas com 1 reprovação, p. ex). 
## Em outros, como o direito, a primeira página (primeira de várias) engloba apenas as 30 matérias com mais reprovações naquele Semestre.
## Isso gera um pequeno enviesamento dos dados, mas foi necessário dado o método de levantamento das estatísticas.
## No geral, esse enviesamento é pouco relevante pois exclui matérias que não estão entre as que mais reprovam por curso.
## Porém, na análise das distribuições de frequência, esse fato empurra a média de reprovações para valores mais altos que o efetivo. 

for(i in cursos){
  ## Carregamento do .html no R
  page20171 <- read_html(paste(i, sep = "", "_20171.html"))
  ## Extração da tabela contida no site.
  table20171 <- html_table(page20171, fill = TRUE)
  ## Filtragem da primeira coluna do table20171, que não é de interesse.
  table20171 <- table20171[2]
  ## Transformação do formato da tabela para df e introdução de
  ## uma nova coluna para identificar o Semestre dos dados coletados.
  table20171 <- mutate(as.data.frame(table20171), "Semestre" = 2017.1)
  ## Lidando com .htmls com problemas.
  if (colnames(table20171) == "Semestre") {
    ERRO <- data.frame("Curso_Problema" = i, "Semestre_Problema" = 2017.1)
    ACHANDO_ERROS <- bind_rows(ACHANDO_ERROS, ERRO)
    table20171 <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(table20171) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  } else {
    colnames(table20171) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  }

  ## Reprodução dos passos acima para o semestre 2017.2
  page20172 <- read_html(paste(i, sep = "", "_20172.html"))
  table20172 <- html_table(page20172, fill = TRUE)
  table20172 <- table20172[2]
  table20172 <- mutate(as.data.frame(table20172), "Semestre" = 2017.2)
  ## Lidando com .htmls com problemas.
  if (colnames(table20172) == "Semestre") {
    ERRO <- data.frame("Curso_Problema" = i, "Semestre_Problema" = 2017.2)
    ACHANDO_ERROS <- bind_rows(ACHANDO_ERROS, ERRO)
    table20172 <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(table20172) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  } else {
    colnames(table20172) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  }
  
  ## Reprodução dos passos acima para o semestre 2018.1
  page20181 <- read_html(paste(i, sep = "", "_20181.html"))
  table20181 <- html_table(page20181, fill = TRUE)
  table20181 <- table20181[2]
  table20181 <- mutate(as.data.frame(table20181), "Semestre" = 2018.1)
  ## Lidando com .htmls com problemas.
  if (colnames(table20181) == "Semestre") {
    ERRO <- data.frame("Curso_Problema" = i, "Semestre_Problema" = 2018.1)
    ACHANDO_ERROS <- bind_rows(ACHANDO_ERROS, ERRO)
    table20181 <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(table20181) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  } else {
    colnames(table20181) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  }
  
  ## Reprodução dos passos acima para o semestre 2018.2
  page20182 <- read_html(paste(i, sep = "", "_20182.html"))
  table20182 <- html_table(page20182, fill = TRUE)
  table20182 <- table20182[2]
  table20182 <- mutate(as.data.frame(table20182), "Semestre" = 2018.2)
  ## Lidando com .htmls com problemas.
  if (colnames(table20182) == "Semestre") {
    ERRO <- data.frame("Curso_Problema" = i, "Semestre_Problema" = 2018.2)
    ACHANDO_ERROS <- bind_rows(ACHANDO_ERROS, ERRO)
    table20182 <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(table20182) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  } else {
    colnames(table20182) <- c("Código_Disciplina", "Nome_Disciplina", "Reprovações", "Semestre")
  }
  
  ## União vertical dos dados semestrais de cada curso.
  table_final <- bind_rows(table20171, table20172, table20181, table20182)
  ## Criação de uma nova coluna para identificar o curso cujos dados foram levantados.
  table_final <- mutate(table_final, "Curso" = i)
  
  ## União vertical do df-base com o df de cada curso.
  ## Df UFF_REPROVAÇÕES é resultado da união vertical dos diferentes semestres
  ## de um mesmo curso e dos diferentes cursos ao longo do período de observação.
  UFF_REPROVAÇÕES <- bind_rows(UFF_REPROVAÇÕES, table_final)
}

## Somando os reprovados de uma mesma matéria em diferentes cursos (agregando por Código_Disciplina).
UFF_REPROVAÇÕES_AGREGADO <- UFF_REPROVAÇÕES %>%
  group_by(Código_Disciplina, Nome_Disciplina, Semestre) %>%
  summarise("Reprovações" = sum(as.numeric(Reprovações)))

## Filtrando EAD
UFF_REPROVAÇÕES_AGREGADO <- UFF_REPROVAÇÕES_AGREGADO %>%
  filter(word(Código_Disciplina, 1, sep = "0") != "EAD")

## Salvando para uso no Markdown.
save(UFF_REPROVAÇÕES_AGREGADO, file="UFF_REPROVAÇÕES_AGREGADO.RData")