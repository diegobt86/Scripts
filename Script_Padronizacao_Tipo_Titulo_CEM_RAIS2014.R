#####################################################################################################
#####################################################################################################
################# PROCEDIMENTOS DE PADRONIZAÇÃO DE TIPO E TÍTULO DE LOGRADOUROS (TESE) ##############
#####################################################################################################
#####################################################################################################


######################## Padronização de Tipos de Logradouros do CEM 2016 (TESE) ########################################
#library(dplyr)
library(plyr)
#library(xlsx)
library(stringr)

setwd("C:/Projetos/Taina/Geocode")
a <-read.csv("TB_PADRONIZACAO_TIPO_CEM.csv", header = TRUE, sep = ';', dec = '.' , stringsAsFactors = FALSE, encoding = "latin1")

colnames(a) <- c("TIPOLOG", "TIPO_PADRONIZADO")

b <-read.table("LOGRADOUROS_CEM_SP_2016_v2.txt", header = TRUE, sep = ';', dec = ',' , stringsAsFactors = FALSE, encoding = "latin1", na.strings = c(" ","NA"))

c <- join (a, b, by = 'TIPOLOG', type= "right")


# Identificação de Tipos de logradouros não existentes na documentação
d <- c[ which(!is.na(c$TIPOLOG) & is.na(c$TIPO_PADRONIZADO)),]
d <- unique(d$TIPOLOG)

# Acrescentando os Tipos de Logradouros não existentes na documentação na tabela de padronização
a[nrow(a)+1,]= list("RA","RODOANEL")
a[nrow(a)+1,]= list("RI","RUA INTERNA")

c <- join (a, b, by = 'TIPOLOG', type= "right")

# Identificação de Tipos de logradouros não existentes na documentação
d1 <- c[ which(!is.na(c$TIPOLOG) & is.na(c$TIPO_PADRONIZADO)),]
d1 <- unique(d1$TIPOLOG)

data <- subset(c, select = c(1,2,11, 12, 13, 14, 15, 16, 17))


######################## Padronização de Títulos de Logradouros do CEM 2016 (TESE) ########################################

e <- read.csv("TB_PADRONIZACAO_TITULO_CEM.csv", header = TRUE, sep = ';', dec = '.' , stringsAsFactors = FALSE, encoding = "latin1")

colnames(e) <- c("NOMETIT", "TITULO_PADRONIZADO")

f <- join (e, c, by = 'NOMETIT', type= "right")

f$NOME_LOGR <- ifelse(!is.na(f$TITULO_PADRONIZADO) & !is.na(f$NOMEPREP), paste (f$TITULO_PADRONIZADO, f$NOMEPREP, f$NAME, sep=" "), "")
f$NOME_LOGR <- ifelse(!is.na(f$TITULO_PADRONIZADO) & is.na(f$NOMEPREP), paste (f$TITULO_PADRONIZADO, f$NAME, sep=" "), f$NOME_LOGR)
f$NOME_LOGR <- ifelse(is.na(f$TITULO_PADRONIZADO) & !is.na(f$NOMEPREP), paste (f$NOMEPREP, f$NAME, sep=" "), f$NOME_LOGR)
f$NOME_LOGR <- ifelse(is.na(f$TITULO_PADRONIZADO) & is.na(f$NOMEPREP), paste (f$NAME, sep=" "), f$NOME_LOGR)

ff <- subset(f, select = c(6, 4, 31))

write.csv2(ff, file = "NOME_LOGR_CEM_2016_PADRONIZADO.csv", row.names = FALSE)

# Identificação de Títulos de logradouros não existentes na documentação
g1 <- f[ which(!is.na(f$NOMETIT) & is.na(f$TITULO_PADRONIZADO)),]
g1 <- unique(g1$NOMETIT)

data2 <- subset(f, select = c(3,4,1,2,13,14, 15, 16, 17, 18))


######################## Padronização de Tipos de Logradouros RAIS 2014 (TESE) ########################################

h <- read.csv2("RAIS_SP_2014_correto.csv", header = TRUE)
#hh <- h[143474,]
#hh1 <- h[ which(h$nomelogradouro==''),]

data3 <- subset(h, select = c(1, 17, 18, 19, 24, 4, 5, 6, 3))
colnames(data3) <- c("ID","NOME_LOGRADOURO", "NUMERO_LOGRADOURO", "NOME_BAIRRO","razaosocial","cnae95classe","cnpjcei","cnpjraiz" ,"CEP_ESTABEL")
data3$NOME_LOGRADOURO <- gsub("\\."," ", data3$NOME_LOGRADOURO)
data3$NOME_LOGRADOURO <- gsub("\\,"," ", data3$NOME_LOGRADOURO) 
data3$NOME_LOGRADOURO <- trimws(data3$NOME_LOGRADOURO, which = ("both"))
data3$TIPO <- word(data3$NOME_LOGRADOURO, 1)
data3$TIPO <- trimws(data3$TIPO, which = ("both"))

i <- as.data.frame(table(data3$TIPO))
colnames(i) <- c("TIPO_NAO_PADRONIZADO", "FREQUENCIA")

j <- i[ which(i$FREQUENCIA >=10),]


#write.xlsx(j, "c:/Projetos/Tese/RAIS/Tipos_RAIS_SP_2014.xlsx")

k <-read.csv2("Tipos_RAIS_SP_2014.csv")
colnames(k) <- c("TIPO","TIPO_PADRONIZADO")

l <- join (data3, k, by = 'TIPO', type= "left")

######################## Padronização de Títulos de Logradouros RAIS 2014 (TESE) ########################################

colnames(j) <- c("TITULO_NAO_PADRONIZADO", "FREQUENCIA")
#write.xlsx(j, "c:/Projetos/Tese/RAIS/Titulos_RAIS_SP_2014.xlsx")

data3$NOME_LOGRADOURO <- trimws(data3$NOME_LOGRADOURO, which = ("both"))
data3$TITULO <- word(data3$NOME_LOGRADOURO, 2, sep = fixed(' '))

m <- as.data.frame(table(data3$TITULO))
colnames(m) <- c("TITULO_NAO_PADRONIZADO", "FREQUENCIA")

n <- m[ which(m$FREQUENCIA >=10),]
#write.xlsx(n, "c:/Projetos/Tese/RAIS/Titulos_RAIS_SP_2014_2.xlsx")

o <- read.csv2("Titulos_RAIS_SP_2014_final.csv")
colnames(o)<-c("TITULO", "TITULO_PADRONIZADO")
data4 <- join (data3, o, by='TITULO', type="left")
data4 <- dplyr::select(data4, ID, TITULO_PADRONIZADO)

data5 <- join(data4, l, by='ID', type="right")

data6 <- dplyr::select(data5, ID, TIPO_PADRONIZADO, TITULO_PADRONIZADO, NOME_LOGRADOURO, NUMERO_LOGRADOURO, CEP_ESTABEL, razaosocial, cnae95classe, cnpjcei, cnpjraiz)

data6$NOME_LOGRADOURO <- trimws(data6$NOME_LOGRADOURO, which = ("both"))
data6$NOME <- ifelse(!is.na(data6$TIPO_PADRONIZADO) & !is.na(data6$TITULO_PADRONIZADO), word(data6$NOME_LOGRADOURO, 3, -1), "")
data6$NOME <- ifelse(!is.na(data6$TIPO_PADRONIZADO) & is.na(data6$TITULO_PADRONIZADO), word(data6$NOME_LOGRADOURO, 2, -1), data6$NOME)
data6$NOME <- ifelse(is.na(data6$TIPO_PADRONIZADO) & is.na(data6$TITULO_PADRONIZADO), word(data6$NOME_LOGRADOURO, 1, -1), data6$NOME)
data6$NOME <- ifelse(is.na(data6$TIPO_PADRONIZADO) & !is.na(data6$TITULO_PADRONIZADO), word(data6$NOME_LOGRADOURO, 1, -1), data6$NOME)

data6$ENDERECO <- ifelse(!is.na(data6$TIPO_PADRONIZADO) & !is.na(data6$TITULO_PADRONIZADO), paste (data6$TIPO_PADRONIZADO, data6$TITULO_PADRONIZADO, data6$NOME, sep=" "), "")
data6$ENDERECO <- ifelse(!is.na(data6$TIPO_PADRONIZADO) & is.na(data6$TITULO_PADRONIZADO), paste (data6$TIPO_PADRONIZADO, data6$NOME, sep=" "), data6$ENDERECO)
data6$ENDERECO <- ifelse(is.na(data6$TIPO_PADRONIZADO) & !is.na(data6$TITULO_PADRONIZADO), paste (data6$TITULO_PADRONIZADO, data6$NOME, sep=" "), data6$ENDERECO)
data6$ENDERECO <- ifelse(is.na(data6$TIPO_PADRONIZADO) & is.na(data6$TITULO_PADRONIZADO), paste (data6$NOME), data6$ENDERECO)

data6$ENDERECO <- paste(data6$ENDERECO, data6$NUMERO_LOGRADOURO, sep = " ")
#data6$ENDERECO <- paste(data6$ENDERECO, data6$CEP_ESTABEL, sep = ", ")

data6$CEP_ESTABEL <- as.character(data6$CEP_ESTABEL)
data6$num_char <- nchar(data6$CEP_ESTABEL, type = "chars", allowNA = FALSE, keepNA = NA)

data6$CEP_ESTABEL <- ifelse(data6$num_char==7, paste("0", data6$CEP_ESTABEL, sep = ""), data6$CEP_ESTABEL) 
data6 <- data6[,-13]

write.csv2(data6, "RAIS_2014_PADRONIZADA_INTERSECT_CEP_CNPJ.csv", row.names = FALSE)

