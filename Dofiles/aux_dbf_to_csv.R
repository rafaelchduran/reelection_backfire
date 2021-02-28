### ENVIPE DBF to .csv
rm(list=ls())


### Packages
require(foreign)

#working directory:
setwd("~/Dropbox/Dissertation/GovernmentStrategies/Data/ConstructionDatabase/EncuestaVictimizacion/") 

# read dbf and save to csv
##PERCEPCION DE INSEGURIDAD
envipe_2011<-read.dbf("envipe_2011/tper_vic.DBF", as.is = FALSE)
write.csv(envipe_2011,"CSVs/envipe_2011.csv")

envipe_2012<-read.dbf("envipe_2012/tper_vic.DBF", as.is = FALSE)
write.csv(envipe_2012,"CSVs/envipe_2012.csv")

envipe_2013<-read.dbf("envipe_2013/tper_vic.DBF", as.is = FALSE)
write.csv(envipe_2013,"CSVs/envipe_2013.csv")

envipe_2014a<-read.dbf("envipe_2014/TPer_Vic1.dbf", as.is = FALSE)
write.csv(envipe_2014a,"CSVs/envipe_2014.csv")

envipe_2014b<-read.dbf("envipe_2014/TPer_Vic2.dbf", as.is = FALSE)
write.csv(envipe_2014b,"CSVs/envipe_2014_b.csv")

envipe_2015a<-read.dbf("envipe_2015/TPer_Vic1.dbf", as.is = FALSE)
write.csv(envipe_2015a,"CSVs/envipe_2015.csv")

envipe_2015b<-read.dbf("envipe_2015/TPer_Vic2.dbf", as.is = FALSE)
write.csv(envipe_2015b,"CSVs/envipe_2015_b.csv")

envipe_2016a<-read.dbf("envipe_2016/TPer_Vic1.dbf", as.is = FALSE)
write.csv(envipe_2016a,"CSVs/envipe_2016.csv")

envipe_2016b<-read.dbf("envipe_2016/TPer_Vic2.dbf", as.is = FALSE)
write.csv(envipe_2016b,"CSVs/envipe_2016_b.csv")

envipe_2017a<-read.dbf("envipe_2017/TPer_Vic1.dbf", as.is = FALSE)
write.csv(envipe_2017a,"CSVs/envipe_2017.csv")

envipe_2017b<-read.dbf("envipe_2017/TPer_Vic2.dbf", as.is = FALSE)
write.csv(envipe_2017b,"CSVs/envipe_2017_b.csv")

envipe_2018a<-read.dbf("envipe_2018/TPer_Vic1.dbf", as.is = FALSE)
write.csv(envipe_2018a,"CSVs/envipe_2018.csv")

envipe_2018b<-read.dbf("envipe_2018/TPer_Vic2.dbf", as.is = FALSE)
write.csv(envipe_2018b,"CSVs/envipe_2018_b.csv")

envipe_2019a<-read.dbf("envipe_2019/TPer_Vic1.dbf", as.is = FALSE)
write.csv(envipe_2019a,"CSVs/envipe_2019.csv")

envipe_2019b<-read.dbf("envipe_2019/TPer_Vic2.dbf", as.is = FALSE)
write.csv(envipe_2019b,"CSVs/envipe_2019_b.csv")


##DATOS GENERALES
envipe_2011_viv<-read.dbf("envipe_2011/tvivien.dbf", as.is = FALSE)
write.csv(envipe_2011_viv,"CSVs/envipe_2011_viv.csv")

envipe_2012_viv<-read.dbf("envipe_2012/tvivienda.dbf", as.is = FALSE)
write.csv(envipe_2012_viv,"CSVs/envipe_2012_viv.csv")

envipe_2013_viv<-read.dbf("envipe_2013/TViviend.DBF", as.is = FALSE)
write.csv(envipe_2013_viv,"CSVs/envipe_2013_viv.csv")

envipe_2014_viv<-read.dbf("envipe_2014/TVivienda.dbf", as.is = FALSE)
write.csv(envipe_2014_viv,"CSVs/envipe_2014_viv.csv")

envipe_2015_viv<-read.dbf("envipe_2015/TVivienda.dbf", as.is = FALSE)
write.csv(envipe_2015_viv,"CSVs/envipe_2015_viv.csv")

envipe_2016_viv<-read.dbf("envipe_2016/TVivienda.dbf", as.is = FALSE)
write.csv(envipe_2016_viv,"CSVs/envipe_2016_viv.csv")

envipe_2017_viv<-read.dbf("envipe_2017/TVivienda.dbf", as.is = FALSE)
write.csv(envipe_2017_viv,"CSVs/envipe_2017_viv.csv")

envipe_2018_viv<-read.dbf("envipe_2018/TVivienda.dbf", as.is = FALSE)
write.csv(envipe_2018_viv,"CSVs/envipe_2018_viv.csv")

envipe_2019_viv<-read.dbf("envipe_2019/TVivienda.dbf", as.is = FALSE)
write.csv(envipe_2019_viv,"CSVs/envipe_2019_viv.csv")

#Mando unico
# read dbf and save to csv

##NIVEL MUNICIPAL
setwd("~/Dropbox/Dissertation/GovernmentStrategies/Data/ConstructionDatabase/MandoUnico/CensoGobiernoMunicipal/") 

mando_2011a<-read.dbf("2011/Ejercicio de la Funcion/Base de datos/EFUN_SPU.DBF", as.is = FALSE)
write.csv(mando_2011a,"CSVs/mando_unico_2011a.csv")

mando_2011b<-read.dbf("2011/Ejercicio de la Funcion/Base de datos/MOTIVCON.DBF", as.is = FALSE)
write.csv(mando_2011b,"CSVs/mando_unico_2011b.csv")

mando_2013a<-read.dbf("2013/SP_Ejercicio_funcion/Bases_de_datos/EJER_FN1.DBF", as.is = FALSE)
write.csv(mando_2013a,"CSVs/mando_unico_2013a.csv")
mando_2013b<-read.dbf("2013/SP_Ejercicio_funcion/Bases_de_datos/EJER_FN2.DBF", as.is = FALSE)
write.csv(mando_2013b,"CSVs/mando_unico_2013b.csv")

mando_2015<-read.dbf("2015/SP_Seguridad_publica_cngmd2015_dbf/Bases_datos/SPEJFUES.DBF", as.is = FALSE)
write.csv(mando_2015,"CSVs/mando_unico_2015.csv")

mando_2017a<-read.dbf("2017/Mando_unico_policial_cngmd2017_dbf/Bases_Datos/CONVEFUN.DBF", as.is = FALSE)
write.csv(mando_2017a,"CSVs/mando_unico_2017a.csv")

mando_2017b<-read.dbf("2017/Mando_unico_policial_cngmd2017_dbf/Bases_Datos/GOBIERNO.DBF", as.is = FALSE)
write.csv(mando_2017b,"CSVs/mando_unico_2017b.csv")

mando_2017c<-read.dbf("2017/Mando_unico_policial_cngmd2017_dbf/Bases_Datos/MANDOUNI.DBF", as.is = FALSE)
write.csv(mando_2017c,"CSVs/mando_unico_2017c.csv")

mando_2017d<-read.dbf("2017/Mando_unico_policial_cngmd2017_dbf/Bases_Datos/PERSDEST.DBF", as.is = FALSE)
write.csv(mando_2017d,"CSVs/mando_unico_2017d.csv")

mando_2019a<-read.dbf("2019/Mando_unico_policial_cngmd2019_dbf/Bases_Datos/CONVEFUN.DBF", as.is = FALSE)
write.csv(mando_2019a,"CSVs/mando_unico_2019a.csv")

mando_2019b<-read.dbf("2019/Mando_unico_policial_cngmd2019_dbf/Bases_Datos/GOBIERNO.DBF", as.is = FALSE)
write.csv(mando_2019b,"CSVs/mando_unico_2019b.csv")

mando_2019c<-read.dbf("2019/Mando_unico_policial_cngmd2019_dbf/Bases_Datos/MANDOUNI.DBF", as.is = FALSE)
write.csv(mando_2019c,"CSVs/mando_unico_2019c.csv")

mando_2019d<-read.dbf("2019/Mando_unico_policial_cngmd2019_dbf/Bases_Datos/PERSDEST.DBF", as.is = FALSE)
write.csv(mando_2019d,"CSVs/mando_unico_2019d.csv")


##NIVEL ESTATAL
setwd("~/Dropbox/Dissertation/GovernmentStrategies/Data/ConstructionDatabase/MandoUnico/CensoGobiernoSeguridad/") 

mando_2011<-read.dbf("2014/SP_Mando_unico_pol/Bases_datos/MANDOUNI.DBF", as.is = FALSE)
write.csv(mando_2014,"CSVs/mando_unico_2014.csv")

int_2014<-read.dbf("2014/SP_Mando_unico_pol/Bases_datos/INT_MAND.DBF", as.is = FALSE)
write.csv(int_2014,"CSVs/int_mando_2014.csv")


mando_2015<-read.dbf("2015/Mando_unico_pol/Bases_datos/MANDOUNI.DBF", as.is = FALSE)
write.csv(mando_2015,"CSVs/mando_unico_2015.csv")

int_2015<-read.dbf("2015/Mando_unico_pol/Bases_datos/INT_MAND.DBF", as.is = FALSE)
write.csv(int_2015,"CSVs/int_mando_2015.csv")

rec_2015<-read.dbf("2015/Mando_unico_pol/Bases_datos/RE_HUMAN.DBF", as.is = FALSE)
write.csv(rec_2015,"CSVs/recursos_humanos_mando_2015.csv")

mando_2016<-read.dbf("2016/Mando_unico_pol_cngspspe2016_dbf/Bases_datos/MANDOUNI.DBF", as.is = FALSE)
write.csv(mando_2016,"CSVs/mando_unico_2016.csv")

int_2016<-read.dbf("2016/Mando_unico_pol_cngspspe2016_dbf/Bases_datos/INT_MAND.DBF", as.is = FALSE)
write.csv(int_2016,"CSVs/int_mando_2016.csv")

rec_2016<-read.dbf("2016/Mando_unico_pol_cngspspe2016_dbf/Bases_datos/RE_HUMAN.DBF", as.is = FALSE)
write.csv(rec_2016,"CSVs/recursos_humanos_mando_2016.csv")

mando_2017<-read.dbf("2017/ProbInfra_Resp_MUP_cngspspe2017_dbf/Bases_datos/MANDOUNI.DBF", as.is = FALSE)
write.csv(mando_2017,"CSVs/mando_unico_2017.csv")

int_2017<-read.dbf("2017/ProbInfra_Resp_MUP_cngspspe2017_dbf/Bases_datos/INT_MAND.DBF", as.is = FALSE)
write.csv(int_2017,"CSVs/int_mando_2017.csv")

rec_2017<-read.dbf("2017/ProbInfra_Resp_MUP_cngspspe2017_dbf/Bases_datos/RE_HUMAN.DBF", as.is = FALSE)
write.csv(rec_2017,"CSVs/recursos_humanos_mando_2017.csv")





