source("GetPanelPSID.R")
# In 2003 PSID tries to better capture the variables linked to each of the jobs that 
# a worker holds. Here we collapse pre 2003 data with main job data from 2003 onwards

panel1968_2015 <- as.data.table(panel1968_2015)[,`:=` (Occ3d=ifelse(year %in% c(2003:2017),PresentOrLastMain_3dOccupation_2003_15,PresentMainJob_3dOcc_1968_2001),
                                                       Ind3d=ifelse(year %in% c(2003:2017),PresentOrLastMain_3dIndustry_2003_15,PresentMainJob_3dInd_1968_2001))]
panel1968_2015 <- panel1968_2015[,!colnames(panel1968_2015) %in% c('PresentOrLastMain_3dOccupation_2003_15','PresentMainJob_3dOcc_1968_2001',
                                                                   'PresentOrLastMain_3dIndustry_2003_15','PresentMainJob_3dInd_1968_2001',
                                                                   'PresentMain_3dIndustry_Head_1968_2001','PresentMain_3dIndustry_Spouse_1968_2001',
                                                                   'PresentMain_3dOccupation_Head_1968_2001','PresentMain_3dIndustry_Spouse_1968_2001'), with=FALSE]
# Marital status and region variable data wrangling  
panel1968_2015$Married_Pairs_Indicator_1968_2015 <- ifelse(panel1968_2015$Married_Pairs_Indicator_1968_2015 %in% c(1,2,3),
                                                           1,
                                                           0)
panel1968_2015$Region_1968_2015 <- ifelse(panel1968_2015$Region_1968_2015 %in% c(0,5,6,9),
                                          NA,
                                          panel1968_2015$Region_1968_2015)
# En 2017 hay un cambio extra en los códigos de ocupaciones/industria que
# podemos dejarlos para otra ocasión
panel1968_2015 <- panel1968_2015[!year %in% 2017 ]

# Nos quedamos con los hombres por el tipo de preguntas que tenían anteriormente.
# Me parece que podríamos levantar el tema de race, no? La diferencia en head
# era por gender, no por race. A Chequear
panel1968_2015 <- panel1968_2015[Sex_Individual_1968_2015 %in% 1 &
                                   # race_1968_2015==1 &
                                   relation.head %in% c(1,10)]

# Solo entre 18 y 64 años
panel1968_2015 <- panel1968_2015[Years_Individual_1968_2015>17 & Years_Individual_1968_2015<65]
# Solo continental US
panel1968_2015 <- panel1968_2015[Region_1968_2015 %in% c(1:4)]


# We need to know the experience with employer. It will be useful not only to 
# measure the returns to employer tenure, but also to detect industry and occupation
# changes.
# The variable that captures this experience requires that the worker is working
# for money and that he is not only an independent worker
# There is a problem with employer tenure in the 1978 data. We set it to NA
panel1968_2015 <-panel1968_2015[,YearsWithEmployer_1968_2015:=ifelse(year == 1978,NA,YearsWithEmployer_1968_2015)]
  
# Now that we have the experience, the next thing we have to do is to detect employer change.
# This is easy from 1976 onwards, but difficult before since it was a bracketed question. We
# first replace all reserved values to NA and convert al information on employer tenure to years
panel1968_2015 <- panel1968_2015[,
                                 YearsWithEmployer_1968_2015:=plyr::mapvalues(YearsWithEmployer_1968_2015,0,NA)]
# Between 1968 and 1993, value 999 was reserved and the experience was recorded in months. We
# deal with both problems
panel1968_2015[year %in% c(1976:1993),
               YearsWithEmployer_1968_2015:=plyr::mapvalues(YearsWithEmployer_1968_2015,999,NA)]
panel1968_2015[year %in% c(1976:1993),
              YearsWithEmployer_1968_2015:=YearsWithEmployer_1968_2015/12]
# Between 1994 y 2017  values 98 and 99 were reserved
panel1968_2015[year>=1994,
               YearsWithEmployer_1968_2015:=plyr::mapvalues(YearsWithEmployer_1968_2015,from=c(98,99),to=c(NA,NA))]


# Just to know if there is a problem in employer tenure. There is no data on
# employer or position tenure during 1979 and 1980. There is something wrong in 1978, so we set it to NAs
expEmpRecord<- panel1968_2015[,
               list(promedio=mean(YearsWithEmployer_1968_2015,na.rm=TRUE),
                    mediana=quantile(YearsWithEmployer_1968_2015,0.5, na.rm=TRUE),
                    count=.N), by=list(year)]

ggplot(expEmpRecord[year>1975]) +
  geom_line(aes(x=year,y=promedio))

ggplot(panel1968_2015[!is.na(YearsWithEmployer_1968_2015) & year>1975]) +
  geom_boxplot(aes(x=factor(year),y=YearsWithEmployer_1968_2015))

# Detección de cambios de empleo 
# Primera aparición
panel1968_2015[,firstApp:=min(year),by='pid']
# Esto está basado en el anexo de KM-2008
panel1968_2015[,EMP_CHANGE:=ifelse(year==1968 & YearsWithEmployer_1968_2015==1, TRUE, FALSE)]
panel1968_2015[,EMP_CHANGE:=ifelse(year==1969 & YearsWithEmployer_1968_2015 ==2 & shift(EMP_CHANGE)==TRUE, TRUE,EMP_CHANGE), by='pid']
panel1968_2015[,EMP_CHANGE:=ifelse(year %in% c(1969:1975) & !EMP_CHANGE & YearsWithEmployer_1968_2015 ==1, TRUE,EMP_CHANGE), by='pid']
panel1968_2015[,EMP_CHANGE:=ifelse(year %in% c(1969:1975) & !EMP_CHANGE & YearsWithEmployer_1968_2015 ==2 & firstApp, TRUE,EMP_CHANGE), by='pid']
panel1968_2015[,EMP_CHANGE:=ifelse(year %in% c(1970:1975) & !EMP_CHANGE & YearsWithEmployer_1968_2015 ==2 & !shift(YearsWithEmployer_1968_2015)%in% c(1,2,9), TRUE,EMP_CHANGE), by='pid']
panel1968_2015[,EMP_CHANGE:=ifelse(year == 1976 & !EMP_CHANGE & !YearsWithEmployer_1968_2015 %in% 0 & YearsWithEmployer_1968_2015 <15/12, TRUE,EMP_CHANGE), by='pid']
# Hasta acá, ambas reglas son iguales así que creamos una variable EMP_CHANGE24T
panel1968_2015[,EMP_CHANGE24T:=EMP_CHANGE]
# "Regla general": la de si lo que transcurrió como experiencia
# es menor al tiempo transcurrido entre entrevistas
# Variable auxiliar para identificar cuando hay cambio de años para un individuo
panel1968_2015 <- panel1968_2015[order(pid,year),
                     difYear:=year-shift(year),
                     by="pid"]
panel1968_2015[!is.na(YearsWithEmployer_1968_2015) & !is.na(difYear) & year %in% c(1976:2017),
         EMP_CHANGE:=ifelse(!EMP_CHANGE %in% TRUE & !YearsWithEmployer_1968_2015 %in% 0 & (YearsWithEmployer_1968_2015 < (difYear)), TRUE,EMP_CHANGE),
         by='pid']
# We add 24T employer changes. "24T" detects an employment change when tenure with exployer
# is two years more or two years less that what should be given the period between interviews
# Solo cambiamos la última parte
panel1968_2015 <- panel1968_2015[order(pid,year),
                     `:=` (difYearsWithEmployer=YearsWithEmployer_1968_2015-shift(YearsWithEmployer_1968_2015),
                           lagged_years_with_employer=shift(YearsWithEmployer_1968_2015)),
                     by="pid"]
panel1968_2015[!is.na(YearsWithEmployer_1968_2015) & !is.na(lagged_years_with_employer) & 
           !lagged_years_with_employer %in% 0 & !YearsWithEmployer_1968_2015 %in% 0 &
           year %in% c(1976:2017) & !is.na(difYearsWithEmployer),
         EMP_CHANGE24T:=ifelse((YearsWithEmployer_1968_2015>(shift(YearsWithEmployer_1968_2015)+(difYear+2)) |
                                  YearsWithEmployer_1968_2015<(shift(YearsWithEmployer_1968_2015)+(difYear-2))) & 
                                 !EMP_CHANGE24T %in% TRUE & year %in% c(1977:2017),TRUE,EMP_CHANGE24T), by='pid']
# Si por alguna razón quedó un NA (falta información sobre el año anterior, por ejemplo, o es la primera obs, asumimos que no hubo cambio)
panel1968_2015 <- panel1968_2015[,`:=` (EMP_CHANGE=ifelse(is.na(EMP_CHANGE),FALSE,EMP_CHANGE),
                            EMP_CHANGE24T=ifelse(is.na(EMP_CHANGE24T),FALSE,EMP_CHANGE24T))]
# Employment switch detection
EmpChangeTest<-panel1968_2015[EmploymentStatus %in% 1 ,
                        sum(EMP_CHANGE24T)/.N,
                        list(year)][order(year)]

# it is somewhat high during the first years of the survey, when employer tenure was
# bracketed. After 1977 stabilizes, despite some weird numbers in specific years
library(ggthemes)
ggplot(EmpChangeTest) +
  geom_point(aes(x=year, y=V1)) +
  theme_fivethirtyeight() + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = 'Share of employer change',
       subtitle='Percentage of man working for a new employer as a share of people working.\nPartition T method.')

# Corregimos algunos casos en los cuales EMP_CHANGE24T es FALSE pero EMP_CHANGE
# es TRUE
panel1968_2015$EMP_CHANGE24T <- ifelse(panel1968_2015$EMP_CHANGE %in% TRUE & 
                                         panel1968_2015$EMP_CHANGE24T %in% FALSE,
                                 TRUE,
                                 panel1968_2015$EMP_CHANGE24T)
table(panel1968_2015$EMP_CHANGE,
      panel1968_2015$EMP_CHANGE24T)

# Occ/ind codes van cambiando de clasificador, por lo cual tendríamos un
# muy elevado conteo de falsos. Homogeneizamos primero eso

# We set to NA occ and ind codes that have 0
panel1968_2015 <- panel1968_2015[,`:=` (Occ3d=ifelse(Occ3d == 0, NA, Occ3d),
                                        Ind3d=ifelse(Ind3d == 0, NA, Ind3d))]

# Ordering data
panel1968_2015 <- panel1968_2015[order(pid,rank(year))]
# Occupation and industry codes compatibility #
conversion <- read_excel(path = "Crosswalks.xlsx",sheet = 5)
conversion <- conversion[,c("OCC2010","1970","2000","ACS 2010-","OneDigit")]
colnames(conversion) <- c("OCC2010","CENSUS1970","CENSUS2000","CENSUS2010","OneDigit")
conversion1970_2010 <- unique(conversion[,c("CENSUS1970","CENSUS2010","OCC2010","OneDigit")])
conversion1970_2010 <- conversion1970_2010[!is.na(conversion1970_2010$CENSUS1970),]
conversion2000_2010 <- unique(conversion[,c("CENSUS2000","CENSUS2010","OCC2010","OneDigit")])
conversion2000_2010 <- conversion2000_2010[!is.na(conversion2000_2010$CENSUS2000),]
conversion2000_2010$CENSUS2010 <- as.numeric(conversion2000_2010$CENSUS2010)
conversion2000_2010$CENSUS2000 <- as.numeric(conversion2000_2010$CENSUS2000)
conversion1970_2010 <- data.table(conversion1970_2010)
conversion1970_2010$CENSUS2010 <- as.numeric(conversion1970_2010$CENSUS2010)
conversion1970_2010$CENSUS1970 <- as.numeric(conversion1970_2010$CENSUS1970)
# Hago un join para el periodo 1968-2001 para pasar del CNO1970 al CNO2010
panel1968_2015$Occ1d <- character()
panel1968_2015$Occ3d_OCC2010 <- numeric()
panel1968_2015[year>=1968 & year<=2001 & !is.na(Occ3d)]<-
  panel1968_2015[year>=1968 & year<=2001 & !is.na(Occ3d)][conversion1970_2010,
                                                          on=c(Occ3d="CENSUS1970"),
                                                          `:=`(Occ1d=i.OneDigit,
                                                               Occ3d_OCC2010=CENSUS2010)]
# Hago un join para el periodo 2003-2015 para pasar del CNO2000 al CNO2010
panel1968_2015[year>=2003&year<=2015]<-
  panel1968_2015[year>=2003&year<=2015][data.table(conversion2000_2010),
                                        on=c(Occ3d="CENSUS2000"),
                                        `:=`(Occ1d=i.OneDigit,
                                             Occ3d_OCC2010=CENSUS2010)]
# Codes that could not be matched to the 2010 Census occupational codes
codesNotMatched1970_2010 <- sort(unique(panel1968_2015[year>=1968 & year<=2001 & !is.na(Occ3d) & is.na(Occ3d_OCC2010)]$Occ3d))
codesNotMatched2000_2010 <- sort(unique(panel1968_2015[year>=2003 & year<=2015 & !is.na(Occ3d) & is.na(Occ3d_OCC2010)]$Occ3d))

## ESTA PARTE LA DEJO COMENTADA,PERO NOS VA A SERVIR PARA LOS SOC-CODES DE LA
## O*NET. TENER PRESENTE
## Join para pasar de CNO2010 a SOC2010
# conversorCNOSOC <- as.data.table(read_xls(path = 'OCC2010 to SOC2010.xls'))
# colnames(conversorCNOSOC) <- c('CENSUS2010','SOC2010')
# conversorCNOSOC$CENSUS2010 <- as.numeric(conversorCNOSOC$CENSUS2010)
# conversorCNOSOC <- conversorCNOSOC[complete.cases(conversorCNOSOC)]
# panel1968_2015 <- panel1968_2015[conversorCNOSOC,
#                                  on=c(Occ3d_OCC2010="CENSUS2010"),
#                                  Occ_SOC2010:=i.SOC2010]
# codesNotMatchedCENSUS2010_SOC2010 <- sort(unique(panel1968_2015[year>=1968 & year<=2015 & !is.na(Occ3d_OCC2010) & is.na(Occ_SOC2010)]$Occ3d_OCC2010))

### Compatiblizacion de los codigos de industrias
conversion <- read_excel(path = "Crosswalks.xlsx",sheet = 4)
conversion1970_1990 <- unique(conversion[,c("1970","IND1990",'OneDigit')])
conversion1970_1990<- data.table(conversion1970_1990) %>%
  .[!is.na(`1970`)] %>%
  .[,`1970`:=as.numeric(`1970`)] %>%
  .[,IND1990:=as.numeric(IND1990)]
conversion2000_1990 <- unique(conversion[,c("2000","IND1990",'OneDigit')])
conversion2000_1990<-data.table(conversion2000_1990) %>%
  .[!is.na(`2000`)] %>%
  .[,`2000`:=as.numeric(`2000`)] %>%
  .[!is.na(`2000`)] %>%
  .[,IND1990:=as.numeric(IND1990)]

panel1968_2015$ind3d_IND1990 <- numeric()
panel1968_2015$Ind1d <- character()
# Hago un join para el periodo 1968-2001 para pasar del IND1970 al IND1990
panel1968_2015[year>=1968 & year<=2001]<-
  panel1968_2015[year>=1968 & year<=2001][conversion1970_1990,
                                          on=c(Ind3d="1970"),
                                          `:=`(ind3d_IND1990=IND1990,
                                               Ind1d=i.OneDigit)]
# Hago un join para el periodo 2015-2001 para pasar del IND2000 al IND1990
panel1968_2015[year>=2003&year<=2015]<-
  panel1968_2015[year>=2003&year<=2015][conversion2000_1990,
                                        on=c(Ind3d="2000"),
                                        `:=`(ind3d_IND1990=IND1990,
                                             Ind1d=i.OneDigit)]
panel1968_2015$ind3d_IND1990 <- ifelse(panel1968_2015$ind3d_IND1990 %in% 0,
                                       NA,
                                       panel1968_2015$ind3d_IND1990)


## De acá en adelante ya se puede replicar creo que bastante linealmente lo de
# replica_km. Dos cuidados relevantes: 1) hay que usar occ3d e Ind3d como variables
# que tienen la experiencia en la ocupación y la industria. 2) Desde 2001 en
# adelante la encuesta se hace cada dos años (hay que sumar 2 años de experiencia
# si dice que está trabajando en 2005, por ejemplo). Esto se puede hacer
# con dif-years (tener cuidado de no eliminar observaciones antes)