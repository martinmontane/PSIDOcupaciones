source("GetPanelPSID.R")
# In 2003 PSID tries to better capture the variables linked to each of the jobs that 
# a worker holds. Here we collapse pre 2003 data with main job data from 2003 onwards

panel1968_2015 <- as.data.table(panel1968_2015)[,`:=` (Occ3d=ifelse(year %in% c(2003:2017),PresentOrLastMain_3dOccupation_2003_15,PresentMainJob_3dOcc_1968_2001),
                                                       Ind3d=ifelse(year %in% c(2003:2017),PresentOrLastMain_3dIndustry_2003_15,PresentMainJob_3dInd_1968_2001))]
panel1968_2015 <- panel1968_2015[,!colnames(panel1968_2015) %in% c('PresentOrLastMain_3dOccupation_2003_15',
                                                                   'PresentOrLastMain_3dIndustry_2003_15',
                                                                   'PresentMain_3dIndustry_Head_1968_2001','PresentMain_3dIndustry_Spouse_1968_2001',
                                                                   'PresentMain_3dOccupation_Head_1968_2001','PresentMain_3dIndustry_Spouse_1968_2001'), with=FALSE]

# Calculamos el salario horario real de los trabajadores. Para eso necesitamos
# el CPI
cpi <- fread("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01USA661S&scale=left&cosd=1960-01-01&coed=2019-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2020-07-29&revision_date=2020-07-29&nd=1960-01-01")
cpi <- cpi[,DATE:= as.Date(DATE)]
cpi <- cpi[,year:= year(DATE)]
cpi <- cpi[,base79:=CPALTT01USA661S[year==1979]/CPALTT01USA661S]

panel1968_2015 <- panel1968_2015[cpi,on=c("year"),base79:=base79]
# En 1968 y 1971 la pregunta V337 (HourlyEarnings_AllJobs_1968_2015) usó 99.99 para identificar
# los que no habían tenido ingresos ese año
panel1968_2015[year %in% c(1968,1971) & HourlyEarnings_AllJobs_1968_2015 == 99.99,HourlyEarnings_AllJobs_1968_2015:=0]
panel1968_2015 <- panel1968_2015[,w_real_alljobs_79:=HourlyEarnings_AllJobs_1968_2015*base79]


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

# Acá creamos algunas variables auxiliares para cada individuo para tomar después
# la decisión de mantenerlos o excluirlos en la muestra
panel1968_2015 <- panel1968_2015[,
                                 `:=`(government_worker = any(MainJob_Government_Head_1975_2001 %in% 1),
                                      independent_worker= any(PresentMain_Modalidad_Head_1968_2001 %in% c(2,3)),
                                      wage_less_79_usd_worker =  any(w_real_alljobs_79>0 & w_real_alljobs_79<1)),
                                 by=list(pid)]

# Paso 5: eliminar a todas las personas que cumplen las siguientes condiciones
# a) trabajan en el gobierno
# b) trabajaron como independientes
# c) tuvieron ingresos laborales por hora menores a un dólar (dolares 1979)
# d) fueron farmers/trabajaron en el army
panel1968_2015 <- panel1968_2015[!government_worker %in% TRUE & 
                                   !independent_worker %in% TRUE &
                                   !wage_less_79_usd_worker %in% TRUE]

# Paso 6: identificamos las observaciones que trabajaron menos de 500 horas
panel1968_2015 <- panel1968_2015[,menos_500horas := ifelse(TodosEmpleos_YearHrs_Head_1968_2015<=500,TRUE,FALSE)]

# Paso 7: identificamos las observaciones que o no trabajaron o tuvieron ingreso cero
panel1968_2015 <- panel1968_2015[,sin_ingresos := ifelse(wage_less_79_usd_worker %in% 0,TRUE,FALSE)]

# Eliminamos a los que trabajan menos de 500 horas y a los que no tienen ingresos
panel1968_2015 <- panel1968_2015[!menos_500horas %in% TRUE & 
                                   !sin_ingresos %in% TRUE]

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

# Ahora que ya tenemos los codigos de Occ homogeneizados sacamos a quienes trabajan en farmy o army
panel1968_2015 <- panel1968_2015[, army_farmer_worker := any(Occ3d %in% c(9840, 6005, 6020, 6040, 6050)), by=list(pid)]

panel1968_2015 <- panel1968_2015[!army_farmer_worker %in% TRUE]

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
# que tienen la experiencia en la ocupación y la industria. 2) Desde 1997 en
# adelante la encuesta se hace cada dos años (hay que sumar 2 años de experiencia
# si dice que está trabajando en 2005, por ejemplo). Esto se puede hacer
# con dif-years (tener cuidado de no eliminar observaciones antes)

# We find changes in occupation and industry based on EMP_CHANGE and EMP_CHANGE24T
# Only changes in reported activity or occupation
# _UNC : uncontrolled. Without employer change detection
# _EMP : New employer
# _EMP24T : New employer with 24T detection
panel1968_2015 <- panel1968_2015[order(pid,year)]
panel1968_2015 <- panel1968_2015[!is.na(Ind3d),
                     `:=`(IND_CHANGE_UNC=ifelse(!Ind3d == shift(Ind3d,type='lag'),TRUE,FALSE)),
                     by=c('pid')]
panel1968_2015 <- panel1968_2015[!is.na(Occ3d),
                     `:=`(OCC_CHANGE_UNC=ifelse(!Occ3d == shift(Occ3d),TRUE,FALSE)),
                     by=c('pid')]
panel1968_2015 <- panel1968_2015[,`:=` (IND_CHANGE_UNC=ifelse(is.na(IND_CHANGE_UNC),FALSE,IND_CHANGE_UNC),
                            OCC_CHANGE_UNC=ifelse(is.na(OCC_CHANGE_UNC),FALSE,OCC_CHANGE_UNC))]

panel1968_2015 <- panel1968_2015[,`:=` (IND_CHANGE_EMP=ifelse(IND_CHANGE_UNC & EMP_CHANGE,TRUE,FALSE),
                            OCC_CHANGE_EMP=ifelse(OCC_CHANGE_UNC & EMP_CHANGE,TRUE,FALSE),
                            IND_CHANGE_EMP24T=ifelse(IND_CHANGE_UNC & EMP_CHANGE24T,TRUE,FALSE),
                            OCC_CHANGE_EMP24T=ifelse(OCC_CHANGE_UNC & EMP_CHANGE24T,TRUE,FALSE))]
# Checking act and occ switches
grafico<-melt(panel1968_2015[EmploymentStatus == 1,
                       list(IND_CHANGE_UNC=sum(IND_CHANGE_UNC,na.rm = TRUE)/.N,
                            IND_CHANGE_EMP=sum(IND_CHANGE_EMP,na.rm = TRUE)/.N,
                            IND_CHANGE_EMP24T=sum(IND_CHANGE_EMP24T,na.rm = TRUE)/.N),
                       list(year)][order(year)],id.vars = 'year')

ggplot(grafico) + 
  geom_line(aes(x=year, y=value,group=variable, color=variable)) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = 'Share of employer change',
       subtitle='Percentage of man working for a new employer as a share of people working.\nPartition UNC/T/T24 method.')


# They closely match between 1968 and 1978, as K&M say in their paper. We will
# assign occupational and industry switches with the uncontrolled version prior to 1980.

panel1968_2015 <- panel1968_2015[,`:=` (IND_CHANGE_EMP=ifelse(year %in% c(1968:1980),IND_CHANGE_UNC,IND_CHANGE_EMP),
                            OCC_CHANGE_EMP=ifelse(year %in% c(1968:1980),OCC_CHANGE_UNC,OCC_CHANGE_EMP),
                            IND_CHANGE_EMP24T=ifelse(year %in% c(1968:1980),IND_CHANGE_UNC,IND_CHANGE_EMP24T),
                            OCC_CHANGE_EMP24T=ifelse(year %in% c(1968:1980),OCC_CHANGE_UNC,OCC_CHANGE_EMP24T))]


grafico<-melt(panel1968_2015[EmploymentStatus == 1 & Sex_Individual_1968_2015 == 1,
                       list(IND_CHANGE_UNC=sum(IND_CHANGE_UNC,na.rm = TRUE)/.N,
                            IND_CHANGE_EMP=sum(IND_CHANGE_EMP,na.rm = TRUE)/.N,
                            IND_CHANGE_EMP24T=sum(IND_CHANGE_EMP24T,na.rm = TRUE)/.N),
                       list(year)][order(year)],id.vars = 'year')
ggplot(grafico[!variable %in% 'IND_CHANGE_UNC' & !year %in% c(1968,2017)] ) + 
  geom_line(aes(x=year, y=value,group=variable, color=variable)) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = 'Share of employer change',
       subtitle='Percentage of man working for a new employer as a share of people working.\nPartition T method.')

# Now that we have the breaks, lets build the the spells.
# First, we indicate the first observation of the first period in which a person
# was employed
panel1968_2015 <- panel1968_2015[,ever_employed:=ifelse(any(EmploymentStatus %in% 1), TRUE, FALSE),by="pid"]
panel1968_2015 <- panel1968_2015[ever_employed==TRUE]
panel1968_2015$ever_employed <- NULL
panel1968_2015 <-panel1968_2015[year < 1998, `:=`(EMPLOYER_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1]),1,0),
                           EMPLOYER_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1]),1,0)),
                    by='pid']
panel1968_2015 <-panel1968_2015[year > 1998, `:=`(EMPLOYER_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1]),2,0),
                                       EMPLOYER_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1]),2,0)),
                                by='pid']
# Convertimos a 1 en la variable de los spell cuando hubo un cambio, con excepción
# del primer trabajo
panel1968_2015 <- panel1968_2015[EMPLOYER_SPELL %in% 0, EMPLOYER_SPELL:=as.numeric(EMP_CHANGE)]
panel1968_2015 <- panel1968_2015[EMPLOYER_SPELL24T %in% 0, EMPLOYER_SPELL:=as.numeric(EMP_CHANGE24T)]

# Ahora ya podemos usar cumsum para tener el spell
panel1968_2015 <- panel1968_2015[,
                     EMPLOYER_SPELL:= cumsum(EMPLOYER_SPELL),
                     by = "pid"]
panel1968_2015 <- panel1968_2015[,
                     EMPLOYER_SPELL24T:= cumsum(EMPLOYER_SPELL24T),
                     by = "pid"]

# Quedan algunos NAs en los spells, que corresponden simplemente a algunos trabajadores
# que no estaban ocupados cuando entraron a la muestra. Vamos a marcar a ellos y a
# todos los spells sin ninguna observación con employmentstatus 1 como NAs

panel1968_2015 <- panel1968_2015[,EMPLOYER_SPELL:=ifelse(!any(EmploymentStatus %in% 1),NA,EMPLOYER_SPELL)
                     ,list(pid,EMPLOYER_SPELL)]

panel1968_2015 <- panel1968_2015[,EMPLOYER_SPELL24T:=ifelse(!any(EmploymentStatus %in% 1),NA,EMPLOYER_SPELL24T)
                     ,list(pid,EMPLOYER_SPELL24T)]

# Summary of employment spells
table(panel1968_2015[,list(min=min(EMPLOYER_SPELL),
                     max=max(EMPLOYER_SPELL)),by='pid']$max)
panel1968_2015$EXP_EMP <- panel1968_2015$EXP_EMP24T <- as.double()
# Remove all data before first tenure with employer data
panel1968_2015 <- panel1968_2015[,firstTenureYear:=min(year[!is.na(YearsWithEmployer_1968_2015)]), by=c('pid')]
panel1968_2015 <- panel1968_2015[!firstTenureYear %in% 'Inf']
panel1968_2015 <- panel1968_2015[!year<firstTenureYear]


# Builds occ and ind spells
panel1968_2015 <- panel1968_2015[year < 1998,
                                 `:=`(OCC_SPELL=ifelse(OCC_CHANGE_EMP,1,0),
                                      IND_SPELL=ifelse(IND_CHANGE_EMP,1,0),
                                      OCC_SPELL24T=ifelse(OCC_CHANGE_EMP24T,1,0),
                                      IND_SPELL24T=ifelse(IND_CHANGE_EMP24T,1,0))]
panel1968_2015 <- panel1968_2015[year > 1998,
                                 `:=`(OCC_SPELL=ifelse(OCC_CHANGE_EMP,2,0),
                                      IND_SPELL=ifelse(IND_CHANGE_EMP,2,0),
                                      OCC_SPELL24T=ifelse(OCC_CHANGE_EMP24T,2,0),
                                      IND_SPELL24T=ifelse(IND_CHANGE_EMP24T,2,0))]
panel1968_2015 <- panel1968_2015[,`:=`(OCC_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(Occ3d)]),1,OCC_SPELL),
                           IND_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(Ind3d)]),1,IND_SPELL),
                           OCC_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(Occ3d)]),1,OCC_SPELL24T),
                           IND_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(Ind3d)]),1,IND_SPELL24T)),
                     by='pid']
panel1968_2015 <- panel1968_2015[,
                     `:=`(OCC_SPELL=ifelse(is.na(OCC_SPELL),0,OCC_SPELL),
                          IND_SPELL=ifelse(is.na(IND_SPELL),0,IND_SPELL),
                          OCC_SPELL24T=ifelse(is.na(OCC_SPELL24T),0,OCC_SPELL24T),
                          IND_SPELL24T=ifelse(is.na(IND_SPELL24T),0,IND_SPELL24T)),
                     by='pid']
panel1968_2015 <- panel1968_2015[,
                     `:=`(OCC_SPELL=cumsum(.SD$OCC_SPELL),
                          IND_SPELL=cumsum(.SD$IND_SPELL),
                          OCC_SPELL24T=cumsum(.SD$OCC_SPELL24T),
                          IND_SPELL24T=cumsum(.SD$IND_SPELL24T)),
                     by='pid']
panel1968_2015 <- panel1968_2015[,
                     `:=`(OCC_SPELL=ifelse(OCC_SPELL %in% 0,NA,OCC_SPELL),
                          IND_SPELL=ifelse(IND_SPELL %in% 0,NA,IND_SPELL),
                          OCC_SPELL24T=ifelse(OCC_SPELL24T %in% 0,NA,OCC_SPELL24T),
                          IND_SPELL24T=ifelse(IND_SPELL24T %in% 0,NA,IND_SPELL24T)),
                     by='pid']

# Before estimating the experience, we need to impute the most frequent occupation
# and industry codes for their specific spells
panel1968_2015 <- panel1968_2015[,
                     MostFreqOCC:=names(sort(table(Occ3d)))[1],
                     by=c('pid','OCC_SPELL')]
panel1968_2015 <- panel1968_2015[,
                     MostFreqOCC24T:=names(sort(table(Occ3d)))[1],
                     by=c('pid','OCC_SPELL24T')]
panel1968_2015 <- panel1968_2015[,
                     MostFreqIND:=names(sort(table(Ind3d)))[1],
                     by=c('pid','IND_SPELL')]
panel1968_2015 <- panel1968_2015[,
                     MostFreqIND24T:=names(sort(table(Ind3d)))[1],
                     by=c('pid','IND_SPELL24T')]
# Data wrangling in WORK EXPERIENCE
panel1968_2015 <- panel1968_2015[,
                     WorkExpSince18:=plyr::mapvalues(WorkExpSince18,
                                                     from=c(0,99),
                                                     to=c(NA,NA))]


# En el A.2 del paper, para employer tenure, saco la siguiente conclusión:
# 1) Para cada spell de empleador, asignar el valor mínimo cuando arranca
panel1968_2015 <- panel1968_2015[!is.na(YearsWithEmployer_1968_2015),
                     EXP_EMP:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid','EMPLOYER_SPELL')]
panel1968_2015 <- panel1968_2015[!is.na(YearsWithEmployer_1968_2015),
                     EXP_EMP24T:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid','EMPLOYER_SPELL24T')]
# 2) Resetear la experiencia con el empleador en 1981 
panel1968_2015 <- panel1968_2015[year %in% 1981,
                     EXP_EMP:=ifelse(!is.na(YearsWithEmployer_1968_2015),YearsWithEmployer_1968_2015,EXP_EMP)]
panel1968_2015 <- panel1968_2015[year %in% 1981,
                     EXP_EMP24T:=ifelse(!is.na(YearsWithEmployer_1968_2015),YearsWithEmployer_1968_2015,EXP_EMP24T)]
# 3) Para cada año entre 1969-1978 y 1982-97 agregarle 1 de experiencia o para 1999-2015
#    agregarle 2 de experiencia, si trabajó 800 horas 
panel1968_2015 <- panel1968_2015[,
                     EXP_EMP:=ifelse(EmploymentStatus %in% 1 &
                                       TodosEmpleos_YearHrs_Head_1968_2015 >= 800 &
                                       year %in% c(1969:1978,1982:1993) & is.na(EXP_EMP),
                                     year-shift(year),EXP_EMP),
                     list(pid)]
panel1968_2015 <- panel1968_2015[is.na(EXP_EMP),EXP_EMP:=0]

panel1968_2015 <- panel1968_2015[,
                     EXP_EMP24T:=ifelse(EmploymentStatus %in% 1 &
                                          TodosEmpleos_YearHrs_Head_1968_2015 >= 800 &
                                          year %in% c(1969:1978,1982:1993) & is.na(EXP_EMP24T),
                                        year-shift(year),EXP_EMP24T),
                     list(pid)]
panel1968_2015 <- panel1968_2015[is.na(EXP_EMP24T),EXP_EMP24T:=0]

panel1968_2015 <- panel1968_2015[,
                     EXP_EMP:=cumsum(EXP_EMP),by=c('pid','EMPLOYER_SPELL')]
panel1968_2015 <- panel1968_2015[,
                     EXP_EMP24T:=cumsum(EXP_EMP24T),by=c('pid','EMPLOYER_SPELL24T')]


#### Occupation and industry experience ####
# Paso 1) Identificar cambios de occupación e industria basado en EMPLOYER_CHANGE
# y EMPLOYER_CHANGE24T. Ya está hecho
# Paso 2) Cualquier persona que entre desde 1968 en adelante le asignamos
# la experiencia con el empleador. IMPORTANTE: también usan la experiencia
# en la posición solo si no hay info con experiencia con el empleador
panel1968_2015 <- panel1968_2015[,
                     OCC_EXP:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid')]
panel1968_2015 <- panel1968_2015[,
                     OCC_EXP24T:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid')]
panel1968_2015 <- panel1968_2015[,
                     IND_EXP:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid')]
panel1968_2015 <- panel1968_2015[,
                     IND_EXP24T:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid')]
# Paso 3) Se agrega uno a la experiencia en ocupacion/industria si en la próxima
# observación no hay un cambio de ocupación/industria, se encuentra trabajando
# en la próxima observación y trabajó 800 horas o más en el período
panel1968_2015 <- panel1968_2015[,
                     OCC_EXP:=ifelse(is.na(OCC_EXP) & !year %in% min(year) & 
                                       TodosEmpleos_YearHrs_Head_1968_2015 > 800 & 
                                       shift(EmploymentStatus,type = "lead") %in% 1 &
                                       (OCC_SPELL - shift(OCC_SPELL,type="lead")) %in% 0,year-shift(year),
                                     ifelse(year %in% min(year),OCC_EXP,0)),
                     by=c('pid')]

panel1968_2015 <- panel1968_2015[,
                     OCC_EXP24T:=ifelse(is.na(OCC_EXP24T) & !year %in% min(year) & 
                                          TodosEmpleos_YearHrs_Head_1968_2015 > 800 & 
                                          shift(EmploymentStatus,type = "lead") %in% 1 &
                                          (OCC_SPELL24T - shift(OCC_SPELL24T,type="lead")) %in% 0,year-shift(year),
                                        ifelse(year %in% min(year),OCC_EXP24T,0)),
                     by=c('pid')]

panel1968_2015 <- panel1968_2015[,
                     IND_EXP:=ifelse(is.na(IND_EXP) & !year %in% min(year) & 
                                       TodosEmpleos_YearHrs_Head_1968_2015 > 800 & 
                                       shift(EmploymentStatus,type = "lead") %in% 1 &
                                       (IND_SPELL - shift(IND_SPELL,type="lead")) %in% 0,year-shift(year),
                                     ifelse(year %in% min(year),IND_EXP,0)),
                     by=c('pid')]

panel1968_2015 <- panel1968_2015[,
                     IND_EXP24T:=ifelse(is.na(IND_EXP24T) & !year %in% min(year) & 
                                          TodosEmpleos_YearHrs_Head_1968_2015 > 800 & 
                                          shift(EmploymentStatus,type = "lead") %in% 1 &
                                          (IND_SPELL24T - shift(IND_SPELL24T,type="lead")) %in% 0,year-shift(year),
                                        ifelse(year %in% min(year),IND_EXP24T,0)),
                     by=c('pid')]

# Paso 4: Esto no me queda muy claro, dice que si cambias de ocupación / industria la experiencia
# vuelve a 6 meses. Yo lo hago acá, solo a fines de replicar, pero asume una tasa de depreciación
# del 100% de lo que hiciste en el pasado... a tener en cuenta para ver si cambian los resultados
panel1968_2015 <- panel1968_2015[OCC_SPELL>1,
                     OCC_EXP:=ifelse(year == min(year),0.5,OCC_EXP),
                     by=c('pid',"OCC_SPELL")]
panel1968_2015 <- panel1968_2015[OCC_SPELL24T>1,
                     OCC_EXP24T:=ifelse(year == min(year),0.5,OCC_EXP24T),
                     by=c('pid',"OCC_SPELL24T")]
panel1968_2015 <- panel1968_2015[IND_SPELL>1,
                     IND_EXP:=ifelse(year == min(year),0.5,IND_EXP),
                     by=c('pid',"IND_SPELL")]
panel1968_2015 <- panel1968_2015[IND_SPELL24T>1,
                     IND_EXP24T:=ifelse(year == min(year),0.5,IND_EXP24T),
                     by=c('pid',"IND_SPELL24T")]

# Paso 5: acumulamos la experiencia en industria/ocupacion

panel1968_2015 <- panel1968_2015[,OCC_EXP:=cumsum(OCC_EXP), by=c('pid',"OCC_SPELL")]
panel1968_2015 <- panel1968_2015[,OCC_EXP24T:=cumsum(OCC_EXP24T), by=c('pid',"OCC_SPELL24T")]
panel1968_2015 <- panel1968_2015[,IND_EXP:=cumsum(IND_EXP), by=c('pid',"IND_SPELL")]
panel1968_2015 <- panel1968_2015[,IND_EXP24T:=cumsum(IND_EXP24T), by=c('pid',"IND_SPELL24T")]


#### Otras variables: experiencia laboral ####
# El primer año que tenga experiencia laboral total (generalmente, 1974),
# se lo asignamos
panel1968_2015 <- panel1968_2015[!is.na(WorkExpSince18),
                     `:=`(EXP_WORK=ifelse(year==min(year),WorkExpSince18,NA),
                          YEAR_MIN_EXP_WORK=min(year)),
                     by='pid']
# Después, podemos agregar 1 si se cumplen los criterios de siempre
panel1968_2015[year > 1973,
         EXP_WORK:= ifelse(!is.na(EXP_WORK),EXP_WORK,
                           ifelse(EmploymentStatus %in% 1 &
                                    TodosEmpleos_YearHrs_Head_1968_2015 >= 800 &
                                    year > YEAR_MIN_EXP_WORK, year-shift(year),0)),
         by='pid']
panel1968_2015[year > 1973,
         EXP_WORK:=cumsum(EXP_WORK),
         by='pid']

# Controls that might be useful for the regression
# Regional dummy
panel1968_2015$Region_1968_2015 <- factor(panel1968_2015$Region_1968_2015)
# OJ dummy: 1 if the worker is NOT working in the first year with a new employer
panel1968_2015 <- panel1968_2015[, OJ:= ifelse(year == min(year), 0, 1),by=c('pid','EMPLOYER_SPELL')]
panel1968_2015 <- panel1968_2015[, OJ24T:= ifelse(year == min(year), 0, 1),by=c('pid','EMPLOYER_SPELL24T')]

# Acá habría que encontrar los clasificadores de indistrias y ocupaciones
# para crear dos variables que tengan a un dígito la industria y ocupación
# Convierto a factor la variable OneDigitOcc para la regresión
# panel1968_2015$OneDigitOcc <- factor(panel1968_2015$Occ1d)
# panel1968_2015$OneDigitInd <- factor(panel1968_2015$Ind1d)

# Checking mincer style equations
mincerTest <- lm(panel1968_2015[year %in% c(1981:1993) & w_real_alljobs_79>0],
                 formula=log(w_real_alljobs_79) ~ Grades_Individual_1968_2015  + WorkExpSince18 + OCC_EXP24T  + IND_EXP24T + EXP_EMP24T + OJ24T + factor(year) + Region_1968_2015 )
summary(mincerTest)

### Otras variables ###
panel1968_2015 <- panel1968_2015[,
                     `:=`(EMP2_100 = EXP_EMP^2 * 100,
                          OCC2_100 = OCC_EXP^2 * 100,
                          OCC3_100 = OCC_EXP^3 * 100,
                          IND2_100 = IND_EXP^2 * 100,
                          IND3_100 = IND_EXP^3 * 100,
                          WORK2    = WorkExpSince18^2,
                          WORK3_100= WorkExpSince18^3 * 100)]

panel1968_2015 <- panel1968_2015[,
                     `:=`(EMP2_100_24T = EXP_EMP24T^2 * 100,
                          OCC2_100_24T = OCC_EXP24T^2 * 100,
                          OCC3_100_24T = OCC_EXP24T^3 * 100,
                          IND2_100_24T = IND_EXP24T^2 * 100,
                          IND3_100_24T = IND_EXP24T^3 * 100)]

#### INSTRUMENTOS ####

# Instruments
panel1968_2015[,
         `:=`(INSTR_EMP=EXP_EMP-mean(EXP_EMP),
              INSTR_EMP2_100=((EXP_EMP^2)-(mean(EXP_EMP)^2))*100),
         by=c('pid','EMPLOYER_SPELL')]

panel1968_2015[,
         `:=`(INSTR_EMP24T=EXP_EMP24T-mean(EXP_EMP24T),
              INSTR_EMP2_100_24T=((EXP_EMP24T^2)-(mean(EXP_EMP24T)^2))*100),
         by=c('pid','EMPLOYER_SPELL24T')]
panel1968_2015[,
         `:=`(INSTR_OCC=OCC_EXP-mean(OCC_EXP),
              INSTR_OCC2_100=((OCC_EXP^2)-(mean(OCC_EXP)^2))*100,
              INSTR_OCC3_100=((OCC_EXP^3)-(mean(OCC_EXP)^3))*100),
         by=c('pid','OCC_SPELL')]
panel1968_2015[,
         `:=`(INSTR_OCC24T=OCC_EXP24T-mean(OCC_EXP24T),
              INSTR_OCC2_100_24T=((OCC_EXP24T^2)-(mean(OCC_EXP24T)^2))*100,
              INSTR_OCC3_100_24T=((OCC_EXP24T^3)-(mean(OCC_EXP24T)^3))*100),
         by=c('pid','OCC_SPELL24T')]
panel1968_2015[,
         `:=`(INSTR_IND=IND_EXP-mean(IND_EXP),
              INSTR_IND2_100=((IND_EXP^2)-(mean(IND_EXP)^2))*100,
              INSTR_IND3_100=((IND_EXP^3)-(mean(IND_EXP)^3))*100),
         by=c('pid','IND_SPELL')]
panel1968_2015[,
         `:=`(INSTR_IND24T=IND_EXP24T-mean(IND_EXP24T),
              INSTR_IND2_100_24T=((IND_EXP24T^2)-(mean(IND_EXP24T)^2))*100,
              INSTR_IND3_100_24T=((IND_EXP24T^3)-(mean(IND_EXP24T)^3))*100),
         by=c('pid','IND_SPELL24T')]
panel1968_2015[,
         `:=`(INSTR_WORK=EXP_WORK-mean(EXP_WORK),
              INSTR_WORK2=(EXP_WORK^2)-(mean(EXP_WORK)^2),
              INSTR_WORK3_100=((EXP_WORK^3)-(mean(EXP_WORK)^3))*100),
         by='pid']
panel1968_2015[,
         INSTR_OJ:=  OJ-mean(OJ),
         by=c('pid','EMPLOYER_SPELL')]
panel1968_2015[,
         INSTR_OJ24T:=  OJ24T-mean(OJ24T),
         by=c('pid','EMPLOYER_SPELL24T')]

### Hacemos el crossover entre el sistema de tres digitos y los de dos y un digito, segun Appendix B del paper

Occ_codes <- fread("occ_codes.csv")
panel1968_2015 <- panel1968_2015[Occ_codes, on=.(Occ3d = OCC_3D)]
Ind_codes <- fread("ind_codes.csv")
panel1968_2015 <- panel1968_2015[Ind_codes, on=.(Ind3d = IND_3d)]

# Checking mincer style equations
mincerTest <- lm(data = panel1968_2015[year %in% c(1981:1993) & w_real_alljobs_79>0],
                 formula = log(w_real_alljobs_79) ~ Grades_Individual_1968_2015  + EXP_WORK +
                   OCC_EXP  +
                   IND_EXP +
                   EXP_EMP +
                   OJ +
                   factor(year) +
                   Region_1968_2015 +
                   Married_Pairs_Indicator_1968_2015)
summary(mincerTest)
library(nlme)
mincerTest <- gls(data = panel1968_2015[year %in% c(1981:1993) & w_real_alljobs_79>0],na.action = "na.omit",
                  model = log(w_real_alljobs_79) ~ Grades_Individual_1968_2015  + INSTR_WORK + INSTR_OCC  + INSTR_IND + INSTR_EMP + OJ + factor(year) + Region_1968_2015 + Married_Pairs_Indicator_1968_2015)
summary(mincerTest)

# Tres o mas observaciones 'confiebles'
panel1968_2015 <- panel1968_2015[, reliable := ifelse(EmploymentStatus %in% 1, 1, 0)]
panel1968_2015 <- panel1968_2015[, reliable := ifelse(reliable==1 & shift(reliable)==1 & shift(reliable, type = "lead"), 1, 0), by='pid']
panel1968_2015 <- panel1968_2015[, reliable := any(reliable %in% 1), by='pid']
# panel1968_2015 <- panel1968_2015[reliable %in% TRUE]

# Si entraron a partir del 80, tomamos a partir del segundo spell
panel1968_2015 <- panel1968_2015[,
                     first_spell_1981 := firstApp > 1980 & OCC_SPELL < 2 & IND_SPELL < 2]


mincerTest <- gls(data = panel1968_2015[year %in% c(1981:1993) & w_real_alljobs_79>0 & first_spell_1981 == FALSE],na.action = "na.omit",
                  model = log(w_real_alljobs_79) ~ Grades_Individual_1968_2015  + INSTR_WORK + INSTR_OCC  + INSTR_IND + INSTR_EMP + OJ + factor(year) + Region_1968_2015 + Married_Pairs_Indicator_1968_2015)
summary(mincerTest)


### Table 1 - DESCRIPTIVE STATISTICS ###
# Años de interes
tabla_1 <- panel1968_2015

# Tres o mas observaciones 'confiebles'
tabla_1 <- tabla_1[, reliable := ifelse(EmploymentStatus == 1, 1, 0)]
tabla_1 <- tabla_1[, reliable := ifelse(reliable==1 & shift(reliable)==1 & shift(reliable, type = "lead"), 1, 0), by='pid']
tabla_1 <- tabla_1[, reliable := any(reliable %in% 1), by='pid']
tabla_1 <- tabla_1[reliable %in% TRUE]

# Si entraron a partir del 80, tomamos a partir del segundo spell
tabla_1 <- tabla_1[, second_spell := firstApp > 1980 & OCC_SPELL < 2 & IND_SPELL < 2]
# Esto por ahi es mejor hacerlo con 24T:
## tabla_1 <- tabla_1[, second_spell := firstApp > 1980 & OCC_SPELL24T < 2 & IND_SPEL24TL < 2]
tabla_1 <- tabla_1[second_spell == FALSE]

# Acomodamos la dummy para trabajadores cubiertos por un sindicato
tabla_1 <- tabla_1[, sindicato := ifelse(TrabajoCubiertoSindicato_1979_2015 == 1, 1, 0)]

# Acomodamos la dummy de casados
tabla_1 <- tabla_1[, casados := ifelse(Married_Pairs_Indicator_1968_2015 == 0, 0, 1)]

# Nos quedamos solo con las variables que nos interesan para la Tabla
tabla_1 <- tabla_1[, .(Years_Individual_1968_2015, Grades_Individual_1968_2015, casados, sindicato, 
                       WorkExpSince18, EXP_EMP, OCC_EXP, OCC_EXP24T, IND_EXP, IND_EXP24T)]

# Armamos la Tabla
library(stargazer)
stargazer(tabla_1, title = 'DESCRIPTIVE STATISTICS', out = "./table_1_full_sample.tex",
          min.max = FALSE, style = 'aer')

