# Este script intenta replicar Kambourov & Manovskii (2008)
source("GetPanelPSID.R")
# Paso 1: Observaciones entre 1968 y 1993
panel_km <- panel1968_2015[year %in% 1968:1993]
# Paso 2: Hombres, blancos y cabezas de hogar
panel_km <- panel_km[Sex_Individual_1968_2015==1 & race_1968_2015==1 & relation.head %in% c(1,10)]
# Paso 3: entre 18 y 64 años
panel_km <- panel_km[Years_Individual_1968_2015>17 & Years_Individual_1968_2015<65]
# Paso 4: continental europe
panel_km <- panel_km[Region_1968_2015 %in% c(1:4)]

# Calculamos el salario horario real de los trabajadores. Para eso necesitamos
# el CPI
cpi <- fread("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPALTT01USA661S&scale=left&cosd=1960-01-01&coed=2019-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=lin&vintage_date=2020-07-29&revision_date=2020-07-29&nd=1960-01-01")
cpi <- cpi[,DATE:= as.Date(DATE)]
cpi <- cpi[,year:= year(DATE)]
cpi <- cpi[,base79:=CPALTT01USA661S[year==1979]/CPALTT01USA661S]

panel_km <- panel_km[cpi,on=c("year"),base79:=base79]
# En 1968 y 1971 la pregunta V337 (HourlyEarnings_AllJobs_1968_2015) usó 99.99 para identificar
# los que no habían tenido ingresos ese año
panel_km[year %in% c(1968,1971) & HourlyEarnings_AllJobs_1968_2015 == 99.99,HourlyEarnings_AllJobs_1968_2015:=0]
panel_km <- panel_km[,w_real_alljobs_79:=HourlyEarnings_AllJobs_1968_2015*base79]

# Acá creamos algunas variables auxiliares para cada individuo para tomar después
# la decisión de mantenerlos o excluirlos en la muestra
# No pude encontrar ocupación / industria que me diga si trabajó en el ejército...
# a revisar al final si los números no dan
# Agregue la ocupacion de army y cambie industria por ocupacion en army_farmer_worker
panel_km <- panel_km[,
                     `:=`(government_worker = any(MainJob_Government_Head_1975_2001 %in% 1),
                          independent_worker= any(PresentMain_Modalidad_Head_1968_2001 %in% c(2,3)),
                          wage_less_79_usd_worker =  any(w_real_alljobs_79>0 & w_real_alljobs_79<1),
                          army_farmer_worker=any(PresentMain_3dOccupation_Head_1968_2001 %in% c(580, 590, 800:824))),
                     by=list(pid)]

# Paso 5: eliminar a todas las personas que cumplen las siguientes condiciones
# a) trabajan en el gobierno
# b) trabajaron como independientes
# c) tuvieron ingresos laborales por hora menores a un dólar (dolares 1979)
# d) fueron farmers/trabajaron en el army
panel_km <- panel_km[!government_worker %in% TRUE & 
                    !independent_worker %in% TRUE &
                    !wage_less_79_usd_worker %in% TRUE &
                    !army_farmer_worker %in% TRUE]

# Paso 6: identificamos las observaciones que trabajaron menos de 500 horas
panel_km <- panel_km[,menos_500horas := ifelse(TodosEmpleos_YearHrs_Head_1968_2015<=500,TRUE,FALSE)]

# Paso 7: identificamos las observaciones que o no trabajaron o tuvieron ingreso cero
panel_km <- panel_km[,sin_ingresos := ifelse(wage_less_79_usd_worker %in% 0,TRUE,FALSE)]

# Paso 8: identificar cambio de empleo. Esta parte implica varios pasos. De acá
# en adelante algunos pasos nos van a servir sólo para replicar el ejercicio
# hasta el 2015. Enfoquémonos en el período de los autores para ver si 
# llegamos a algo parecido.

# We find changes in occupation and industry based on EMP_CHANGE and EMP_CHANGE24T
# Only changes in reported activity or occupation
# _UNC : uncontrolled. Without employer change detection
# _EMP : New employer
# _EMP24T : New employer with 24T detection

# We need to know the experience with employer. It will be useful not only to 
# measure the returns to employer tenure, but also to detect industry and occupation
# changes.
# The variable that captures this experience requires that the worker is working
# for money and that he is not only an independent worker
# There is a problem with employer tenure in the 1978 data. We set it to NA
panel_km <-panel_km[,
                    YearsWithEmployer_1968_2015:=ifelse(year == 1978,
                                                        NA,
                                                        YearsWithEmployer_1968_2015)]

# Now that we have the experience, the next thing we have to do is to detect employer change.
# This is easy from 1976 onwards, but difficult before since it was a bracketed question. We
# first replace all reserved values to NA and convert all information on employer tenure to years
panel_km <- panel_km[,
                     YearsWithEmployer_1968_2015:=plyr::mapvalues(YearsWithEmployer_1968_2015,0,NA)]
# Between 1968 and 1993, value 999 was reserved and the experience was recorded in months. We
# deal with both problems
panel_km[year %in% c(1976:1993),
               YearsWithEmployer_1968_2015:=plyr::mapvalues(YearsWithEmployer_1968_2015,999,NA)]
panel_km[year %in% c(1976:1993),
               YearsWithEmployer_1968_2015:=YearsWithEmployer_1968_2015/12]
# Between 1994 y 2017 values 98 and 99 were reserved
panel_km[year>=1994,
               YearsWithEmployer_1968_2015:=plyr::mapvalues(YearsWithEmployer_1968_2015,from=c(98,99),to=c(NA,NA))]
# Just to know if there is a problem in employer tenure. There is no data on
# employer or position tenure during 1979 and 1980. There is something wrong in 1978, so we set it to NAs
expEmpRecord<- panel_km[,
                              list(promedio=mean(YearsWithEmployer_1968_2015,na.rm=TRUE),
                                   mediana=quantile(YearsWithEmployer_1968_2015,0.5, na.rm=TRUE),
                                   count=.N), by=list(year)]
# Un gráfico - mejorable - como para comparar la distribución de experiencia con el empleador
# por año
ggplot(panel_km[!is.na(YearsWithEmployer_1968_2015) & year>1975 & YearsWithEmployer_1968_2015<=20 & YearsWithEmployer_1968_2015>0]) +
  geom_density(aes(x=YearsWithEmployer_1968_2015),fill="#FF0000") +
  facet_wrap(~year,ncol=1,strip.position="left")

# Detección de cambios de empleo 
# Primera aparición
panel_km[,firstApp:=min(year),by='pid']
# Esto está basado en el anexo. Revisar si está bien
panel_km[,EMP_CHANGE:=ifelse(year==1968 & YearsWithEmployer_1968_2015==1, TRUE, FALSE)]
panel_km[,EMP_CHANGE:=ifelse(year==1969 & YearsWithEmployer_1968_2015 ==2 & shift(EMP_CHANGE)==TRUE, TRUE,EMP_CHANGE), by='pid']
panel_km[,EMP_CHANGE:=ifelse(year %in% c(1969:1975) & !EMP_CHANGE & YearsWithEmployer_1968_2015 ==1, TRUE,EMP_CHANGE), by='pid']
panel_km[,EMP_CHANGE:=ifelse(year %in% c(1969:1975) & !EMP_CHANGE & YearsWithEmployer_1968_2015 ==2 & firstApp, TRUE,EMP_CHANGE), by='pid']
panel_km[,EMP_CHANGE:=ifelse(year %in% c(1970:1975) & !EMP_CHANGE & YearsWithEmployer_1968_2015 ==2 & !shift(YearsWithEmployer_1968_2015)%in% c(1,2,9), TRUE,EMP_CHANGE), by='pid']
panel_km[,EMP_CHANGE:=ifelse(year == 1976 & !EMP_CHANGE & !YearsWithEmployer_1968_2015 %in% 0 & YearsWithEmployer_1968_2015 <15/12, TRUE,EMP_CHANGE), by='pid']
# Hasta acá, ambas reglas son iguales así que creamos una variable EMP_CHANGE24T
panel_km[,EMP_CHANGE24T:=EMP_CHANGE]
# "Regla general": la de si lo que transcurrió como experiencia
# es menor al tiempo transcurrido entre entrevistas
panel_km[!is.na(YearsWithEmployer_1968_2015),
         EMP_CHANGE:=ifelse(year %in% c(1976:2017) & !EMP_CHANGE & !YearsWithEmployer_1968_2015 %in% 0 & (YearsWithEmployer_1968_2015 < (year-shift(year))), TRUE,EMP_CHANGE), by='pid']
# We add 24T employer changes. "24T" detects an employment change when tenure with exployer
# is two years more or two years less that what should be given the period between interviews
# Solo cambiamos la última parte
panel_km[!is.na(YearsWithEmployer_1968_2015) & year %in% c(1976:2017) & !YearsWithEmployer_1968_2015 %in% 0,
               EMP_CHANGE24T:=ifelse((YearsWithEmployer_1968_2015>(shift(YearsWithEmployer_1968_2015)+((year-shift(year))+2)) |
                                        YearsWithEmployer_1968_2015<(shift(YearsWithEmployer_1968_2015)+((year-shift(year))-2))) & 
                                       !EMP_CHANGE24T & year %in% c(1977:2017),TRUE,EMP_CHANGE24T), by='pid']
# Si no había información sobre experiencia con el empleador, lo dejamos como NA en EMP_CHANGE
# y EMP_CHANGE24T
panel_km <- panel_km[,`:=` (EMP_CHANGE=ifelse(is.na(YearsWithEmployer_1968_2015),NA,EMP_CHANGE),
                            EMP_CHANGE24T=ifelse(is.na(YearsWithEmployer_1968_2015),NA,EMP_CHANGE24T))]
# Employment switch detection
EmpChangeTest<-panel_km[EmploymentStatus %in% 1 & !is.na(EMP_CHANGE24T),
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

table(panel_km$EMP_CHANGE,
      panel_km$EMP_CHANGE24T)


# We find changes in occupation and industry based on EMP_CHANGE and EMP_CHANGE24T
# Only changes in reported activity or occupation
# _UNC : uncontrolled. Without employer change detection
# _EMP : New employer
# _EMP24T : New employer with 24T detection
panel_km <- panel_km[order(pid,year)]
panel_km <- panel_km[!is.na(PresentMainJob_3dInd_1968_2001),
                    `:=`(IND_CHANGE_UNC=ifelse(!PresentMainJob_3dInd_1968_2001 == shift(PresentMainJob_3dInd_1968_2001,type='lag'),TRUE,FALSE)),
                    by=c('pid')]
panel_km <- panel_km[!is.na(PresentMainJob_3dOcc_1968_2001),
                                 `:=`(OCC_CHANGE_UNC=ifelse(!PresentMainJob_3dOcc_1968_2001 == shift(PresentMainJob_3dOcc_1968_2001),TRUE,FALSE)),
                                 by=c('pid')]
panel_km <- panel_km[,`:=` (IND_CHANGE_UNC=ifelse(is.na(IND_CHANGE_UNC),FALSE,IND_CHANGE_UNC),
                                        OCC_CHANGE_UNC=ifelse(is.na(OCC_CHANGE_UNC),FALSE,OCC_CHANGE_UNC))]

panel_km <- panel_km[,`:=` (IND_CHANGE_EMP=ifelse(IND_CHANGE_UNC & EMP_CHANGE,TRUE,FALSE),
                                        OCC_CHANGE_EMP=ifelse(OCC_CHANGE_UNC & EMP_CHANGE,TRUE,FALSE),
                                        IND_CHANGE_EMP24T=ifelse(IND_CHANGE_UNC & EMP_CHANGE24T,TRUE,FALSE),
                                        OCC_CHANGE_EMP24T=ifelse(OCC_CHANGE_UNC & EMP_CHANGE24T,TRUE,FALSE))]
# Checking act and occ switches
grafico<-melt(panel_km[EmploymentStatus == 1,
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

panel_km <- panel_km[,`:=` (IND_CHANGE_EMP=ifelse(year %in% c(1968:1980),IND_CHANGE_UNC,IND_CHANGE_EMP),
                                        OCC_CHANGE_EMP=ifelse(year %in% c(1968:1980),OCC_CHANGE_UNC,OCC_CHANGE_EMP),
                                        IND_CHANGE_EMP24T=ifelse(year %in% c(1968:1980),IND_CHANGE_UNC,IND_CHANGE_EMP24T),
                                        OCC_CHANGE_EMP24T=ifelse(year %in% c(1968:1980),OCC_CHANGE_UNC,OCC_CHANGE_EMP24T))]


grafico<-melt(panel_km[EmploymentStatus == 1 & Sex_Individual_1968_2015 == 1,
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

# Now that we have the breaks, lets build the the spells. First, we create the 
panel_km <- panel_km[, `:=`(EMPLOYER_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(YearsWithEmployer_1968_2015)]),1,0),
                            EMPLOYER_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1  & !is.na(YearsWithEmployer_1968_2015)]),1,0)),
                                 by='pid']
panel_km$year <- as.numeric(as.character(panel_km$year))
panel_km <- panel_km[is.na(EMPLOYER_SPELL) & is.na(EMPLOYER_SPELL24T),
                                 `:=`(EMPLOYER_SPELL=as.numeric(EMP_CHANGE),
                                      EMPLOYER_SPELL24T=as.numeric(EMP_CHANGE24T))]
panel_km <- panel_km[!is.na(EMPLOYER_SPELL24T) & !is.na(EMPLOYER_SPELL),
                                 `:=`(EMPLOYER_SPELL= cumsum(.SD$EMPLOYER_SPELL[!is.na(.SD$EMPLOYER_SPELL)]),
                                      EMPLOYER_SPELL24T=cumsum(.SD$EMPLOYER_SPELL24T[!is.na(.SD$EMPLOYER_SPELL24T)])),
                                 by = pid]
panel_km <- panel_km[,`:=`(EMPLOYER_SPELL= ifelse(EMPLOYER_SPELL %in% 0,NA,EMPLOYER_SPELL),
                                       EMPLOYER_SPELL24T= ifelse(EMPLOYER_SPELL24T %in% 0,NA,EMPLOYER_SPELL24T))]

# Summary of employment spells
table(panel_km[,list(min=min(EMPLOYER_SPELL),
                           max=max(EMPLOYER_SPELL)),by='pid']$max)
panel_km$EXP_EMP <- panel_km$EXP_EMP24T <- as.double()
# Remove all data before first tenure with employer data
panel_km <- panel_km[,firstTenureYear:=min(year[!is.na(YearsWithEmployer_1968_2015)]), by=c('pid')]
panel_km <- panel_km[!firstTenureYear %in% 'Inf']
panel_km <- panel_km[!year<firstTenureYear]

# Builds occ and act spells
panel_km <- panel_km[,
                     `:=`(OCC_SPELL=ifelse(OCC_CHANGE_EMP,1,0),
                          IND_SPELL=ifelse(IND_CHANGE_EMP,1,0),
                          OCC_SPELL24T=ifelse(OCC_CHANGE_EMP24T,1,0),
                          IND_SPELL24T=ifelse(IND_CHANGE_EMP24T,1,0))]
panel_km <- panel_km[,`:=`(OCC_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(PresentMainJob_3dOcc_1968_2001)]),1,OCC_SPELL),
                           IND_SPELL=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(PresentMainJob_3dInd_1968_2001)]),1,IND_SPELL),
                           OCC_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(PresentMainJob_3dOcc_1968_2001)]),1,OCC_SPELL24T),
                           IND_SPELL24T=ifelse(year==min(year[EmploymentStatus%in% 1 & !is.na(PresentMainJob_3dInd_1968_2001)]),1,IND_SPELL24T)),
                                 by='pid']
panel_km <- panel_km[,
                           `:=`(OCC_SPELL=ifelse(is.na(OCC_SPELL),0,OCC_SPELL),
                                IND_SPELL=ifelse(is.na(IND_SPELL),0,IND_SPELL),
                                OCC_SPELL24T=ifelse(is.na(OCC_SPELL24T),0,OCC_SPELL24T),
                                IND_SPELL24T=ifelse(is.na(IND_SPELL24T),0,IND_SPELL24T)),
                                 by='pid']
panel_km <- panel_km[,
                     `:=`(OCC_SPELL=cumsum(.SD$OCC_SPELL),
                          IND_SPELL=cumsum(.SD$IND_SPELL),
                          OCC_SPELL24T=cumsum(.SD$OCC_SPELL24T),
                          IND_SPELL24T=cumsum(.SD$IND_SPELL24T)),
                     by='pid']
panel_km <- panel_km[,
                     `:=`(OCC_SPELL=ifelse(OCC_SPELL %in% 0,NA,OCC_SPELL),
                          IND_SPELL=ifelse(IND_SPELL %in% 0,NA,IND_SPELL),
                          OCC_SPELL24T=ifelse(OCC_SPELL24T %in% 0,NA,OCC_SPELL24T),
                          IND_SPELL24T=ifelse(IND_SPELL24T %in% 0,NA,IND_SPELL24T)),
                     by='pid']

# Before estimating the experience, we need to impute the most frequent occupation
# and industry codes for their specific spells
panel_km <- panel_km[,
                     MostFreqOCC:=names(sort(table(PresentMainJob_3dOcc_1968_2001)))[1],
                     by=c('pid','OCC_SPELL')]
panel_km <- panel_km[,
                     MostFreqOCC24T:=names(sort(table(PresentMainJob_3dOcc_1968_2001)))[1],
                     by=c('pid','OCC_SPELL24T')]
panel_km <- panel_km[,
                     MostFreqIND:=names(sort(table(PresentMainJob_3dInd_1968_2001)))[1],
                     by=c('pid','IND_SPELL')]
panel_km <- panel_km[,
                     MostFreqIND24T:=names(sort(table(PresentMainJob_3dInd_1968_2001)))[1],
                     by=c('pid','IND_SPELL24T')]
# Data wrangling in WORK EXPERIENCE
panel_km <- panel_km[,
                     WorkExpSince18:=plyr::mapvalues(WorkExpSince18,
                                                     from=c(0,99),
                                                     to=c(NA,NA))]
panel_km <- panel_km[,
                     minInfoWorkExp:=min(year[!is.na(WorkExpSince18)],na.rm=TRUE),
                     by='pid']
# Me quedé por acá en la revisión. Hay que ver cómo le imputan la experiencia en la ocupación, industria
# y experiencia en general.
# Corte 800 horas anuales para todas las tenure

# En el A.2 del paper, para employer tenure, saco la siguiente conclusión:
# 1) Para cada spell de empleador, asignar el valor mínimo cuando arranca
panel_km <- panel_km[!is.na(YearsWithEmployer_1968_2015),
                     EXP_EMP:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid','EMPLOYER_SPELL')]
panel_km <- panel_km[!is.na(YearsWithEmployer_1968_2015),
                     EXP_EMP24T:=ifelse(year == min(year),YearsWithEmployer_1968_2015,NA),
                     by=c('pid','EMPLOYER_SPELL24T')]
# 2) Resetear la experiencia con el empleador en 1981 
panel_km <- panel_km[year %in% 1981,
                     EXP_EMP:=YearsWithEmployer_1968_2015]
panel_km <- panel_km[year %in% 1981,
                     EXP_EMP24T:=YearsWithEmployer_1968_2015]
# 3) Para cada año entre 1969-1978 y 1982-93 agregarle 1 de experiencia si trabajó 800 horas
panel_km <- panel_km[year %in% c(1969:1978,1982:1993) & is.na(EXP_EMP),
                     EXP_EMP:=ifelse(EmploymentStatus %in% 1 & TodosEmpleos_YearHrs_Head_1968_2015 >= 800, year-shift(year),0),
                     list(pid)]
panel_km <- panel_km[year %in% c(1969:1978,1982:1993) & is.na(EXP_EMP24T),
                     EXP_EMP24T:=ifelse(EmploymentStatus %in% 1 & TodosEmpleos_YearHrs_Head_1968_2015 >= 800, year-shift(year),0),
                     list(pid)]


panel_km <- panel_km[!is.na(EXP_EMP),
                     EXP_EMP:=cumsum(EXP_EMP),by=c('pid','EMPLOYER_SPELL')]
panel_km <- panel_km[!is.na(EXP_EMP24T),
                     EXP_EMP24T:=cumsum(EXP_EMP24T),by=c('pid','EMPLOYER_SPELL24T')]

panel_km <- panel_km[!is.na(Years_Individual_1968_2015),
                     EXP_EMP24T:=Years_Individual_1968_2015[year==min(year)],
                     list(pid,EMPLOYER_SPELL24T)]



panel_km <- panel_km[,
                     minInfoWorkExp:=ifelse(is.infinite(minInfoWorkExp),NA,minInfoWorkExp)]
panel_km <- panel_km[,
                     minInfoWorkExp:=ifelse(minInfoWorkExp==year,TRUE,FALSE)]
# Acá asignamos la experiencia inicial
panel_km <- panel_km[minInfoWorkExp == TRUE,
                     EXP_WORK:=WorkExpSince18]
# Si la diferencia entre la edad y la experiencia de empleo es menor a 18,
# se setea a 0. Es lo mismo que pensar que solo tienen experiencia desde esa edad
panel_km[!is.na(EXP_WORK),
         EXP_WORK:=ifelse((Years_Individual_1968_2015-EXP_WORK)<18,0,EXP_WORK)]
# Aprovechamos para inicializar, también, la exp
panel_km[,`:=` (EXP_OCC=EXP_WORK,
               EXP_IND=EXP_WORK)]

# Now we are able to compute the work, occupation and activity years 
# Overall working experience 
panel_km[year > 1973,
         EXP_WORK:= ifelse(!is.na(EXP_WORK),EXP_WORK,
                           ifelse(EmploymentStatus %in% 1 & TodosEmpleos_YearHrs_Head_1968_2015 >= 800, year-shift(year),0)),
         by='pid']
panel_km[year > 1973,
         EXP_WORK:=cumsum(EXP_WORK),
         by='pid']

# Occupational/industry experience
# Primero tenemos que inicializar las experiencias.
# Si es anterior a 1981, le ponemos la misma experiencia que work experience


# Si es posterior a 1981, recién empezamos a computar la experiencia en el
# segundo spell de industria/ocupacion
panel_km[year>1980 & OCC_SPELL > 1 & EmploymentStatus %in% 1 & TodosEmpleos_YearHrs_Head_1968_2015 >= 800,
         EXP_OCC:=1]
panel_km[year>1980 & OCC_SPELL > 1 & !(EmploymentStatus %in% 1 & TodosEmpleos_YearHrs_Head_1968_2015 >= 800),
         EXP_OCC:=0]


panel_km <- panel_km[,difYearsOcc:=ifelse(!is.na(EXP_OCC),EXP_OCC,
                                          ifelse(, year-shift(year),0)),
                     by='pid']

panel1968_2015 <- panel1968_2015[,
                                 EXP_OCC:= sapply(seq_len(.N), function(x) (MostFreqOCC[seq_len(x)] %in% MostFreqOCC[x])%*%difYearsOcc[seq_len(x)]),
                                 by='pid']
panel1968_2015 <- panel1968_2015[,difYearsOcc:=ifelse(!is.na(EXP_OCC24T),EXP_OCC24T,
                                                      ifelse(EmploymentStatus %in% 1, year-shift(year),0)),
                                 by='pid']

panel1968_2015 <- panel1968_2015[,
                                 EXP_OCC24T:= sapply(seq_len(.N), function(x) (MostFreqOCC24T[seq_len(x)] %in% MostFreqOCC24T[x])%*%difYearsOcc[seq_len(x)]),
                                 by='pid']

# Industry experience
panel1968_2015 <- panel1968_2015[,difYearsInd:=ifelse(!is.na(EXP_IND),EXP_IND,
                                                      ifelse(EmploymentStatus %in% 1, year-shift(year),0)),
                                 by='pid']
panel1968_2015 <- panel1968_2015[,
                                 EXP_IND:= sapply(seq_len(.N), function(x) (MostFreqIND[seq_len(x)] %in% MostFreqIND[x])%*%difYearsInd[seq_len(x)]),
                                 by='pid']
panel1968_2015 <- panel1968_2015[,difYearsInd:=ifelse(!is.na(EXP_IND24T),EXP_IND24T,
                                                      ifelse(EmploymentStatus %in% 1, year-shift(year),0)),
                                 by='pid']

panel1968_2015 <- panel1968_2015[,
                                 EXP_IND24T:= sapply(seq_len(.N), function(x) (MostFreqIND24T[seq_len(x)] %in% MostFreqIND24T[x])%*%difYearsInd[seq_len(x)]),
                                 by='pid']
# Controls that might be useful for the regression
# Regional dummy
panel1968_2015$Region_1968_2015 <- factor(panel1968_2015$Region_1968_2015)
# Convierto a factor la variable OneDigitOcc para la regresión
panel1968_2015$OneDigitOcc <- factor(panel1968_2015$Occ1d)
panel1968_2015$OneDigitInd <- factor(panel1968_2015$Ind1d)
# OJ dummy: 1 if the worker is not working in the first year with a new employer
panel1968_2015 <- panel1968_2015[, OJ:= ifelse(year == min(year), 0, 1),by=c('pid','EMPLOYER_SPELL')]
panel1968_2015 <- panel1968_2015[, OJ24T:= ifelse(year == min(year), 0, 1),by=c('pid','EMPLOYER_SPELL24T')]
# Convierto a factor la variable year para la regresión
panel1968_2015$year <- factor(panel1968_2015$year)
# Data wrangling on income data
panel1968_2015$HourlyEarnings_AllJobs_1968_2015 <- ifelse(panel1968_2015$HourlyEarnings_AllJobs_1968_2015 %in% 99.99 & panel1968_2015$year==1968,
                                                          NA,
                                                          panel1968_2015$HourlyEarnings_AllJobs_1968_2015)
panel1968_2015$HourlyEarnings_AllJobs_1968_2015 <- ifelse(panel1968_2015$HourlyEarnings_AllJobs_1968_2015 %in% 0.00,
                                                          NA,
                                                          panel1968_2015$HourlyEarnings_AllJobs_1968_2015)
panel1968_2015$HourlyEarnings_AllJobs_1968_2015 <- ifelse(panel1968_2015$HourlyEarnings_AllJobs_1968_2015 %in% c(9999.99,9999.00,9998,9998.00),
                                                          NA,
                                                          panel1968_2015$HourlyEarnings_AllJobs_1968_2015)

# Data wrangling with government status
panel1968_2015$MainJob_Government_1979_2001 <- ifelse(panel1968_2015$year %in% c(1968:1982) & panel1968_2015$MainJob_Government_1979_2001 %in% 5,'FALSE',
                                                      ifelse(panel1968_2015$year %in% c(1983:2001) & panel1968_2015$MainJob_Government_1979_2001 %in% 4,'FALSE',
                                                             panel1968_2015$MainJob_Government_1979_2001))

panel1968_2015$PresentOrLastMain_Government_2003_15 <-  ifelse(panel1968_2015$PresentOrLastMain_Government_2003_15 %in% 4,'FALSE',
                                                               panel1968_2015$PresentOrLastMain_Government_2003_15)


panel1968_2015$MainJob_Government <- ifelse(panel1968_2015$year %in% c(1968:2001),
                                            panel1968_2015$MainJob_Government_1979_2001,
                                            panel1968_2015$PresentOrLastMain_Government_2003_15)
panel1968_2015$MainJob_Government <- ifelse(panel1968_2015$MainJob_Government %in% "FALSE",FALSE,TRUE)
# Nominal to real incomes
library(zoo)
cpi <- fread('CPIAUCSL.csv')
cpi$DATE <- as.integer(year(as.Date(cpi$DATE)))
cpi <- cpi[,list(ipc=mean(CPIAUCSL)),by='DATE']
cpi$ipc <- cpi$ipc[73]/cpi$ipc
panel1968_2015$year <- as.numeric(as.character(panel1968_2015$year))
panel1968_2015 <- panel1968_2015[cpi,on=c('year'='DATE'),ipc:=i.ipc]
panel1968_2015 <- panel1968_2015[,
                                 HourlyEarnings_MainJob_1970_2015:=ifelse(year %in% c(1970:1977) & HourlyEarnings_MainJob_1970_2015 %in% c(0.00,9.99),NA,HourlyEarnings_MainJob_1970_2015)]
panel1968_2015 <- panel1968_2015[,
                                 HourlyEarnings_MainJob_1970_2015:=ifelse(year %in% c(1978:1992) & HourlyEarnings_MainJob_1970_2015 %in% c(0.00,99.99),NA,HourlyEarnings_MainJob_1970_2015)]
panel1968_2015 <- panel1968_2015[,
                                 HourlyEarnings_MainJob_1970_2015:=ifelse(year %in% c(1993) & HourlyEarnings_MainJob_1970_2015 %in% c(0.00,998.00,999.00),NA,HourlyEarnings_MainJob_1970_2015)]
panel1968_2015 <- panel1968_2015[,
                                 HourlyEarnings_MainJob_1970_2015:=ifelse(year %in% c(1994:2017) & !(HourlyEarnings_MainJob_1970_2015 > 0.00 & HourlyEarnings_MainJob_1970_2015 <= 997.00),NA,HourlyEarnings_MainJob_1970_2015)]

panel1968_2015 <- panel1968_2015[,
                                 LaborEarnings_AllJobs_Head_1968_2015:=ifelse(LaborEarnings_AllJobs_Head_1968_2015 %in% 0 | LaborEarnings_AllJobs_Head_1968_2015>9999998,NA,LaborEarnings_AllJobs_Head_1968_2015)]

panel1968_2015$RealHourlyEarningsAllJobs <- panel1968_2015$HourlyEarnings_AllJobs_1968_2015*panel1968_2015$ipc 
panel1968_2015$RealHourlyEarningsMainJob <- panel1968_2015$HourlyEarnings_MainJob_1970_2015*panel1968_2015$ipc 
panel1968_2015$RealLaborEarnings <- panel1968_2015$LaborEarnings_AllJobs_Head_1968_2015*panel1968_2015$ipc 
panel1968_2015 <- panel1968_2015[,INDEPENDIENTE:=ifelse(any(PresentMain_Modalidad_Head %in% c(2,3)),TRUE,FALSE),by='pid']
panel1968_2015 <- panel1968_2015[,FirstYear := min(as.numeric(as.character(year))), by='pid']
panel1968_2015 <- panel1968_2015[,
                                 SelectionKambourov:=ifelse( (any(EMPLOYER_SPELL > 1) & any(OCC_SPELL > 1) & any(IND_SPELL > 1)) & !any(Occ3d_OCC2010 %in% c(6005:6130,9800:9830)), TRUE,FALSE),
                                 by='pid']
panel1968_2015 <- panel1968_2015[,
                                 lengthHistory:=.N,
                                 by=c('pid')]
panel1968_2015 <- panel1968_2015[,
                                 lengthHistoryEMP:=.N,
                                 by=c('pid','EMPLOYER_SPELL24T')]
# Checking mincer style equations

mincerTest <- lm(panel1968_2015[Grades_Individual_1968_2015 <98 &
                                  Grades_Individual_1968_2015 >0 & 
                                  !Occ3d_OCC2010 %in% 9920 &
                                  !ind3d_IND1990 %in% 0 &
                                  !MainJob_Government  &
                                  Sex_Individual_1968_2015 %in% 1 &
                                  !INDEPENDIENTE & SelectionKambourov &
                                  RealHourlyEarningsMainJob>1],
                 formula=log(RealHourlyEarningsMainJob) ~ Grades_Individual_1968_2015 + EXP_WORK + EXP_OCC24T  + EXP_IND24T + EXP_EMP24T + OneDigitInd + OneDigitOcc )