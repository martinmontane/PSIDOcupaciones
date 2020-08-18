# Carga librerias (instala si es necesario)
librerias <- c("tidyverse","readxl","data.table","RCurl","futile.logger","SAScii")
lapply(librerias,
       function(x) {
         if(!x %in% installed.packages()){ install.packages(x) } 
         require(x,character.only = TRUE)
       })
# Importante tener estos dos códigos que replican el paquete psidR
source("buildPanelCustom.R")
source("makeids.R")
# Creamos un objeto que tenga las variables para cada año
tablaDatos <- openxlsx::read.xlsx("https://psidonline.isr.umich.edu/help/xyr/psid.xlsx")
# Nos quedamos con los años que están cubiertos
PSIDFullYears <- as.numeric(gsub(pattern = 'Y',
                                 replacement = '',
                                 x = c(colnames(tablaDatos)[-c(1:5)])))
# getNamesPSID busca el equivalente de una variable para todos los años de la PSID,
# dándole como inicio cualquier valor para cualquier año. Pero hay que hacer
# algunos cambios para construir todos los datos de ocupaciones
## Ocupacion
PresentMain_3dOccupation_Head_1968_2001 <- c(getNamesPSID("V197_A",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1980]),
                                             getNamesPSID("V7712",tablaDatos,years=PSIDFullYears[PSIDFullYears>1980]))
PresentMain_3dOccupation_Spouse_1968_2001 <- c(getNamesPSID("V243_A",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1980]),
                                               getNamesPSID("V7885",tablaDatos,years=PSIDFullYears[PSIDFullYears>1980]))
## Industria
PresentMain_3dIndustry_Head_1968_2001 <- c(getNamesPSID("V197_B",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1980]),
                                           getNamesPSID("V7713",tablaDatos,years=PSIDFullYears[PSIDFullYears>1980]))
PresentMain_3dIndustry_Spouse_1968_2001 <- c(getNamesPSID("V243_B",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1980]),
                                             getNamesPSID("V7886",tablaDatos,years=PSIDFullYears[PSIDFullYears>1980]))
## Modalidad
PresentMain_Modalidad_Head_1968_2001 <- getNamesPSID("V198",tablaDatos,years=PSIDFullYears)
PresentMain_Modalidad_Spouse_1968_2001 <- getNamesPSID("V6592",tablaDatos,years=PSIDFullYears)

## Horas trabajadas
TodosEmpleos_YearHrs_Head_1968_2015 <- getNamesPSID("V47",tablaDatos,years=PSIDFullYears)
TodosEmpleos_YearHrs_Spouse_1968_2015 <- getNamesPSID("V53",tablaDatos,years=PSIDFullYears)
TodosEmpleos_WeekHrs_Head_1994_2015 <- getNamesPSID("ER4093",tablaDatos,years=PSIDFullYears)
TodosEmpleos_WeekHrs_Spouse_1994_2015 <- getNamesPSID("ER4104",tablaDatos,years=PSIDFullYears)
MainJob_WeekHrs_Head_1968_2001 <- getNamesPSID("V225",tablaDatos,years=PSIDFullYears)
MainJob_WeekHrs_Spouse_1968_2001 <- getNamesPSID("V245",tablaDatos,years=PSIDFullYears)

## Ingresos

HourlyEarnings_MainJob_Head_1970_2015 <- getNamesPSID("V1297",tablaDatos,years=PSIDFullYears) # Solo si es pago por hora
HourlyEarnings_MainJob_Spouse_1979_2015 <- getNamesPSID("V6616",tablaDatos,years=PSIDFullYears) # Solo si es pago por hora
  HourlyEarnings_AllJobs_Head_1968_2015 <-getNamesPSID("V337",tablaDatos,years=PSIDFullYears) # Solo si es pago por hora
HourlyEarnings_AllJobs_Spouse_1968_2015 <-getNamesPSID("V338",tablaDatos,years=PSIDFullYears) # Solo si es pago por hora


# Sindicato
TrabajoCubiertoSindicato_Head_1976_2015 <- getNamesPSID("V4478",tablaDatos,years=PSIDFullYears)
TrabajoCubiertoSindicato_Spouse_1979_2015 <- getNamesPSID("V4861",tablaDatos,years=PSIDFullYears)
TrabajadorPerteneceUnion_Head_1976_2015 <- getNamesPSID("V4479",tablaDatos,years=PSIDFullYears)
TrabajadorPerteneceUnion_Spouse_1979_2015 <- getNamesPSID("V4862",tablaDatos,years=PSIDFullYears)
UnionMember_Head_1968_1981 <- getNamesPSID("V294",tablaDatos,years=PSIDFullYears)

# Government Employee
MainJob_Government_Head_1975_2001 <- getNamesPSID("V3971",tablaDatos,years=PSIDFullYears)
MainJob_Government_Spouse_1979_2001 <- getNamesPSID("V4845",tablaDatos,years=PSIDFullYears)

# Employment Status
EmploymentStatus_Head_1968_2015 <- c(getNamesPSID("ER2068",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1996]),
                                     getNamesPSID("ER25104",tablaDatos,years=PSIDFullYears[PSIDFullYears>1996]))
EmploymentStatus_Spouse_1976_2015 <- c(getNamesPSID("V4841",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1996]),
                                       getNamesPSID("ER25362",tablaDatos,years=PSIDFullYears[PSIDFullYears>1996]))
## 2003-2015
# Ocupacion 
PresentOrLastMain_3dOccupation_Head_2003_15 <- getNamesPSID("ER21145",tablaDatos,years=PSIDFullYears)
PresentOrLastMain_3dOccupation_Spouse_2003_15 <- getNamesPSID("ER21395",tablaDatos,years=PSIDFullYears)
Additional1Job_3dOccupation_Head_2003_15 <- getNamesPSID("ER21201",tablaDatos,years=PSIDFullYears)
Additional1Job_3dOccupation_Spouse_2003_15 <- getNamesPSID("ER21451",tablaDatos,years=PSIDFullYears)
Additional2Job_3dOccupation_Head_2003_15 <- getNamesPSID("ER21233",tablaDatos,years=PSIDFullYears)
Additional2Job_3dOccupation_Spouse_2003_15 <- getNamesPSID("ER21483",tablaDatos,years=PSIDFullYears)
Additional3Job_3dOccupation_Head_2003_15 <- getNamesPSID("ER21265",tablaDatos,years=PSIDFullYears)
Additional3Job_3dOccupation_Spouse_2003_15 <- getNamesPSID("ER21515",tablaDatos,years=PSIDFullYears)

# Industria
PresentOrLastMain_3dIndustry_Head_2003_15 <- getNamesPSID("ER21146",tablaDatos,years=PSIDFullYears)
PresentOrLastMain_3dIndustry_Spouse_2003_15 <- getNamesPSID("ER21396",tablaDatos,years=PSIDFullYears)
Additional1Job_3dIndustry_Head_2003_15 <- getNamesPSID("ER21202",tablaDatos,years=PSIDFullYears)
Additional1Job_3dIndustry_Spouse_2003_15 <- getNamesPSID("ER21452",tablaDatos,years=PSIDFullYears)
Additional2Job_3dIndustry_Head_2003_15 <- getNamesPSID("ER21234",tablaDatos,years=PSIDFullYears)
Additional2Job_3dIndustry_Spouse_2003_15 <- getNamesPSID("ER21484",tablaDatos,years=PSIDFullYears)
Additional3Job_3dIndustry_Head_2003_15 <- getNamesPSID("ER21266",tablaDatos,years=PSIDFullYears)
Additional3Job_3dIndustry_Spouse_2003_15 <- getNamesPSID("ER21516",tablaDatos,years=PSIDFullYears)

# Modalidad
PresentOrLastMain_Modalidad_Head_2003_15 <- getNamesPSID("ER21147",tablaDatos,years=PSIDFullYears)
PresentOrLastMain_Modalidad_Spouse_2003_15 <- getNamesPSID("ER21397",tablaDatos,years=PSIDFullYears)
Additional1Job_Modalidad_Head_2003_15 <- getNamesPSID("ER21203",tablaDatos,years=PSIDFullYears)
Additional1Job_Modalidad_Spouse_2003_15 <- getNamesPSID("ER21453",tablaDatos,years=PSIDFullYears)
Additional2Job_Modalidad_Head_2003_15 <- getNamesPSID("ER21235",tablaDatos,years=PSIDFullYears)
Additional2Job_Modalidad_Spouse_2003_15 <- getNamesPSID("ER21485",tablaDatos,years=PSIDFullYears)
Additional3Job_Modalidad_Head_2003_15 <- getNamesPSID("ER21267",tablaDatos,years=PSIDFullYears)
Additional3Job_Modalidad_Spouse_2003_15 <- getNamesPSID("ER21517",tablaDatos,years=PSIDFullYears)

# Horas trabajadas
PresentOrLastMain_WeekHrs_Head_2003_15 <- getNamesPSID("ER21176",tablaDatos,years=PSIDFullYears)
PresentOrLastMain_WeekHrs_Spouse_2003_15 <- getNamesPSID("ER21426",tablaDatos,years=PSIDFullYears)
Additional1Job_WeekHrs_Head_2003_15 <- getNamesPSID("ER21208",tablaDatos,years=PSIDFullYears)
Additional1Job_WeekHrs_Spouse_2003_15 <- getNamesPSID("ER21458",tablaDatos,years=PSIDFullYears)
Additional2Job_WeekHrs_Head_2003_15 <- getNamesPSID("ER21240",tablaDatos,years=PSIDFullYears)
Additional2Job_WeekHrs_Spouse_2003_15 <- getNamesPSID("ER21490",tablaDatos,years=PSIDFullYears)
Additional3Job_WeekHrs_Head_2003_15 <- getNamesPSID("ER21272",tablaDatos,years=PSIDFullYears)
Additional3Job_WeekHrs_Spouse_2003_15 <- getNamesPSID("ER21522",tablaDatos,years=PSIDFullYears)

# Government Employee
PresentOrLastMain_Government_Head_2003_15 <- getNamesPSID("ER21149",tablaDatos,years=PSIDFullYears)
PresentOrLastMain_Government_Spouse_2003_15 <- getNamesPSID("ER21399",tablaDatos,years=PSIDFullYears)

# Años con el empleador tal cual aparece en el apéndice del paper (desde el
# 94 en adelante lo agregamos nosotros)
YearsWithEmployer_Head_1968_1993 <- getNamesPSID("V200",tablaDatos,years=c(1968:1993))
YearsWithEmployer_Head_1968_1993[2:8]  <-  getNamesPSID("V642",tablaDatos,years=c(1969:1975))                                    
YearsWithEmployer_Head_1994_2015 <- getNamesPSID("ER2098",tablaDatos,years=c(1994:2017)) 
YearsWithEmployer_Head_1968_2015 <- c(YearsWithEmployer_Head_1968_1993,YearsWithEmployer_Head_1994_2015)
# Información sobre position. Igual que como aparece en el paper
# Hasta 1987 se computa en una sola variable la cantidad de meses en la misma
# posición. Desde 1988 hasta 2001 la situación es un poco más compleja porque
# hay que hacer el cálculo de meses manualmente
position_tenure_total_months_1968_2017 <- getNamesPSID("V4488",tablaDatos,years=c(1968:2017))
position_tenure_start_month_1968_2017 <- getNamesPSID("V15192",tablaDatos,years=c(1968:2017))
position_tenure_start_year_1968_2017 <- getNamesPSID("V15193",tablaDatos,years=c(1968:2017))
date_of_interview_1968_2017 <-  getNamesPSID("V99",tablaDatos,years=c(1968:2017))
#position_equals_employer_tenure_1968_2017 <- getNamesPSID("V15184",tablaDatos,years=c(1968:2017))


YearsWithEmployer_Head_1968_1993[2:8]  <-  getNamesPSID("V642",tablaDatos,years=c(1969:1975))                                    
YearsWithEmployer_Head_1994_2015 <- getNamesPSID("ER2098",tablaDatos,years=c(1968:2017)) 

# Educación y otras variables
Grades_Head_1968_1990 <- getNamesPSID("V313",tablaDatos,years=PSIDFullYears)
Grades_Spouse_1968_1990 <- getNamesPSID("V246",tablaDatos,years=PSIDFullYears)
Grades_Individual_1968_2015 <- getNamesPSID("ER30010",tablaDatos,years=PSIDFullYears)
Years_Individual_1968_2015 <- getNamesPSID("ER30004",tablaDatos,years=PSIDFullYears)
Sex_Individual_1968_2015 <- rep("ER32000",40)
EmployerYears_Head_1968_2015 <- c(getNamesPSID("V4480",tablaDatos,years=PSIDFullYears[PSIDFullYears<=1993]),
                                  getNamesPSID("ER2098",tablaDatos,years=PSIDFullYears[PSIDFullYears>1993]))
race <- getNamesPSID("V181",tablaDatos,years=PSIDFullYears)
Married_Pairs_Indicator_1968_2015 <- c(getNamesPSID("ER30005",tablaDatos,years=PSIDFullYears))
Region_1968_2015 <- c(getNamesPSID("ER6997E",tablaDatos,years=PSIDFullYears))

# Work Experience
# Available from 1974 onwards for heads and spouses
WorkExpSince18_Head <- getNamesPSID('V3620',tablaDatos,years=PSIDFullYears)
WorkExpSince18_Spouse <- getNamesPSID('V3610',tablaDatos,year=PSIDFullYears)
WorkExpSince18FullTime_Head <- getNamesPSID('V3621',tablaDatos,years=PSIDFullYears)
WorkExpSince18FullTime_Spouse <- getNamesPSID('V3611',tablaDatos,year=PSIDFullYears)

# Hay que crear un data frame que tenga los vectores de las variables. Hay que ponerle
# un nombre a cada uno de los vectores porque va a ser el nombre de la columna
dfVariablesFam <- data.frame(year=PSIDFullYears,
                             PresentMain_3dOccupation_Head_1968_2001=PresentMain_3dOccupation_Head_1968_2001,
                             PresentMain_3dOccupation_Spouse_1968_2001=PresentMain_3dOccupation_Spouse_1968_2001,
                             PresentMain_3dIndustry_Head_1968_2001=PresentMain_3dIndustry_Head_1968_2001,
                             PresentMain_3dIndustry_Spouse_1968_2001=PresentMain_3dIndustry_Spouse_1968_2001,
                             PresentMain_Modalidad_Head_1968_2001=PresentMain_Modalidad_Head_1968_2001,
                             PresentMain_Modalidad_Spouse_1968_2001=PresentMain_Modalidad_Spouse_1968_2001,
                             TodosEmpleos_YearHrs_Head_1968_2015=TodosEmpleos_YearHrs_Head_1968_2015,
                             TodosEmpleos_YearHrs_Spouse_1968_2015=TodosEmpleos_YearHrs_Spouse_1968_2015,
                             TodosEmpleos_WeekHrs_Head_1994_2015=TodosEmpleos_WeekHrs_Head_1994_2015,
                             TodosEmpleos_WeekHrs_Spouse_1994_2015=TodosEmpleos_WeekHrs_Spouse_1994_2015,
                             PresentOrLastMain_3dOccupation_Head_2003_15=PresentOrLastMain_3dOccupation_Head_2003_15,
                             PresentOrLastMain_3dOccupation_Spouse_2003_15=PresentOrLastMain_3dOccupation_Spouse_2003_15,
                             Additional1Job_3dOccupation_Head_2003_15=Additional1Job_3dOccupation_Head_2003_15,
                             Additional1Job_3dOccupation_Spouse_2003_15=Additional1Job_3dOccupation_Spouse_2003_15,
                             Additional2Job_3dOccupation_Head_2003_15=Additional2Job_3dOccupation_Head_2003_15,
                             Additional2Job_3dOccupation_Spouse_2003_15=Additional2Job_3dOccupation_Spouse_2003_15,
                             Additional3Job_3dOccupation_Head_2003_15=Additional3Job_3dOccupation_Head_2003_15,
                             Additional3Job_3dOccupation_Spouse_2003_15=Additional3Job_3dOccupation_Spouse_2003_15,
                             PresentOrLastMain_3dIndustry_Head_2003_15=PresentOrLastMain_3dIndustry_Head_2003_15,
                             PresentOrLastMain_3dIndustry_Spouse_2003_15=PresentOrLastMain_3dIndustry_Spouse_2003_15,
                             Additional1Job_3dIndustry_Head_2003_15=Additional1Job_3dIndustry_Head_2003_15,
                             Additional1Job_3dIndustry_Spouse_2003_15=Additional1Job_3dIndustry_Spouse_2003_15,
                             Additional2Job_3dIndustry_Head_2003_15=Additional2Job_3dIndustry_Head_2003_15,
                             Additional2Job_3dIndustry_Spouse_2003_15=Additional2Job_3dIndustry_Spouse_2003_15,
                             Additional3Job_3dIndustry_Head_2003_15=Additional3Job_3dIndustry_Head_2003_15,
                             Additional3Job_3dIndustry_Spouse_2003_15=Additional3Job_3dIndustry_Spouse_2003_15,
                             PresentOrLastMain_Modalidad_Head_2003_15=PresentOrLastMain_Modalidad_Head_2003_15,
                             PresentOrLastMain_Modalidad_Spouse_2003_15=PresentOrLastMain_Modalidad_Spouse_2003_15,
                             Additional1Job_Modalidad_Head_2003_15=Additional1Job_Modalidad_Head_2003_15,
                             Additional1Job_Modalidad_Spouse_2003_15=Additional1Job_Modalidad_Spouse_2003_15,
                             Additional2Job_Modalidad_Head_2003_15=Additional2Job_Modalidad_Head_2003_15,
                             Additional2Job_Modalidad_Spouse_2003_15=Additional2Job_Modalidad_Spouse_2003_15,
                             Additional3Job_Modalidad_Head_2003_15=Additional3Job_Modalidad_Head_2003_15,
                             Additional3Job_Modalidad_Spouse_2003_15=Additional3Job_Modalidad_Spouse_2003_15,
                             PresentOrLastMain_WeekHrs_Head_2003_15=PresentOrLastMain_WeekHrs_Head_2003_15,
                             PresentOrLastMain_WeekHrs_Spouse_2003_15=PresentOrLastMain_WeekHrs_Spouse_2003_15,
                             Additional1Job_WeekHrs_Head_2003_15=Additional1Job_WeekHrs_Head_2003_15,
                             Additional1Job_WeekHrs_Spouse_2003_15=Additional1Job_WeekHrs_Spouse_2003_15,
                             Additional2Job_WeekHrs_Head_2003_15=Additional2Job_WeekHrs_Head_2003_15,
                             Additional2Job_WeekHrs_Spouse_2003_15=Additional2Job_WeekHrs_Spouse_2003_15,
                             Additional3Job_WeekHrs_Head_2003_15=Additional3Job_WeekHrs_Head_2003_15,
                             Additional3Job_WeekHrs_Spouse_2003_15=Additional3Job_WeekHrs_Spouse_2003_15,
                             MainJob_WeekHrs_Head_1968_2001=MainJob_WeekHrs_Head_1968_2001,
                             MainJob_WeekHrs_Spouse_1968_2001=MainJob_WeekHrs_Spouse_1968_2001,
                             HourlyEarnings_MainJob_Head_1970_2015=HourlyEarnings_MainJob_Head_1970_2015,
                             HourlyEarnings_MainJob_Spouse_1979_2015=HourlyEarnings_MainJob_Spouse_1979_2015,
                             TrabajoCubiertoSindicato_Head_1976_2015=TrabajoCubiertoSindicato_Head_1976_2015,
                             TrabajoCubiertoSindicato_Spouse_1979_2015=TrabajoCubiertoSindicato_Spouse_1979_2015,
                             TrabajadorPerteneceUnion_Head_1976_2015=TrabajadorPerteneceUnion_Head_1976_2015,
                             TrabajadorPerteneceUnion_Spouse_1979_2015=TrabajadorPerteneceUnion_Spouse_1979_2015,
                             UnionMember_Head_1968_1981=UnionMember_Head_1968_1981,
                             MainJob_Government_Head_1975_2001=MainJob_Government_Head_1975_2001,
                             MainJob_Government_Spouse_1979_2001=MainJob_Government_Spouse_1979_2001,
                             PresentOrLastMain_Government_Head_2003_15=PresentOrLastMain_Government_Head_2003_15,
                             PresentOrLastMain_Government_Spouse_2003_15=PresentOrLastMain_Government_Spouse_2003_15,
                             Grades_Head_1968_1990=Grades_Head_1968_1990,
                             Grades_Spouse_1968_1990=Grades_Spouse_1968_1990,
                             EmploymentStatus_Head_1968_2015=EmploymentStatus_Head_1968_2015,
                             EmploymentStatus_Spouse_1976_2015=EmploymentStatus_Spouse_1976_2015,
                             HourlyEarnings_AllJobs_Head_1968_2015=HourlyEarnings_AllJobs_Head_1968_2015,
                             HourlyEarnings_AllJobs_Spouse_1968_2015=HourlyEarnings_AllJobs_Spouse_1968_2015,
                             YearsWithEmployer_Head_1968_2015=YearsWithEmployer_Head_1968_2015,
                             Region_1968_2015=Region_1968_2015,
                             # YearsWithEmployer_Head_1968_1993=YearsWithEmployer_Head_1968_1993,
                             # YearsWithEmployer_Head_1994_2015=YearsWithEmployer_Head_1994_2015,
                             race_1968_2015=race,
                             WorkExpSince18_Head=WorkExpSince18_Head,
                             WorkExpSince18_Spouse=WorkExpSince18_Spouse,
                             WorkExpSince18FullTime_Head=WorkExpSince18FullTime_Head,
                             WorkExpSince18FullTime_Spouse=WorkExpSince18FullTime_Spouse)
dfVariablesInd <- data.frame(year=PSIDFullYears,
                             Grades_Individual_1968_2015=Grades_Individual_1968_2015,
                             Years_Individual_1968_2015=Years_Individual_1968_2015,
                             Sex_Individual_1968_2015=Sex_Individual_1968_2015,
                             Married_Pairs_Indicator_1968_2015=Married_Pairs_Indicator_1968_2015)

# Finalmente, build.panel crea el panel pasándole el data frame de variables
# a nivel de familia que queremos pasarle y la dirección donde están los .rda
# Acá asumí que estaban pegados en la carpeta principal de tu proyecto,
# pero se puede cambiar.
panel1968_2015 <- build.panel(datadir = paste0(getwd(),'/','PSIDRDa/'),
                              fam.vars=dfVariablesFam,
                              ind.vars = dfVariablesInd,
                              design = 'all')
# Filtro heads 
# panel1968_2015 <- panel1968_2015 %>% filter(relation.head %in% c(1,10))

# #Alguna estadística descriptiva
# info <- data.table(panel1968_2015) %>% 
#   .[,list(conteo=.N),by=c('pid')] %>%
#   .[,list(apariciones=.N), by=c('conteo')] 

# ggplot(info) + geom_bar(aes(x=conteo, y=apariciones),stat = 'identity')
# Reducimos las variables que duplican (hombre/mujer).
# CHEQUEAR si esto está bien (es un ifelse según la relation.head)
panel1968_2015<- panel1968_2015 %>%
  mutate(PresentMainJob_3dOcc_1968_2001=ifelse(relation.head %in% c(20,22),PresentMain_3dOccupation_Spouse_1968_2001,PresentMain_3dOccupation_Head_1968_2001),
         PresentMainJob_3dInd_1968_2001=ifelse(relation.head %in% c(20,22),PresentMain_3dIndustry_Spouse_1968_2001,PresentMain_3dIndustry_Head_1968_2001),
         PresentMainJob_Modalidad_1968_2001=ifelse(relation.head %in% c(20,22), PresentMain_Modalidad_Spouse_1968_2001, PresentMain_Modalidad_Head_1968_2001),
         TodosEmpleos_WeekHrs_1994_2015=ifelse(relation.head %in% c(20,22), TodosEmpleos_WeekHrs_Spouse_1994_2015, TodosEmpleos_WeekHrs_Head_1994_2015),
         TodosEmpleos_YearHrs_1968_2015=ifelse(relation.head %in% c(20,22),TodosEmpleos_YearHrs_Spouse_1968_2015,TodosEmpleos_YearHrs_Head_1968_2015),
         MainJob_WeekHrs_1968_2001=ifelse(relation.head %in% c(20,22), MainJob_WeekHrs_Spouse_1968_2001,MainJob_WeekHrs_Head_1968_2001),
         UnionMember_Head_1968_1981=ifelse(relation.head %in% c(20,22), NA,UnionMember_Head_1968_1981),
         PresentOrLastMain_3dOccupation_2003_15=ifelse(relation.head %in% c(20,22), PresentOrLastMain_3dOccupation_Spouse_2003_15,PresentOrLastMain_3dOccupation_Head_2003_15),
         Additional1Job_3dOccupation_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_3dOccupation_Spouse_2003_15,Additional1Job_3dOccupation_Head_2003_15),
         Additional2Job_3dOccupation_2003_15=ifelse(relation.head %in% c(20,22), Additional2Job_3dOccupation_Spouse_2003_15,Additional2Job_3dOccupation_Head_2003_15),
         Additional3Job_3dOccupation_2003_15=ifelse(relation.head %in% c(20,22), Additional3Job_3dOccupation_Spouse_2003_15,Additional3Job_3dOccupation_Head_2003_15),
         PresentOrLastMain_3dIndustry_2003_15=ifelse(relation.head %in% c(20,22), PresentOrLastMain_3dIndustry_Spouse_2003_15,PresentOrLastMain_3dIndustry_Head_2003_15),
         Additional1Job_3dIndustry_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_3dIndustry_Spouse_2003_15,Additional1Job_3dIndustry_Head_2003_15),
         Additional2Job_3dIndustry_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_3dIndustry_Spouse_2003_15,Additional2Job_3dIndustry_Head_2003_15),
         Additional3Job_3dIndustry_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_3dIndustry_Spouse_2003_15,Additional3Job_3dIndustry_Head_2003_15),
         PresentOrLastMain_Government_2003_15=ifelse(relation.head %in% c(20,22), PresentOrLastMain_Government_Spouse_2003_15,PresentOrLastMain_Government_Head_2003_15),
         PresentOrLastMain_Government_2003_15=ifelse(relation.head %in% c(20,22), PresentOrLastMain_Government_Spouse_2003_15,PresentOrLastMain_Government_Head_2003_15),
         PresentOrLastMain_Modalidad_2003_15=ifelse(relation.head %in% c(20,22),PresentOrLastMain_Modalidad_Spouse_2003_15,PresentOrLastMain_Modalidad_Head_2003_15),
         Additional1Job_Modalidad_2003_15=ifelse(relation.head %in% c(20,22),Additional1Job_Modalidad_Spouse_2003_15,Additional1Job_Modalidad_Head_2003_15),
         Additional2Job_Modalidad_2003_15=ifelse(relation.head %in% c(20,22),Additional2Job_Modalidad_Spouse_2003_15,Additional2Job_Modalidad_Head_2003_15),
         Additional3Job_Modalidad_2003_15=ifelse(relation.head %in% c(20,22),Additional3Job_Modalidad_Spouse_2003_15,Additional3Job_Modalidad_Head_2003_15),
         PresentOrLastMain_WeekHrs_2003_15=ifelse(relation.head %in% c(20,22), PresentOrLastMain_WeekHrs_Spouse_2003_15,PresentOrLastMain_WeekHrs_Head_2003_15),
         Additional1Job_WeekHrs_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_WeekHrs_Spouse_2003_15,Additional1Job_WeekHrs_Head_2003_15),
         Additional2Job_WeekHrs_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_WeekHrs_Spouse_2003_15,Additional2Job_WeekHrs_Head_2003_15),
         Additional3Job_WeekHrs_2003_15=ifelse(relation.head %in% c(20,22), Additional1Job_WeekHrs_Spouse_2003_15,Additional3Job_WeekHrs_Head_2003_15),
         HourlyEarnings_MainJob_1970_2015 = ifelse(relation.head %in% c(20,22), HourlyEarnings_MainJob_Spouse_1979_2015,HourlyEarnings_MainJob_Head_1970_2015),
         TrabajadorPerteneceUnion_1979_2015=ifelse(relation.head %in% c(20,22), TrabajadorPerteneceUnion_Spouse_1979_2015,TrabajadorPerteneceUnion_Head_1976_2015),
         TrabajoCubiertoSindicato_1979_2015=ifelse(relation.head %in% c(20,22), TrabajoCubiertoSindicato_Spouse_1979_2015,TrabajoCubiertoSindicato_Head_1976_2015),
         MainJob_Government_1979_2001=ifelse(relation.head %in% c(20,22), MainJob_Government_Spouse_1979_2001, MainJob_Government_Head_1975_2001),
         EmploymentStatus = ifelse(relation.head %in% c(20,22),EmploymentStatus_Spouse_1976_2015,EmploymentStatus_Head_1968_2015),
         HourlyEarnings_AllJobs_1968_2015 = ifelse(relation.head %in% c(20,22),HourlyEarnings_AllJobs_Spouse_1968_2015,HourlyEarnings_AllJobs_Head_1968_2015),
         YearsWithEmployer_Head_1968_2015=YearsWithEmployer_Head_1968_2015,
         Married_Pairs_Indicator_1968_2015=Married_Pairs_Indicator_1968_2015,
         Region_1968_2015=Region_1968_2015,
         YearsWithEmployer_1968_2015=YearsWithEmployer_Head_1968_2015,
         race_1968_2015=race_1968_2015,
         MainJob_Government_2003_2015=ifelse(relation.head %in% c(20,22), PresentOrLastMain_Government_Spouse_2003_15,PresentOrLastMain_Government_Head_2003_15),
         WorkExpSince18 = ifelse(relation.head %in% c(2,20,22), WorkExpSince18_Spouse, WorkExpSince18_Head),
         WorkExpSince18FullTime = ifelse(relation.head %in% c(2,20,22), WorkExpSince18FullTime_Spouse, WorkExpSince18FullTime_Head)
  )

# # Algunos otros cambios que ya no me acuerdo por qué eran. Hay que comentar antes
# panel1968_2015$PresentMainJob_3dOcc_1968_2015 <- ifelse(panel1968_2015$year %in% c(2003:2017),
#                                                         panel1968_2015$PresentOrLastMain_3dOccupation_2003_15,
#                                                         panel1968_2015$PresentMainJob_3dOcc_1968_2001)
# panel1968_2015$PresentMainJob_3dInd_1968_2015 <- ifelse(panel1968_2015$year %in% c(2003:2017),
#                                                         panel1968_2015$PresentOrLastMain_3dIndustry_2003_15,
#                                                         panel1968_2015$PresentMainJob_3dInd_1968_2001)
# panel1968_2015$EmployerYears_1968_2015 <- ifelse(panel1968_2015$year<=1993 & !panel1968_2015$EmployerYears_1968_2015 %in% c(0,998,999),
#                                                  panel1968_2015$EmployerYears_1968_2015/12,
#                                                  panel1968_2015$EmployerYears_1968_2015)
# panel1968_2015$EmployerYears_1968_2015 <- ifelse(panel1968_2015$EmployerYears_1968_2015 %in% c(0,98,99,998,999),
#                                                  NA,
#                                                  panel1968_2015$EmployerYears_1968_2015)
# panel1968_2015$Married_Pairs_Indicator_1968_2015 <- ifelse(panel1968_2015$Married_Pairs_Indicator_1968_2015 %in% c(1,2,3),1,0)
# 
# panel1968_2015$Region_1968_2015 <- ifelse(panel1968_2015$Region_1968_2015 %in% c(0,5,6,9),
#                                           NA,
#                                           panel1968_2015$Region_1968_2015)
# Elimino todo lo que ya no sirve y limpio la memoria
rm(list=ls()[!ls() %in% c("panel1968_2015","PSIDFullYears")])
gc()

# Elimino las variables que tienen nombre "head" o "spouse". Con excepción
# de relation.head
panel1968_2015 <- panel1968_2015[,
                                 !grepl(x = colnames(panel1968_2015),pattern = "*Head*|*Spouse*"),
                                 with=FALSE]

# # Elimino a los que no se les pudo hacer los retrospective files.
# # Esto lo identificamos en base a los que estaban trabajando pero NO
# # tienen información sobre la ocupación en el período 1968-1980
# panel1968_2015 <- panel1968_2015 %>%
#   filter(!(EmploymentStatus==1 & PresentMainJob_3dOcc_1968_2015==0 & year >=1968 & year <=1980))
# 
# # Levanto los Crosswalk para hacer compatibles los códigos de ocupación
# conversion <- read_excel(path = "Crosswalks.xlsx",sheet = 5)
# conversion1970_2010 <- unique(conversion[,c("1970","OCC2010","OneDigit")])
# conversion1970_2010 <- conversion1970_2010[!is.na(conversion1970_2010$`1970`),]
# conversion2000_2010 <- unique(conversion[,c("2000","OCC2010","OneDigit")])
# conversion2000_2010 <- conversion2000_2010[!is.na(conversion2000_2010$`2000`),]
# conversion2000_2010$OCC2010 <- as.numeric(conversion2000_2010$OCC2010)
# conversion2000_2010$`2000` <- as.numeric(conversion2000_2010$`2000`)
# 
# # Convierto al panel a un objeto data.table
panel1968_2015 <- as.data.table(panel1968_2015)
# 
# conversion1970_2010 <- data.table(conversion1970_2010)
# conversion1970_2010$OCC2010 <- as.numeric(conversion1970_2010$OCC2010)
# # Hago un join para el periodo 1968-2001 para pasar del CNO1970 al CNO2010
# panel1968_2015$OneDigit <- character()
# panel1968_2015$PresentMainJob_3dOcc_1968_2015_OCC2010 <- numeric()
# 
# panel1968_2015[year>=1968 & year<=2001]<-panel1968_2015[year>=1968 & year<=2001][conversion1970_2010,
#                                                                                  on=c(PresentMainJob_3dOcc_1968_2015="1970"),
#                                                                                  `:=`(OneDigit=i.OneDigit,
#                                                                                       PresentMainJob_3dOcc_1968_2015_OCC2010=OCC2010)]
# 
# # Hago un join para el periodo 2003-2015 para pasar del CNO1970 al CNO2010
# panel1968_2015[year>=2003&year<=2015]<-panel1968_2015[year>=2003&year<=2015][data.table(conversion2000_2010),
#                                                                              on=c(PresentMainJob_3dOcc_1968_2015="2000"),
#                                                                              `:=`(PresentMainJob_3dOcc_1968_2015_OCC2010=OCC2010,
#                                                                                   OneDigit=i.OneDigit)] 
# 
# conversion <- read_excel(path = "Crosswalks.xlsx",sheet = 4)
# conversion1970_1990 <- unique(conversion[,c("1970","IND1990")])
# conversion1970_1990<- data.table(conversion1970_1990) %>%
#   .[!is.na(`1970`)] %>%
#   .[,`1970`:=as.numeric(`1970`)] %>%
#   .[,IND1990:=as.numeric(IND1990)]
# conversion2000_1990 <- unique(conversion[,c("2000","IND1990")])
# conversion2000_1990<-data.table(conversion2000_1990) %>%
#   .[!is.na(`2000`)] %>%
#   .[,`2000`:=as.numeric(`2000`)] %>%
#   .[!is.na(`2000`)] %>%
#   .[,IND1990:=as.numeric(IND1990)]
# 
# panel1968_2015$PresentMainJob_3dInd_1968_2015_IND1990 <- numeric()
# # Hago un join para el periodo 1968-2001 para pasar del IND1970 al IND1990
# panel1968_2015[year>=1968 & year<=2001]<-
#   panel1968_2015[year>=1968 & year<=2001][conversion1970_1990,
#                                           on=c(PresentMainJob_3dInd_1968_2015="1970"),
#                                           PresentMainJob_3dInd_1968_2015_IND1990:=IND1990] 
# # Hago un join para el periodo 2015-2001 para pasar del IND2000 al IND1990
# panel1968_2015[year>=2003&year<=2015]<-
#   panel1968_2015[year>=2003&year<=2015][conversion2000_1990,
#                                         on=c(PresentMainJob_3dInd_1968_2015="2000"),
#                                         PresentMainJob_3dInd_1968_2015_IND1990:=IND1990] 
# # Ordenar datos
# panel1968_2015 <- panel1968_2015[order(pid,rank(year))]
