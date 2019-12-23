---
title: "Proyecto Final: Definición"
author: "Jordi Casulleras, Nadal Comparini, Julián Rocabruna"
output: 
  html_document: 
    keep_md: yes
    number_sections: yes
    toc: yes
  pdf_document: 
    number_sections: yes
    toc: yes
---



# Introducción: Physionet 2012 UCI data mortality

En el concurso del congreso ["Computers in  Cardiology" (ahora "Computing in Cardiology") del año 2012](https://physionet.org/content/challenge-2012/) propuso un  caso de estudio como reto: *Predicción de la tasa de mortalidad de los pacientes de una UCI*

Resto de años mas recientes 

* https://physionet.org/content/challenge-2018/
* https://physionet.org/content/challenge-2019/



##   Enlaces de interés

[**HR**: Heart Rate bpm beats per minut](https://en.wikipedia.org/wiki/Heart_rate)

[**GCS**: Glasgow Comma Score (scale 3-15)](https://en.wikipedia.org/wiki/Glasgow_Coma_Scale)

[**RespRate**:  Respiration rate (bpm) breaths for one minute](https://en.wikipedia.org/wiki/Respiratory_rate)


# The Research Question

Con los datos que disponemos, se puede predecir "la muerte en hospital" (in hospital death) de muchas formas. Pero en nuestro caso se ha decidio hacer algo un tanto distinto. Se intentará averiguar la influencia de los distintos órganos relacionados con nuestras variables con el fallecimiento en el hospital. Es decir, se tratará de definir en que medida los problemas derivados de un órgano en concreto se encuentran relacionadas con la muerte en hospital.

Para ello, se clasificarán las variables en grupos según al órgano al que pertenezcan. Una vez se tengan los grupos, se escogerá el modelo adecuado y se predecirá la variable "muerte en hospital" con cada grupo de variables. El grupo con mayor porcentaje de aciertos será el órgano que tenga más probabilidades de ser un órgano crítico.


# Ingesta de datos

Después de hacer un ejercicio de investigación, se han detectado 5 grupos de variables, y por tanto 5 órganos, mayoritarios:

## Variables relacionados con el **corazón**:

*  **AST.** Aspartate transaminase
*  **DiasABP.** Invasive diastolic arterial blood pressure
*  **HR.** Hearth Rate
*  **MAP.** Invasive mean arterial blood pressure
*  **NIDiasABP.** Non-invasive diastolic arterial blood pressure
*  **NIMAP.** Non-invasive mean arterial blood pressure
*  **NISysABP.** Non-invasive systolic arterial blood pressure
*  **PaCO2.** Partial pressure of arterial CO2
*  **PaO2.**	Partial pressure of arterial O2
*  **pH.**	Arterial pH
*  **SysABP.** Invasive systolic arterial blood pressure
*  **TropT.** Troponin-T

## Variables relacionadas con el **hígado**:

* **Albumin.** Albumin
* **ALP.** Alkaline phosphatase
* **ALT.** Alanine transaminase
* **AST.** Aspartate transaminase
* **Bilirubin.** Bilirubin
* **Cholesterol.** Cholesterol

## Variables relacionadas con el **riñón**:

* **ALP.** Alkaline phosphatase
* **BUN.** Blood urea nitrogen
* **Creatinine.**	Creatinine
* **HCO3.**	Serum bicarbonate
* **Urine.** Urine

## Variables relacionadas con la **sangre**:

* **HCT.** Hematocrit
* **Platelets.** Platelets
* **WBC.** White blood cell count

## Variables relacionadas con los **pulmones**:

* **FiO2.**	Fractional inspired O2
* **RespRate.**	Respiration rate
* **Sao2.** O2 saturation in hemoglobin

Las variables restantes que no aparecen entre estos grupos han sido inicialmente descartadas debido a que no ha sido posible asociarles algún órgano en concreto o que el grupo formado ha sido realmente minoritario. No se descarta que durante el transcurso del desarrollo del proyecto se utilice alguna de ellas para reforzar los sistemas según los resultados de estos.

Además, de las variables descritas no se van a utilizar muchas de ellas debido a la ausencia de estas en muchos pacientes. Para determinar el porcentaje de valores NAs de cada variable se ha utilizado una pequeño función.

## Funcion de cálculo de valores no asignados (NAs)

```r
PercentageNA <- function(df){
  na <- vector("double", ncol(df))
  names(na) = colnames(df)
  for (i in seq_along(df)) {
    na[[i]] = sum(is.na(df[i])) / nrow(df)
  }

  return (data.frame(na))
}
```

# Modelo de datos 

Con tal de recoger los datos, se utilizará el código que nos facilitó el profesor Ricardo.

La idea es generar tantos datasets como órganos queramos comparar. Todavía no se ha llegado a completar toda la ingesta de datos, pero la idea es utilizar un método similar al que planteó Ricardo. A continuación se muestra el código del profesor modificado para generar un dataset con las variables relacionadas con la sangre.


```r
path="data_basic_physionet/set-a/"# path training
lista_pacientes_set_a=dir(path) # lista  ficheros pacientes 
length(lista_pacientes_set_a) # número pacientes en training
```

```
## [1] 4000
```


```
data_paciente_132539=read_csv("data_basic_physionet/set-a/132539.txt", col_types =cols(Time=col_time(format="%M:%S"),Parameter=col_character(),
Value=col_double()))
str(data_paciente_132539)
glimpse(data_paciente_132539)
class(data_paciente_132539)
head(data_paciente_132539,30)
```

## Carga set_a


```r
# lista path's  a cada  ficjero de paciente
list_files=paste0(path,lista_pacientes_set_a)
# Función leer paciente
leer_paciente=function(file) {
  read_csv(file, col_types =cols(Time=col_character(),
                                 Parameter= col_character(),
                                 Value=col_double())) %>%
    separate(Time,into=c("H","M"),sep=":") %>% 
    mutate(Time_Minutes=as.numeric(H)*60+as.numeric(M)) %>% 
    select(Time_Minutes,Parameter,Value)
}

#leer_paciente(list_files[1])

raw_data=lapply(list_files,leer_paciente)# lista de los datos por paciente

#extraer perfiles "RecordID" "Age"      "Gender"   "Height"   "Weight"   "ICUType" 
perfil=function(data_paciente){
  data_paciente %>% filter(Parameter %in% c("RecordID", "Age", "Gender", "Height", "ICUType", "Weight")) %>% select(-Time_Minutes) %>% distinct(Parameter,.keep_all=TRUE) %>% spread(Parameter,Value)
}
## ejemplo
#perfil(data_paciente_132539)
## Guardo  todos los datos  del perfil de cada paciente
perfiles=lapply(raw_data,perfil)%>% bind_rows() %>% select(RecordID, Age, Gender, Height,Weight,ICUType)
glimpse(perfiles)
```

```
## Observations: 4,000
## Variables: 6
## $ RecordID <dbl> 132539, 132540, 132541, 132543, 132545, 132547, 13254...
## $ Age      <dbl> 54, 76, 44, 68, 88, 64, 68, 78, 64, 74, 64, 71, 66, 8...
## $ Gender   <dbl> 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,...
## $ Height   <dbl> -1.0, 175.3, -1.0, 180.3, -1.0, 180.3, 162.6, 162.6, ...
## $ Weight   <dbl> -1.0, 76.0, 56.7, 84.6, -1.0, 114.0, 87.0, 48.4, 60.7...
## $ ICUType  <dbl> 4, 2, 3, 3, 3, 1, 3, 3, 3, 2, 3, 2, 3, 1, 1, 2, 3, 3,...
```

```r
## Ler series
## se modifica error de time

serie_UCI_parameter<-  function(paciente,parameters){
  paciente %>% arrange(Parameter,Time_Minutes) %>% filter(Parameter %in% parameters) %>% add_column(RecordID=paciente[1,3]$Value) 
  } 

##ejemplo
v = c("HCT",
"Platelets",
"WBC")
parameters=v
serie_paciente1 =serie_UCI_parameter(raw_data[[1]],parameters)
serie_paciente1
```

```
## # A tibble: 7 x 4
##   Time_Minutes Parameter Value RecordID
##          <dbl> <chr>     <dbl>    <dbl>
## 1          188 HCT        33.7   132539
## 2          637 HCT        33.5   132539
## 3         1987 HCT        30.3   132539
## 4          637 Platelets 221     132539
## 5         1987 Platelets 185     132539
## 6          637 WBC        11.2   132539
## 7         1987 WBC         9.4   132539
```

```r
# paso parámetros y  apilo 
parameters=v
series_parameters = lapply(raw_data,FUN=function(x) serie_UCI_parameter(x,parameters)) %>% bind_rows()
glimpse(series_parameters)
```

```
## Observations: 45,285
## Variables: 4
## $ Time_Minutes <dbl> 188, 637, 1987, 637, 1987, 637, 1987, 71, 146, 30...
## $ Parameter    <chr> "HCT", "HCT", "HCT", "Platelets", "Platelets", "W...
## $ Value        <dbl> 33.7, 33.5, 30.3, 221.0, 185.0, 11.2, 9.4, 24.7, ...
## $ RecordID     <dbl> 132539, 132539, 132539, 132539, 132539, 132539, 1...
```

## En resumen  tenemos


```r
#set-a
glimpse(perfiles)
```

```
## Observations: 4,000
## Variables: 6
## $ RecordID <dbl> 132539, 132540, 132541, 132543, 132545, 132547, 13254...
## $ Age      <dbl> 54, 76, 44, 68, 88, 64, 68, 78, 64, 74, 64, 71, 66, 8...
## $ Gender   <dbl> 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,...
## $ Height   <dbl> -1.0, 175.3, -1.0, 180.3, -1.0, 180.3, 162.6, 162.6, ...
## $ Weight   <dbl> -1.0, 76.0, 56.7, 84.6, -1.0, 114.0, 87.0, 48.4, 60.7...
## $ ICUType  <dbl> 4, 2, 3, 3, 3, 1, 3, 3, 3, 2, 3, 2, 3, 1, 1, 2, 3, 3,...
```

```r
glimpse(series_parameters)
```

```
## Observations: 45,285
## Variables: 4
## $ Time_Minutes <dbl> 188, 637, 1987, 637, 1987, 637, 1987, 71, 146, 30...
## $ Parameter    <chr> "HCT", "HCT", "HCT", "Platelets", "Platelets", "W...
## $ Value        <dbl> 33.7, 33.5, 30.3, 221.0, 185.0, 11.2, 9.4, 24.7, ...
## $ RecordID     <dbl> 132539, 132539, 132539, 132539, 132539, 132539, 1...
```

## Unificar: series, perfiles y scores

Nos faltan los scores clásicos que se utilizan eb las ICU. Estos ewstán el fichero Outcome-a.txt para el set-a


```r
scoresApath="data_basic_physionet/Outcomes-a.txt"
scoresA=read_csv(scoresApath)
```

```
## Parsed with column specification:
## cols(
##   RecordID = col_double(),
##   `SAPS-I` = col_double(),
##   SOFA = col_double(),
##   Length_of_stay = col_double(),
##   Survival = col_double(),
##   `In-hospital_death` = col_double()
## )
```

```r
glimpse(scoresA)
```

```
## Observations: 4,000
## Variables: 6
## $ RecordID            <dbl> 132539, 132540, 132541, 132543, 132545, 13...
## $ `SAPS-I`            <dbl> 6, 16, 21, 7, 17, 14, 14, 19, 11, 14, 15, ...
## $ SOFA                <dbl> 1, 8, 11, 1, 2, 11, 4, 8, 0, 6, 2, 7, 2, 7...
## $ Length_of_stay      <dbl> 5, 8, 19, 9, 4, 6, 9, 6, 17, 8, 13, 7, 22,...
## $ Survival            <dbl> -1, -1, -1, 575, 918, 1637, -1, 5, 38, -1,...
## $ `In-hospital_death` <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ...
```

```r
Scores_perfilesA= inner_join(perfiles,scoresA,"RecordID")
glimpse(Scores_perfilesA)
```

```
## Observations: 4,000
## Variables: 11
## $ RecordID            <dbl> 132539, 132540, 132541, 132543, 132545, 13...
## $ Age                 <dbl> 54, 76, 44, 68, 88, 64, 68, 78, 64, 74, 64...
## $ Gender              <dbl> 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, ...
## $ Height              <dbl> -1.0, 175.3, -1.0, 180.3, -1.0, 180.3, 162...
## $ Weight              <dbl> -1.0, 76.0, 56.7, 84.6, -1.0, 114.0, 87.0,...
## $ ICUType             <dbl> 4, 2, 3, 3, 3, 1, 3, 3, 3, 2, 3, 2, 3, 1, ...
## $ `SAPS-I`            <dbl> 6, 16, 21, 7, 17, 14, 14, 19, 11, 14, 15, ...
## $ SOFA                <dbl> 1, 8, 11, 1, 2, 11, 4, 8, 0, 6, 2, 7, 2, 7...
## $ Length_of_stay      <dbl> 5, 8, 19, 9, 4, 6, 9, 6, 17, 8, 13, 7, 22,...
## $ Survival            <dbl> -1, -1, -1, 575, 918, 1637, -1, 5, 38, -1,...
## $ `In-hospital_death` <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, ...
```

### Extracción factores de las series 

genero una tabla con resumenes de  las variables por paciente: media, desviación típica 


```r
series_summary=series_parameters %>%
  group_by(RecordID,Parameter) %>%
  summarise(count=n(), mean=mean(Value,na.rm=TRUE), sd=sd(Value,na.rm=TRUE))%>%
  gather(Stat, Value, count:sd) %>%
  ungroup() %>%
  transmute(RecordID, ParameterStat=paste0(Parameter,"_",Stat), Value) %>%
  spread(ParameterStat, Value)
```



```r
data_tidy=Scores_perfilesA %>% inner_join(series_summary)
```

```
## Joining, by = "RecordID"
```

```r
head(data_tidy)
```

```
## # A tibble: 6 x 20
##   RecordID   Age Gender Height Weight ICUType `SAPS-I`  SOFA Length_of_stay
##      <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>    <dbl> <dbl>          <dbl>
## 1   132539    54      0    -1    -1         4        6     1              5
## 2   132540    76      1   175.   76         2       16     8              8
## 3   132541    44      0    -1    56.7       3       21    11             19
## 4   132543    68      1   180.   84.6       3        7     1              9
## 5   132545    88      0    -1    -1         3       17     2              4
## 6   132547    64      1   180.  114         1       14    11              6
## # ... with 11 more variables: Survival <dbl>, `In-hospital_death` <dbl>,
## #   HCT_count <dbl>, HCT_mean <dbl>, HCT_sd <dbl>, Platelets_count <dbl>,
## #   Platelets_mean <dbl>, Platelets_sd <dbl>, WBC_count <dbl>,
## #   WBC_mean <dbl>, WBC_sd <dbl>
```

### Porcentaje NAs


```r
PercentageNA(data_tidy)
```

```
##                             na
## RecordID          0.0000000000
## Age               0.0000000000
## Gender            0.0000000000
## Height            0.0000000000
## Weight            0.0000000000
## ICUType           0.0000000000
## SAPS-I            0.0000000000
## SOFA              0.0000000000
## Length_of_stay    0.0000000000
## Survival          0.0000000000
## In-hospital_death 0.0000000000
## HCT_count         0.0002540005
## HCT_mean          0.0002540005
## HCT_sd            0.0172720345
## Platelets_count   0.0012700025
## Platelets_mean    0.0012700025
## Platelets_sd      0.0251460503
## WBC_count         0.0025400051
## WBC_mean          0.0025400051
## WBC_sd            0.0287020574
```

En este caso, el porcentaje de NAs de las variables relacionadas con la sangre es bastante bajo.

# Conclusión

En definitiva, tras realizar una primera inspección de los datos se ha decidido optar por la obtención de resultados según la agrupación por órganos. Tras debatir entre nosotros cúal podría resultar ser la pregunta de investigación más apropiada nos han surgido diferentes opciones:

* **¿Que influencia tiene {órgano en concreto} sobre el fallecimiento en hospital?**
* **¿Es un grupo de órganos específico el causante del fallecimiento en hospital?**

Básicamente las dos preguntas son muy similares y el punto crucial que comparten es la agrupación de variables en órganos que será en lo que nos centremos durante todo el proyecto.
Para concluir, no hemos querido seleccionar a estas alturas una sola pregunta de investigación porque queremos ver como se comportan ambas opciones al realizar el desarrollo del proyecto. Además, creemos que de esta forma sabemos hacia donde deben ir dirigidos nuestros primeros pasos pero se nos permite un cierto margen de flexibilidad.

Así pues, aunque no conozcamos todavía la pregunta exacta se podría resumir nuestro proyecto en que se tratará de llevar a cabo un análisis de los causantes de mortalidad en hospital según el órgano afectado.

