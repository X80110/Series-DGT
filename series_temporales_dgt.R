library(tidyverse)
library(httr)
library(readxl)
library(writexl)

# URL BASE
# url <- "https://www.dgt.es/Galerias/seguridad-vial/estadisticas-e-indicadores/accidentes-30dias/tablas-estadisticas/2016/GRUPO-1.-TABLAS-GENERALES_2016.xlsx"


# RESUM DEL PROCÉS PER CADA ANY
# prov_via_int <- read_excel(tf, 1L)[c(-1,-2),c(1,2:6)]                   # eliminem 2 de les files i seleccionem vies interurbanes
# colnames(prov_via_int) <- prov_via_int[1, ]                             # assignem noms de les columnes
# names(prov_via_int)[1] <- "Provincia"                                   # posem nom a la 1a columna
# df_prov_via_int <- prov_via_int[-1,]                                    # ens carreguem la fila amb els noms
# df_prov_via_int[,2:6] <-  sapply(df_prov_via_int[,2:6], as.numeric)     # convertim a format numèric
# df_prov_via_int$año = year                                              # assignem any al conjunt

# FALTA TREURE TOTAL
get_series <- function(year_list){
  df <- data.frame(NULL)      
  for (year in year_list) { 
      
    # url per cada any
      url <- paste0("https://www.dgt.es/Galerias/seguridad-vial/estadisticas-e-indicadores/accidentes-30dias/tablas-estadisticas/",year,"/GRUPO-1.-TABLAS-GENERALES_",year,".xlsx")
      httr::GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
      
    # llegim 1r full de càlcul, seleccionem files que interessen assignem columnes
      provincias <- read_excel(tf, 1L)[c(-1,-2),]#,c(1,2:6)]                
      colnames(provincias) <- provincias[1, ]                             
      names(provincias)[1] <- "Provincia"                                 
      df_provincias <- provincias[-1,] 
      
    # passem a format numèric
      df_provincias[,2:length(df_provincias)] <-  sapply(df_provincias[,2:length(df_provincias)], as.numeric)     
    
    # assignem la categoria de tipus de via i les reunim sota el mateix dataset
      vias_interurbanas <- df_provincias %>% select(c(1,2:6)) %>% filter(Provincia != "Total") 
      colnames(vias_interurbanas) <-(c("provincia", "accidentes_victimas","accidentes_mortales","fallecidos","heridos_hospitalizados","heridos_no_hospitalizados"))
      vias_interurbanas$tipo_via <- "Vias interurbanas"
      
    
      vias_urbanas <- df_provincias %>% select(c(1,7:11)) %>% filter(Provincia != "Total")
      colnames(vias_urbanas) <-(c("provincia", "accidentes_victimas","accidentes_mortales","fallecidos","heridos_hospitalizados","heridos_no_hospitalizados"))
      vias_urbanas$tipo_via <- "Vias urbanas"
    # assignem any
      df_year <- rbind(vias_urbanas, vias_interurbanas)
      df_year$año = year
      df <- rbind(df_year, df)
  }
  return(df)
  
}
years <- seq(from=2015, to= 2019)
dataset <- get_series(years)

write_xlsx(dataset, "dgt_accidentes_general_2015_2019.xlsx")

# llegir les altres fulles de càlcul
# df_prov_via_urb <- read_excel(tf, 1L)[c(-1,-2),c(1,7:11)]
# df_ccaa_via <- read_excel(tf, 2L)
# df_tipo_via <- read_excel(tf, 3L)
# df_ilum_via <- read_excel(tf, 4L)




