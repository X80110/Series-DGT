library(tidyverse)
library(httr)
library(readxl)
library(writexl)

# URL BASE
# ____________
# url <- "https://www.dgt.es/Galerias/seguridad-vial/estadisticas-e-indicadores/accidentes-30dias/tablas-estadisticas/2019/GRUPO-1.-TABLAS-GENERALES_2019.xlsx"


# ESBÓS ETL PER A CADA ANY
# ____________
# prov_via_int <- read_excel(tf, 1L)[c(-1,-2),c(1,2:6)]                   # eliminem 2 de les files i seleccionem vies interurbanes
# colnames(prov_via_int) <- prov_via_int[1, ]                             # assignem noms de les columnes
# names(prov_via_int)[1] <- "Provincia"                                   # posem nom a la 1a columna
# df_prov_via_int <- prov_via_int[-1,]                                    # ens carreguem la fila amb els noms
# df_prov_via_int[,2:6] <-  sapply(df_prov_via_int[,2:6], as.numeric)     # convertim a format numèric
# df_prov_via_int$año = year                                              # assignem any al conjunt


get_location_series <- function(year_list){
  df <- data.frame(NULL)
  
  # PER A CADA ANY ITEREM:
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
      
      # (1) assignem la categoria de tipus de via, unifiquem noms de columna, eliminem totals i (2) les reunim sota el mateix dataset
        # (1)...
        vias_interurbanas <- df_provincias %>% select(c(1,2:6)) %>% filter(Provincia != "Total") 
        colnames(vias_interurbanas) <-(c("provincia", "accidentes_victimas","accidentes_mortales","fallecidos","heridos_hospitalizados","heridos_no_hospitalizados"))
        vias_interurbanas$tipo_via <- "Vias interurbanas"
        
      
        vias_urbanas <- df_provincias %>% select(c(1,7:11)) %>% filter(Provincia != "Total")
        colnames(vias_urbanas) <-(c("provincia", "accidentes_victimas","accidentes_mortales","fallecidos","heridos_hospitalizados","heridos_no_hospitalizados"))
        vias_urbanas$tipo_via <- "Vias urbanas"
      
        # (2)... també assignem any i agrupem categories
        df_year <- rbind(vias_urbanas, vias_interurbanas)
        df_year$año = year
        
      # acumulem sota el mateix dataset les dades de les files de cada iteració, és a dir, de cada any
        df <- rbind(df_year, df)
      }
  return(df)       # <----- output
  
}
years <- seq(from=2015, to=2019)
dataset <- get_location_series(years)

write_xlsx(dataset, "dgt_accidentes_general_2015_2019.xlsx")
write.csv(dataset, "dgt_accidentes_general_2015_2019.csv")



# TODO
  # CREAR get_type_series <- function(years_list) { ... }  
    # df_ccaa_via <- read_excel(tf, 2L) #TODO no captar de l'excel, buscar relació ccaa <-> prov. i assignar

  # CREAR get_day_period_series <- function(years_list) { ... }  
    # df_tipo_via <- read_excel(tf, 3L) #TODO fer el mateix que amb el general canviant provincia per tipo_accidente

  # CREAR get_day_period_series <- function(years_list) { ... } 
    # df_ilum_via <- read_excel(tf, 4L) #TODO pivotar il·luminació i tipus vies (urbana i interurbana), segons tipus d'accident -.- cols(tipo_via_inter>>c(autopista, autovía, via convencional, camino vecinal, via de servicio, ramal de enlace, otro)), tipo_via_urb>>c(calle,travesia)) i tipus d'accident




