colnames(parcial_data) <- parcial_clean_colnames(colnames)
rawdata<-parcial_data
rawdata
#Diagnóstico

# Limpieza General
  
problematic_rows <- readr::problems(parcial_data)$row

parcial_data[problematic_rows,] <- parcial_data %>% 
  slice(problematic_rows) %>% 
  unite(col="all", -seq(1:13), sep = "/", remove=TRUE) %>% 
  extract(all, into=c("curb_weight", "engine_type", "resto"), regex="([0-9]{4})([a-z]+)/(.*)/NA", remove=TRUE) %>% 
  separate(resto, into=names(parcial_data)[16:26], sep="/", remove=TRUE) %>% 
  readr::type_convert() %>%
  mutate(across(c("wheel_base",
                  "price",
                  "peak_rpm",
                  "horsepower",
                  "engine_size",
                  "curb_weight"),
                as.character))


#Corrección puntual
parcial_data$'wheel_base'[157]=95.7
parcial_data$'curb_weight' <- substr(parcial_data$'curb_weight', 0, 4)

#Formato a NA
parcial_data[parcial_data == "?"] <- NA


#Formato general adecuado por clase
parcial_data<- readr::type_convert(parcial_data)




