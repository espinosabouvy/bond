

limpiar.materiales <- function(){
     setwd("~/Magro/Clientes/Bond/Bond - en proceso")
     
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(stringi)
  
     '%!in%' <- function(x,y)!('%in%'(x,y))
     #unique(df[c("yad", "per")])
     
  ruta <- "~/Magro/Clientes/Bond/Bond - en proceso"
  datos.raw <- read.xlsx(paste0(ruta,"/Lista de Materiales y Proveedores.xlsx"),sheet = "BOM")
  
  names(datos.raw) <- make.names(names(datos.raw),unique = T)%>%
       tolower()
  
  datos <- datos.raw%>%
       select(codigo.unico = sys.code, codigo = codigo...bond, nombre_material_bond, talla = hijitos, 
              asp.linea = asp.línea.5., unidad = un.ent, linea = línea_2, alias, 
              obervaciones = tips.material..30.)%>%
       arrange(nombre_material_bond)
  
  # #unicas
  # length(unique(datos$sys.code))  #todos los syscode son únicos
  # length(unique(datos$codigo))
  # length(unique(datos$nombre_material_bond))
  # 
  # #syscode repetidos  (sys code es valor unico)
  # datos%>%
  #      group_by(sys.code)%>%
  #      summarise(f= n())%>%
  #      filter(f>1)
  
  #quitar distinto código y sin talla (000)
  sin.talla <- datos%>%
       filter(talla == "000")%>%   #las cajas de envase en ese momento quedaron aqui y otras con talla
       mutate(tipo = "Unico-unitalla")%>%
       select(codigo.unico, tipo)
  
  con.talla <- datos%>%
       filter(codigo.unico %!in% sin.talla$codigo.unico)
  
       #un codigo, varias tallas
       unicos <- con.talla%>%
            group_by(codigo)%>%
            mutate(f=n())%>%
            filter(f>1)%>%
            mutate(tipo = "Multiple-tallas")%>%
            ungroup()%>%
            select(codigo.unico, tipo)
  
       varios.codigos.con.talla <- con.talla%>%
            filter(codigo.unico %!in% unicos$codigo.unico)%>%
            mutate(tipo = "Unico-tallas-error")%>%
            select(codigo.unico, tipo)
       
       tipo <- rbind(sin.talla, unicos, varios.codigos.con.talla)
       
  
  #corregir nombres, para tener un nombre comun y varias tallas
  
  datos.new <<- datos%>%
       left_join(tipo, by = "codigo.unico")%>%
       mutate(nuevo.nombre = ifelse(tipo == "Multiple-tallas",
              nombre_material_bond%>%
                   toupper%>%
                   trimws(which = "both")%>%
                   gsub('(\\s(CHICO|MEDIANO|GRANDE)|(\\s\\((CH|MED|M|CH|G|GDE)\\))|(\\sT\\s\\d*\\s?(\\(.\\))?\\s?½?)|(\\s?\\#\\s?\\d*(\\-\\d*½)?)|(\\s\\"\\d*\\s*\\d*\\")|(\\s\\dM))$',"",.), 
              nombre_material_bond))%>%
       mutate(nueva.talla = ifelse(tipo == "Unico-tallas-error", "000", talla))
  
  #mismo código, un solo nombre
  
}
