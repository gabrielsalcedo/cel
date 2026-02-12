library(dplyr)        # Para manipulación de datos
library(tidyr)        # Para transformar datos entre formatos ancho/largo
library(stringr)      # Para manejo de texto
library(readxl)       # Para leer archivos Excel
library(httr)         # Para descargar archivos desde la web
library(openxlsx)     # Para guardar archivos Excel
library(janitor)
library(writexl)

url <- "https://raw.github.com/gabrielsalcedo/cel/446c4b31e785aabce2125f7d439111c4fb3c1310/Base_Assesment.xlsx"

temp_file <- tempfile(fileext = ".xlsx")     # Crear archivo temporal
download.file(url, temp_file, mode = "wb")   # Descargar archivo Excel

df <- read_excel(temp_file)

df <- df %>% janitor::clean_names()

#Modificamos tipo de variable

df <- df %>%
  mutate(
    documento_identidad = as.character(documento_identidad),
    snies_prog = as.character(snies_prog),
    fecha_de_nacimiento = as.character(fecha_de_nacimiento)
  )

#Verificamos datos nulos

nas <- df %>%
  filter(if_any(everything(), is.na))

#Como hay bastantes nulos en 'categoria_fondo' asumimos el NA como 0 sí en 'fondo' aparece Regular

nas <- nas %>%
  mutate(categoria_fondo = ifelse(is.na(categoria_fondo) & fondo == "Regular",
                                  0,
                                  categoria_fondo))

#Volvemos a filtrar datos nulos

nas <- nas %>%
  filter(if_any(everything(), is.na))

#A pesar de tener NA en columnas: 'departamento_de_nacimiento' y 'ciudad_municipio_de_nacimiento'
#no eliminamos las filas puesto que las variables que utilizaremos para gráficar no se ven afectadas
#nos piden hacer la relación con procedencia geográfica de los estudiantes no lugar de nacimiento.

#Estudiante con ID 17853 se observa con NA en 'fecha_de_nacimiento', lo cual es irrelevante para lo solicitado

#Repetimos linea 33 para ver los cambios en df

df <- df %>%
  mutate(categoria_fondo = ifelse(is.na(categoria_fondo) & fondo == "Regular",
                                  0,
                                  categoria_fondo))

#Dejar datos de todas las variables tipo character en mayuscula

df <- df %>%
  mutate(across(where(is.character), str_to_upper))

#---------------
#INCONSISTENCIAS
#---------------

#Estudiante ID 5316 se encuentra incoherencia entre edad y rango

incon_1 <- df  %>%
  filter(edad==6)

#Como rango de edad = mayor de 42 años se modifica la edad. Pasa de 6 a 60

df <- df %>%
  mutate(edad = ifelse(edad == 6,
                       60,
                       edad))

#Rectificamos que se haya hecho la modificacion

rect_1 <- df %>%
  filter(documento_identidad==5316)






#Exportar base limpia a Excel

#write_xlsx(df, "base_limpia.xlsx")

