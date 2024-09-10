#carregar llibreries
library(tidyverse)
library(dplyr)

# Carregar els fitxers CSV
file_path <- "/Users/ganguera/Desktop/TFM/data/ALL/CMA4_201216_093340.csv"
fitxer1 <- read.csv(file_path, skip = 8, header = TRUE)

################################ LLEGIM I ARREGLEM EL 1R FITXER ##########################

# Llegeix les primeres 8 files com a header
header <- read.csv(file_path, nrows = 8, header = FALSE)

# Llegeix les dades a partir de la fila 9
data <- read.csv(file_path, skip = 8, header = TRUE)

# Si vols veure el header com a variables amb els valors
header_clean <- header %>%
  apply(2, function(x) {
    sapply(x, function(y) {
      parts <- strsplit(as.character(y), ":")[[1]]
      if (length(parts) == 2) {
        return(parts[2])  # Agafa el valor després dels dos punts
      } else {
        return(NA)  # Si no troba els dos punts, retorna NA
      }
    })
  }) %>%
  as.data.frame()

# Transposar el dataframe header_clean
header_transposed <- t(header_clean)

# Convertir el resultat transposat a un dataframe
header_transposed <- as.data.frame(header_transposed, stringsAsFactors = FALSE)

# Canviar els noms de les columnes eliminant tot el que hi ha després dels ":"
colnames(header_transposed) <- sapply(colnames(header_transposed), function(x) {
  parts <- strsplit(x, ":")[[1]]
  return(parts[1])  # Manté només la part abans dels dos punts
})

# Filtrar les variables del header transposat
header_filtered <- header_transposed %>%
  select(ID, GENDER, AGE, HEIGHT, WEIGHT)

# Seleccionar les variables TIME i EVENT del dataframe 'data'
data_filtered <- data %>%
  select(TIME, EVENT)

# Combinar les dades del header filtrat amb les dades de TIME i EVENT
combined_df <- cbind(header_filtered, data_filtered)

############################# LLEGIM I ARREGLEM EL 2N FITXER #####################################
fitxer2 <- read.csv("CMA4_201216_093340_EEG.csv", skip=5)

# Comprovar el nombre de files de cada fitxer
n_rows_fitxer1 <- nrow(fitxer1)
n_rows_fitxer2 <- nrow(fitxer2)

# Si el primer fitxer té menys files, afegim NA fins que tinguin el mateix nombre de files
if (n_rows_fitxer1 < n_rows_fitxer2) {
  files_a_afegir <- n_rows_fitxer2 - n_rows_fitxer1
  files_na <- data.frame(matrix(NA, nrow = files_a_afegir, ncol = ncol(fitxer1)))
  colnames(files_na) <- colnames(fitxer1)
  fitxer1 <- rbind(fitxer1, files_na)
}

# Copiar la columna EVENT del primer fitxer al segon
fitxer2$EVENT <- fitxer1$EVENT

# Comprovar si la columna EVENT està buida i substituir valors buits per NA
fitxer2$EVENT[fitxer2$EVENT == ""] <- NA

# Substituir els valors NA per 0 només a les columnes EEG1_WAV i EEG2_WAV
fitxer2$EEG1_WAV[is.na(fitxer2$EEG1_WAV)] <- 0
fitxer2$EEG2_WAV[is.na(fitxer2$EEG2_WAV)] <- 0

# Afegir una columna de temps al segon fitxer, sumant 1 per cada fila
fitxer2$Time_s <- 1:nrow(fitxer2)

# Identificar la fila on hi ha l'esdeveniment "verbal" a la columna EVENT
event_row <- which(fitxer2$EVENT == "verbal")

# Verificar el valor de Time_s en la fila de l'esdeveniment "verbal"
print(paste("Esdeveniment 'verbal' trobat a temps:", fitxer2$Time_s[event_row]))

# Filtrar valors d'EEG1_WAV i EEG2_WAV que no estiguin entre -30 i -8 o entre 8 i 30
fitxer2$EEG1_WAV_filtered <- ifelse((fitxer2$EEG1_WAV >= 8 & fitxer2$EEG1_WAV <= 30) |
                                      (fitxer2$EEG1_WAV <= -8 & fitxer2$EEG1_WAV >= -30),
                                    fitxer2$EEG1_WAV, NA)

fitxer2$EEG2_WAV_filtered <- ifelse((fitxer2$EEG2_WAV >= 8 & fitxer2$EEG2_WAV <= 30) |
                                      (fitxer2$EEG2_WAV <= -8 & fitxer2$EEG2_WAV >= -30),
                                    fitxer2$EEG2_WAV, NA)

# Eliminar les files amb valors NA per les variables EEG1, EEG2
cleaned_datag <- na.omit(combined_df)

# Afegir una columna de temps suposant 1 segon entre cada punt
cleaned_datag$Time_s <- 1:nrow(cleaned_datag)

# Identificar la fila on hi ha l'esdeveniment "verbal" a la columna EVENT
event_row <- which(cleaned_datag$EVENT == "verbal")

############################# GRAFIQUEM ELS RESULTATS ###################################

# Crear el gràfic de línies per EEG1 i EEG2 amb l'esdeveniment "verbal" marcat
g1 <- ggplot(fitxer2, aes(x = Time_s)) +
  geom_line(aes(y = EEG1_WAV_filtered, color = "EEG1")) +
  geom_line(aes(y = EEG2_WAV_filtered, color = "EEG2")) +
  geom_point(data = fitxer2[event_row, ], aes(x = Time_s, y = EEG1_WAV), color = "black", size = 3, shape = 18) +
  geom_point(data = fitxer2[event_row, ], aes(x = Time_s, y = EEG2_WAV), color = "green", size = 3, shape = 18) +
  labs(title = "EEG alpha waves with unconciousness event marked", x = "Time (s)", y = "EEG Signal") +
  scale_color_manual(name = "EEG", values = c("EEG1" = "blue", "EEG2" = "red")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +  # Mostrar l'eix X com a números lineals
  coord_cartesian(xlim = c(150, 500), ylim = c(0, 30)) +  # Zoom en l'eix X entre 0 i 300 segons i Y entre -30 i 30
  theme_minimal()



# Afegir el text del pacient al gràfic com una anotació
# Convertir el dataframe a cadena
header_filtered_string <- paste(
  "ID:", header_filtered$ID, "\n",
  "GENDER:", header_filtered$GENDER, "\n",
  "AGE:", header_filtered$AGE, "\n",
  "HEIGHT:", header_filtered$HEIGHT, "\n",
  "WEIGHT:", header_filtered$WEIGHT)

g1 + annotate("text", x = max(fitxer2$Time_s) * 0.7, y = max(fitxer2$EEG1_WAV), 
             label = header_filtered_string, hjust = 0, vjust = 1, size = 4, color = "black")


### GRÀFIC DE DISPERSIÓ ###

# Identificar la fila on hi ha l'esdeveniment "verbal" a la columna EVENT
event_row <- which(fitxer2$EVENT == "verbal")

# Create scatter plot with EEG1 in blue and EEG2 in red
ggplot(cleaned_datag, aes(x = Time_s)) +
  geom_point(aes(y = EEG1_WAV), color = "blue", size = 1) +
  geom_point(aes(y = EEG2_WAV), color = "red", size = 1) +
  labs(title = "EEG Data Points Over Time", x = "Time (s)", y = "EEG Signal") +
  theme_minimal()

#################### GRÀFIC DE LINIES #####################

# Carregar el fitxer modificat (segon fitxer amb la columna EVENT i NA substituïts per 0)
fitxer3 <- read.csv("paacient1.csv")

# Identificar la fila on hi ha l'esdeveniment "verbal" a la columna EVENT
event_row <- which(fitxer3$EVENT == "verbal")

# Verificar el valor de Time_s en la fila de l'esdeveniment "verbal"
print(fitxer3$Time_s[event_row])

# Crear el gràfic de línies per EEG1 i EEG2 amb l'esdeveniment "verbal" marcat
ggplot(fitxer3, aes(x = Time_s)) +
  geom_line(aes(y = EEG1_WAV, color = "EEG1")) +
  geom_line(aes(y = EEG2_WAV, color = "EEG2")) +
  geom_point(data = fitxer3[event_row, ], aes(x = Time_s, y = EEG1_WAV), color = "black", size = 3, shape = 18) +
  geom_point(data = fitxer3[event_row, ], aes(x = Time_s, y = EEG2_WAV), color = "black", size = 3, shape = 18) +
  labs(title = "EEG waves with unconciousness event marked", x = "Time (s)", y = "EEG Signal") +
  scale_color_manual(name = "EEG", values = c("EEG1" = "blue", "EEG2" = "red")) +
  theme_minimal()

################## BOXPLOT ########################

# Reorganitzar les dades en format llarg (long format)
data_long <- melt(cleaned_datag, id.vars = "Time_s", measure.vars = c("EEG1_WAV", "EEG2_WAV"))

# Crear el boxplot
ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplot de EEG1 i EEG2", x = "EEG", y = "EEG Signal") +
  scale_fill_manual(values = c("EEG1_WAV" = "blue", "EEG2_WAV" = "red")) +
  theme_minimal()

################ DENSITAT #########################

# Crear el gràfic de densitat per EEG1 i EEG2 
ggplot(data_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densitat de EEG1 i EEG2", x = "EEG Signal", y = "Densitat") +
  scale_fill_manual(values = c("EEG1_WAV" = "blue", "EEG2_WAV" = "red")) +
  theme_minimal()   

