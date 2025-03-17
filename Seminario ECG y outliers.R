
##########################################
######## SEMINARIO ECG y outliers ########
##########################################

##### Álvaro Fandos Garea, 1638110

### Hoy trabajaremos con el ECG_3 de la página: https://www.physionet.org/content/ephnogram/1.0.0/

### 1- Investiga qué función de R puedes usar para importar el ECG:

# En los archivos de physionet hay dos tipos de archivos:
# .dat contiene las señales del ECG en binario.
# .hea es el encabezado con metadatos.

# Descargamos los archivos desde PhysioNet y los guardamos en el directorio de trabajo.
hea_file <- "ECGPCG0003.hea"
dat_file <- "ECGPCG0003.dat"

# Leemos los metadatos del ECG (archivo .hea).
hea_data <- readLines(hea_file)
sampling_rate <- as.numeric(strsplit(hea_data[1], " ")[[1]][3])  # Extraer la frecuencia de muestreo

# Leemos la señal del ECG (archivo .dat).
signal <- readBin(dat_file, what = "integer", size = 2, signed = TRUE, endian = "little", n = file.info(dat_file)$size / 2)

# Creamos una serie temporal en función de la frecuencia de muestreo
time <- seq(0, length(signal) - 1) / sampling_rate

# Graficamos el ECG
plot(time, signal, type = "l", main = "Señal ECG", xlab = "Tiempo (s)", ylab = "Amplitud")

### 2- Busca alguna librería que calcule los picos R:

library(pracma)

# Primero detectamos los picos 
picos <- findpeaks(signal, nups = 1, ndowns = 1, minpeakheight = max(signal) * 0.6, minpeakdistance = sampling_rate / 2)

# Después graficamos estos picos en el ECG
plot(time, signal, type = "l", main = "ECG con Picos R", xlab = "Tiempo (s)", ylab = "Amplitud")
points(time[picos[, 2]], picos[, 1], col = "red", pch = 19)  # Marcar picos R en rojo

### 3- Calcula los intervalos RR:

# Obtenemos los tiempos de los picos R
RR_intervals <- diff(time[picos[, 2]])

# Después graficamos la variabilidad de los intervalos RR
plot(RR_intervals, type = "o", main = "Intervalos RR", xlab = "Latido", ylab = "Tiempo (s)")

### 4- Usa algún método estadístico para detectar valores anómalos en la serie:

# Calculamos los cuartiles y el rango intercuartil
Q1 <- quantile(RR_intervals, 0.25)
Q3 <- quantile(RR_intervals, 0.75)
IQR <- Q3 - Q1

# Definimos las anomalías
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identificamos anomalías
anomalous_RR <- which(RR_intervals < lower_bound | RR_intervals > upper_bound)
