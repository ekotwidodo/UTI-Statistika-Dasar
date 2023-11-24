# Data tinggi badan mahasiswa Universitas Y
tinggi_badan <- c(175, 178, 190, 180, 160, 155, 157, 167, 164, 153, 
                  156, 167, 170, 169, 170, 167, 170, 181, 183, 167,
                  167, 178, 177, 166, 155, 167, 187, 159, 161, 160,
                  189, 177, 171, 170, 167, 165, 168, 170, 178, 181,
                  162, 163, 163, 171, 173, 163, 164, 161, 160, 172,
                  172, 173, 171, 170, 161, 162, 162, 164, 181, 182,
                  172, 173, 172, 170, 158, 159, 173, 171, 151, 158,
                  177, 181, 151, 159, 178, 180, 182, 174, 182, 180)

# Mengetahui banyaknya data
banyak_data <- length(tinggi_badan)

# Visualisasi Tinggi Badan

hist(tinggi_badan, main = "Histogram Tinggi Badan")
plot(tinggi_badan, main = "Plot Tinggi Badan")
boxplot(tinggi_badan, main = "Boxplot Tinggi Badan")

# Ukuran Pemusatan (mean, median, modus)
mean_tinggi_badan <- mean(tinggi_badan)
mean_tinggi_badan

median_tinggi_badan <- median(tinggi_badan)
median_tinggi_badan

hitung_modus <- function(x) {
  nilai_unik <- unique(x)
  nilai_unik[which.max(tabulate(match(x, nilai_unik)))]
}

modus_tinggi_badan <- hitung_modus(tinggi_badan)
modus_tinggi_badan

# Ukuran Lokasi

Q1_tinggi_badan <- quantile(tinggi_badan, probs = seq(1/4, 0))
Q1_tinggi_badan

Q3_tinggi_badan <- quantile(tinggi_badan, probs = seq(3/4, 0))
Q3_tinggi_badan

D3_tinggi_badan <- quantile(tinggi_badan, probs = seq(3/10, 0))
D3_tinggi_badan

D8_tinggi_badan <- quantile(tinggi_badan, probs = seq(8/10, 0))
D8_tinggi_badan

P21_tinggi_badan <- quantile(tinggi_badan, probs = seq(21/100, 0))
P21_tinggi_badan

P45_tinggi_badan <- quantile(tinggi_badan, probs = seq(45/100, 0))
P45_tinggi_badan

P93_tinggi_badan <- quantile(tinggi_badan, probs = seq(93/100, 0))
P93_tinggi_badan

# Ukuran Penyebaran (range, ragam/varians, standar deviasi, koefisien variasi)

range_tingi_badan <- max(tinggi_badan) - min(tinggi_badan)
range_tingi_badan

varians_tinggi_badan <- var(tinggi_badan)
varians_tinggi_badan

stdev_tinggi_badan <- sd(tinggi_badan)
stdev_tinggi_badan

koef_variasi_tinggi_badan <- stdev_tinggi_badan / mean_tinggi_badan * 100
koef_variasi_tinggi_badan

# Ukuran Kemencengan dan Keruncingan

install.packages("moments")
library(moments)

skewness_tinggi_badan <- skewness(tinggi_badan)
skewness_tinggi_badan

# Jika nilai skewness_tinggi_badan < 0 atau negatif -> kurva cenderung condong ke kiri (kurva negatif)
# Jika nilai skewness_tinggi_badan > 0 atau positif -> kurva cenderung condong ke kanan (kurva positif)
# Jika nilai skewness_tinggi_badan mendekati nol atau 0 -> kurva cenderung simetris

kurtosis_tinggi_badan <- kurtosis(tinggi_badan)
kurtosis_tinggi_badan

# Jika kurtosis_tinggi_badan < 3, platikurtis -> kurva cenderung datar dan puncak tidak terlalu tinggi
# Jika kurtosis_tinggi_badan = 3, mesokurtis -> kurva normal (tidak terlalu tajam dan datar)
# Jika kurtosis_tinggi_badan > 3, leptokurtis -> kurva cenderung terlihat lancip dan tinggi

# Cara lain menghitung skewness dan kurtosis dengan package e1071

install.packages("e1071")
library(e1071)
skewness_tinggi_badan2 <- skewness(tinggi_badan, type = 2) # type=2 menggunakan moment based formula
kurtosis_tinggi_badan2 <- kurtosis(tinggi_badan, type = 2)

# Visualisasi Skewness dan Kurtosis
hist(tinggi_badan, prob = TRUE)
lines(density(tinggi_badan), col = "red")

# Packages untuk memudahkan penghitungan ukuran statistik

install.packages("psych")
library(psych)

describe(tinggi_badan)

install.packages("pastecs")
library(pastecs)

stat.desc(tinggi_badan)
