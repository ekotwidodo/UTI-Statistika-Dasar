# ANALISIS KORELASI

# 1. Membaca raw data excel
install.packages("readxl") # install packages readxl

library(readxl) # load libary readxl

data_ipm_hls <- read_excel(file.choose()) # memunculkan popup untuk memilih file excel
data_ipm_hls # menampilkan data

View(data_ipm_hls)

# 2. Melakukan pengecekan normalitas (z = x_bar / simp_baku) -> UJI NORMALITAS

install.packages("nortest") # install packages nortest

library(nortest) # load library nortest

# Shapiro-Wilk normality test
shapiro.test(data_ipm_hls$IPM)

# p-value = 0.06165, > 5% - > 0.05 --> variable IPM berdistribusi normal

shapiro.test(data_ipm_hls$HLS)

# p-value = 0.06981, > 5% --> variabel HLS berdistribusi normal

# 3. Melakukan pengujian KORELASI - PEARSON

cor.test(data_ipm_hls$IPM, data_ipm_hls$HLS, method = 'pearson')

# nilai korelasi = 0.893947 -> korelasi kuat, positif -> searah = HLS naik, IPM naik

# 4. Visualisasikan
plot(data_ipm_hls$IPM, data_ipm_hls$HLS, main = "IPM vs HLS")

# par membagi tampilan menjadi sekian kolom
par(mfrow = c(1,2))

hist(data_ipm_hls$IPM, main = "Histogram IPM")
hist(data_ipm_hls$HLS, main = "Histogram HLS")

# ---------------------------------------------------------- #

# REGRESI LINIER
# Menggunakan data curah hujan dan penjualan payung

# 1. Membaca data dari file excel
data_hujan_payung <- read_excel(file.choose())
data_hujan_payung

# Rename nama kolom menjadi curah_hujan dan penjualan_payung
colnames(data_hujan_payung)[colnames(data_hujan_payung) == "Curah Hujan (X)"] = "curah_hujan"
colnames(data_hujan_payung)[colnames(data_hujan_payung) == "Penjualan Payung (Y)"] = "penjualan_payung"
data_hujan_payung

# 2. Visualisasi data
plot(data_hujan_payung$curah_hujan, data_hujan_payung$penjualan_payung, 
     main = "Hubungan antara Curah Hujan dan Penjualan Payung", 
     xlab = "Curah Hujan (mm)", ylab = "Penjualan Payung")

# 3. Uji Normalitas
shapiro.test(data_hujan_payung$curah_hujan)
shapiro.test(data_hujan_payung$penjualan_payung)
# --> kedua variabel p-value > 5% -> distribusi normal

# 4. Uji Korelasi
cor.test(data_hujan_payung$curah_hujan, data_hujan_payung$penjualan_payung, method = 'pearson')
# --> korelasinya 99% (sangat kuat) dan searah -> semakin tinggi curah hujan, penjualan payung semakin tinggi

# 5. Regresi Linier Sederhana
model_regresi <- lm(data_hujan_payung$penjualan_payung ~ data_hujan_payung$curah_hujan)
summary(model_regresi)

# - Call: informasi tentang fungsi yang dipakai dalam analisis regresi linier sederhana
# - Residuals: selisih antara nilai observasi aktual dan nilai prediksi yang dihasilkan oleh model regresi
# - Coefficients: estimasi koefisien regresi. intercept (a) bernilai 5.1992 artinya, setiap peningkatan satu unit curah_hujan akan meningkat penjualan_payung sebesar 5.1992
# - Signif. codes: signifikansi statistik dari koefisien regresi (tanda asterisk * sebanyak 3, 0.001, level signifikansi 0.001 -> sangat signifikan)
# - Residual standard error: mengukur seberapa jauh prediksi model regresi dari nilai observasi aktual. Semakin kecil nilai ini, semakin dekat prediksi model dengan nilai observasi aktual
# - Multiple R-squared: seberapa besar variasi dari variabel penjualan_payung yang dapat dijelaskan oleh variabel curah_hujan.
# - Adjusted R-squared: seberapa besar variasi dari variabel penjualan_payung yang dapat dijelaskan oleh variabel curah_hujan setelah mempertimbangkan jumlah parameter dalam model. Semakin dekat nilai adjusted R-squared dengan nilai R-squared, semakin baik model tersebut.
# - F-statistic: mengukur signifikansi secara keseluruhan dari model regresi.
# - p-value kurang dari 2.2e-16 (kurang dari 5%) artinya model regresi tersebut signifikan secara statistik
# Model regresi -> y = 2.6033 + 5.1992x
