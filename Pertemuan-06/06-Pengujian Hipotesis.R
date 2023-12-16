# --- PENGUJIAN HIPOTESIS 2 POPULASI ---

# Pengujian Hipotesis Beda Rata-rata 2 Populasi -> INDEPENDEN

tinggi_siswa <- c(120, 122, 120, 138, 130, 128, 132, 125, 127, 130)
tinggi_siswi <- c(115, 120, 118, 130, 135, 126, 127, 126, 125, 129)
# Asumsinya adalah data diambil dari populasi normal
# Apakah bisa dikatakan TINGGI SISWA dan SISWI Kelas 1 tersebut sama?
# H0 -> rata2 tinggi siswa dan siswi kelas 1 sama
# H1 -> rata2 tinggi siswa dan siswi kelas 1 tidak sama
# Tingkat signifikansi, alfa 5% -> 0.05

t.test(tinggi_siswa, tinggi_siswi, alternative = c("two.sided"), paired = F,
       var.equal = T, conf.level = 0.95)

# p-value = 0.4313 > alfa 5%, sehingga GAGAL TOLAK H0 -> rata2 tinggi siswa dan siswi kelas 1 sama

# Pengujian Hipotesis Beda Rata-rata 2 Populasi -> DEPENDEN

ipk_metode_sebelum <- c(3.7, 3.6, 3.8, 3.7, 3.9, 3.8, 3.6, 3.9)
ipk_metode_sesudah <- c(3.6, 3.7, 3.7, 3.6, 3.6, 3.4, 3.5, 3.5)
# H0 -> rata2 IPK mahasiswa sebelum menerima metode pembelajaran baru lebih besar atau sama dengan rata2 IPK mahasiswa setelah menerima metode pembelajaran baru
# H1 -> rata2 IPK mahasiswa sebelum menerima metode pembelajaran baru lebih kecil dari rata2 IPK mahasiswa setelah menerima metode pembelajaran baru
# Tingkat signifikansi, alfa 5% -> 0.05

t.test(ipk_metode_sebelum, ipk_metode_sesudah, alternative = c("less"), paired = T, 
       var.equal = T, conf.level = 0.95)

# p-value = 0.9872 > alfa 5%, sehingga GAGAL TOLAK H0

# Pengujian Hipotesis Beda Proporsi 2 Populasi

# p1 -> proporsi mahasiswa Universitas Teknokrat yang menyukai sosok calon A sebagai Gubernur BEM yang baru
# p2 -> proporsi mahasiswi Universitas Teknokrat yang menyukai sosok calon A sebagai Gubernur BEM yang baru
# H0 -> p1 = p2
# H1 -> p1 > p2 (calon A lebih disukai oleh mahasiswa daripada mahasiswi)
# mahasiswa -> 345 dari 600, mahasiswi -> 132 dari 530
# alfa -> 5%

prop.test(x=c(345, 132), n=c(600, 530), alternative = "two.sided", correct = FALSE)

# p-value kecil sekali (2.2e-16) < 5%, maka H0 ditolak -> calon A lebih disukai oleh mahasiswa daripada mahasiswi
