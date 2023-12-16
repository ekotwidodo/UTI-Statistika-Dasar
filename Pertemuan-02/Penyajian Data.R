# Install dan gunakan library ggplot dan openxlsx

install.packages("ggplot2")
install.packages("openxlsx")
library(ggplot2)
library(openxlsx)

# Data mahasiswa
mahasiswa <- read.xlsx("mahasiswa.xlsx", sheet = "Sheet 1")
mahasiswa

# --- VISUALISASI DATA ---

# Membuat canvas
gambar <- ggplot(mahasiswa, aes(x=Fakultas, y=JUMLAH, fill=Fakultas))

# Menambahkan objek bar chart dan simpan kembali ke dalam gambar
gambar <- gambar + geom_bar(width = 1, stat = "identity")

# Menampilkan grafik
gambar


# Menghitung Jumlah Data by Fakultas
summaryByFakultas <- aggregate(x=mahasiswa$JUMLAH, by=list(Kategori=mahasiswa$Fakultas, Tahun=mahasiswa$ANGKATAN), FUN=sum)
summaryByFakultas <- setNames(summaryByFakultas, c("Fakultas", "Tahun", "JumlahMahasiswa"))
summaryByFakultas

summaryByFakultas$Tahun = as.factor(summaryByFakultas$Tahun)

# Menampilkan Grafik
gambar2 <- ggplot(summaryByFakultas, aes(x=Fakultas, y=JumlahMahasiswa)) +
  geom_bar(stat = "identity", aes(fill = Tahun), width=0.8, position = position_dodge(width=0.8)) +
  theme_classic()

gambar2

# Menampilkan data dengan PieChart
piechart <- ggplot(summaryByFakultas, aes(x="", y=JumlahMahasiswa, fill=Fakultas))+ geom_bar(width = 1, stat = "identity")
piechart <- piechart + coord_polar("y", start=0)
piechart <- piechart + ggtitle("Disribusi Mahasiswa per Fakultas")
piechart <- piechart + scale_fill_brewer(palette="Blues")+ theme_minimal()
piechart <- piechart + guides(fill=guide_legend(title="Fakultas"))
piechart <- piechart + ylab("Jumlah Mahasiswa")
piechart

# Filter Data
summaryByFakultas[summaryByFakultas$Fakultas %in% c("ICT", "Ilmu Komunikasi"),]
gambar3 <- ggplot(summaryByFakultas[summaryByFakultas$Fakultas %in% c("ICT", "Ilmu Komunikasi"),], aes(x=Fakultas, y=JumlahMahasiswa)) +
  geom_bar(stat = "identity", aes(fill = Tahun), width=0.8, position = position_dodge(width=0.8)) +
  theme_classic()
gambar3
