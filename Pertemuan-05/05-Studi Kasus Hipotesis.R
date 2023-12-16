# Pengujian Hipotesis Satu Populasi

# --- RATA-RATA ---

x1.mu = 400
x1.sigma = 35
x1.bar = 398
x1.conf = 1 - 0.01
x1.n = 40

install.packages("BSDA")
library(BSDA)

# Uji hipotesis
zsum.test(mean.x = x1.bar, sigma.x = x1.sigma, n.x = x1.n, 
          alternative = "greater", mu = x1.mu, conf.level = x1.conf)

# Karena p-value (0.6411) > alfa (0.01 / 1%), maka GAGAL TOLAK H0

# --- PROPORSI ---

x2.x = 25
x2.p = 0.08
x2.conf = 1 - 0.05
x2.n = 500

# Uji hipotesis
prop.test(x = x2.x, n = x2.n, p = x2.p, 
          alternative = "two.sided", correct = FALSE)

# Karena p-value (0.01341) < alfa (0.05 / 5%), maka TOLAK H0

# Pengujian Hipotesis Satu Populasi

# --- BEDA RATA-RATA ---

chsq.
