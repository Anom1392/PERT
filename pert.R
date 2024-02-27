# versi 3 =======================================
# PERT -------------------------------

library(simmer)
library(EnvStats)

pert <- simmer("PERT")
durasi_A <- function() rtri(1, 3, 6, 4)
durasi_B <- function() rtri(1, 7, 10, 8)
durasi_C <- function() rtri(1, 6, 10, 7)
durasi_D <- function() rtri(1, 3, 5, 4)
durasi_E <- function() rtri(1, 2, 6, 3)
traj_1 <- trajectory() %>%
  timeout(durasi_A) %>%
  timeout(durasi_B) %>%
  set_attribute("waktu_AB", function() round(now(pert),1)) 

traj_2 <- trajectory() %>%
  timeout(durasi_C) %>%
  timeout(durasi_D) %>%
  set_attribute("waktu_CD", function() round(now(pert),1))

traj_3 <- trajectory() %>%
  timeout(durasi_E)

traj_pert <- trajectory() %>%
  clone(
    n = 2,
    traj_1,
    traj_2) %>%
  synchronize(wait = TRUE) %>% 
  set_attribute("waktu_tahap1", function() round(now(pert),1)) %>%
  join(traj_3) %>%
  set_attribute("waktu_akhir", function() round(now(pert),1))

pert %>%
  add_generator("dummy", traj_pert, at(0), mon=2) %>%
  run() %>% invisible

# Replikasi ---------------------------------------------
require(parallel)
pert <- mclapply(1:250, function(i) {
  simmer("PERT") %>%
    add_generator("dummy", traj_pert, at(0), mon=2) %>%
    run() %>% invisible %>%
    wrap()
})

# versi 4 ===============================================
# PERT2 Revisi Replikasi ==================================

require(simmer)
require(EnvStats)

pert2 <- simmer("PERT2")

  durasi_1 <- function() rtri(1, 1, 5, 3)
  durasi_2 <- function() rtri(1, 3, 9, 6)
  durasi_3 <- function() rtri(1, 10, 19, 13)
  durasi_4 <- function() rtri(1, 3, 12, 9)
  durasi_5 <- function() rtri(1, 1, 8, 3)
  durasi_6 <- function() rtri(1, 8, 16, 9)
  durasi_7 <- function() rtri(1, 4, 13, 7)
  durasi_8 <- function() rtri(1, 3, 9, 6)
  durasi_9 <- function() rtri(1, 1, 8, 3)

traj_1 <- trajectory() %>%  
  set_global("waktu_1",durasi_1) #%>% 
# log_("traj_1") 

traj_2 <- trajectory() %>%
  set_global("waktu_2", durasi_2) #%>% 
# log_("traj_2") 

traj_3 <- trajectory() %>%
  set_global("waktu_3", durasi_3) #%>% 
# log_("traj_3") 

traj_4 <- trajectory() %>%  
  set_global("waktu_4",durasi_4) #%>% 
# log_("traj_4") 

traj_5 <- trajectory() %>%
  set_global("waktu_5", durasi_5) #%>% 
# log_("traj_5") 

traj_6 <- trajectory() %>%
  set_global("waktu_6", durasi_6) #%>% 
# log_("traj_6") 

traj_7 <- trajectory() %>%  
  set_global("waktu_7",durasi_7) #%>% 
# log_("traj_7") 

traj_8 <- trajectory() %>%
  set_global("waktu_8", durasi_8) #%>% 
# log_("traj_5") 

traj_9 <- trajectory() %>%
  set_global("waktu_9", durasi_9) #%>% 
# log_("traj_9")

traj_148 <- trajectory() %>%
  timeout_from_global("waktu_1") %>%
  # log_("masuk aktivitas 1") %>%
  timeout_from_global("waktu_4") %>%
  # log_("masuk aktivitas 4") %>%
  timeout_from_global("waktu_8") %>%
  # log_("masuk aktivitas 8") %>%
  set_attribute("waktu_148", function() now(pert2))

traj_156 <- trajectory() %>%
  timeout_from_global("waktu_1") %>%
  # log_("masuk aktivitas 1") %>%
  timeout_from_global("waktu_5") %>%
  # log_("masuk aktivitas 5") %>%
  timeout_from_global("waktu_6") %>%
  # log_("masuk aktivitas 6") %>%
  set_attribute("waktu_156", function() now(pert2))

traj_1579 <- trajectory() %>%
  timeout_from_global("waktu_1") %>%
  # log_("masuk aktivitas 1") %>%
  timeout_from_global("waktu_5") %>%
  # log_("masuk aktivitas 5") %>%
  timeout_from_global("waktu_7") %>%
  # log_("masuk aktivitas 7") %>%
  timeout_from_global("waktu_9") %>%
  # log_("masuk aktivitas 9") %>%
  set_attribute("waktu_1579", function() now(pert2))

traj_26 <- trajectory() %>%
  timeout_from_global("waktu_2") %>%
  # log_("masuk aktivitas 2") %>%
  timeout_from_global("waktu_6") %>%
  # log_("masuk aktivitas 6") %>%
  set_attribute("waktu_26", function() now(pert2))

traj_279 <- trajectory() %>%
  timeout_from_global("waktu_2") %>%
  # log_("masuk aktivitas 2") %>%
  timeout_from_global("waktu_7") %>%
  # log_("masuk aktivitas 7") %>%
  timeout_from_global("waktu_9") %>%
  # log_("masuk aktivitas 9") %>%
  set_attribute("waktu_279", function() now(pert2))

traj_39 <- trajectory() %>%
  timeout_from_global("waktu_3") %>%
  # log_("masuk aktivitas 3") %>%
  timeout_from_global("waktu_9") %>%
  # log_("masuk aktivitas 9") %>%
  set_attribute("waktu_39", function() now(pert2))

traj_pert2 <- trajectory() %>%
  clone(
    n = 6,
    traj_148,
    traj_156,
    traj_1579,
    traj_26,
    traj_279,
    traj_39
  ) %>%
  synchronize(wait = TRUE) %>% 
  set_attribute("waktu_pelaksanaan", function() now(pert2))

traj_awal <- trajectory() %>%
  clone(
    n = 9,
    traj_1,
    traj_2,
    traj_3,
    traj_4,
    traj_5,
    traj_6,
    traj_7,
    traj_8,
    traj_9
  )

# Hasil satu kali run simulasi ===========================
pert2 %>%
  add_generator("dummy1", traj_awal, at(0), mon=2) %>%
  add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
  run() %>% invisible %>%      
  get_mon_attributes()

# Hasil multi run simulasi =================================

require(parallel)
pert2 <- mclapply(1:100, function(i) {
  simmer("PERT2") %>%
    add_generator("dummy1", traj_awal, at(0), mon=2) %>%
    add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
    run() %>% invisible %>%
    wrap()
})
(hasil <- get_mon_attributes(pert2))
hasil[,"value"] <- ifelse(hasil$name == "dummy20", hasil$time, hasil$value)



# Replicate 2 ======================
pert2 <- lapply(1:100, function(i) {
  simmer("PERT2") %>%
    add_generator("dummy1", traj_awal, at(0), mon=2) %>%
    add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
    run() %>% invisible
})
get_mon_attributes(pert2)

################################################################
# user define function untuk mencari jalur kritis,             #
# waktu penyelesaian proyek                                    #
# nama fungsi prediksi_WP2                                            #
################################################################

prediksi_WP2 <- function(){
  require(simmer)
  require(EnvStats)
  require(parallel)
  
  pert2 <- simmer("PERT2")
  
  durasi_1 <- function() rtri(1, 1, 5, 3)
  durasi_2 <- function() rtri(1, 3, 9, 6)
  durasi_3 <- function() rtri(1, 10, 19, 13)
  durasi_4 <- function() rtri(1, 3, 12, 9)
  durasi_5 <- function() rtri(1, 1, 8, 3)
  durasi_6 <- function() rtri(1, 8, 16, 9)
  durasi_7 <- function() rtri(1, 4, 13, 7)
  durasi_8 <- function() rtri(1, 3, 9, 6)
  durasi_9 <- function() rtri(1, 1, 8, 3)
  
  traj_1 <- trajectory() %>%  
    set_global("waktu_1",durasi_1) 
  
  traj_2 <- trajectory() %>%
    set_global("waktu_2", durasi_2) 
  
  traj_3 <- trajectory() %>%
    set_global("waktu_3", durasi_3) #%>% 
  # log_("traj_3") 
  
  traj_4 <- trajectory() %>%  
    set_global("waktu_4",durasi_4) 
  
  traj_5 <- trajectory() %>%
    set_global("waktu_5", durasi_5) #%>% 
  # log_("traj_5") 
  
  traj_6 <- trajectory() %>%
    set_global("waktu_6", durasi_6) 
  
  traj_7 <- trajectory() %>%  
    set_global("waktu_7",durasi_7) 
  
  traj_8 <- trajectory() %>%
    set_global("waktu_8", durasi_8) #%>% 
  # log_("traj_5") 
  
  traj_9 <- trajectory() %>%
    set_global("waktu_9", durasi_9) 
  
  traj_148 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_4") %>%
    timeout_from_global("waktu_8") %>%
    set_attribute("waktu_148", function() now(pert2))
  
  traj_156 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_5") %>%
    timeout_from_global("waktu_6") %>%
    set_attribute("waktu_156", function() now(pert2))
  
  traj_1579 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_5") %>%
    timeout_from_global("waktu_7") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_1579", function() now(pert2))
  
  traj_26 <- trajectory() %>%
    timeout_from_global("waktu_2") %>%
    timeout_from_global("waktu_6") %>%
    set_attribute("waktu_26", function() now(pert2))
  
  traj_279 <- trajectory() %>%
    timeout_from_global("waktu_2") %>%
    timeout_from_global("waktu_7") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_279", function() now(pert2))
  
  traj_39 <- trajectory() %>%
    timeout_from_global("waktu_3") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_39", function() now(pert2))
  
  traj_pert2 <- trajectory() %>%
    clone(
      n = 6,
      traj_148,
      traj_156,
      traj_1579,
      traj_26,
      traj_279,
      traj_39
    ) %>%
    synchronize(wait = TRUE) %>% 
    set_attribute("waktu_pelaksanaan", function() now(pert2))
  
  traj_awal <- trajectory() %>%
    clone(
      n = 9,
      traj_1,
      traj_2,
      traj_3,
      traj_4,
      traj_5,
      traj_6,
      traj_7,
      traj_8,
      traj_9
    )
  
  pert2 <- mclapply(1:100, function(i) {
    simmer("PERT2") %>%
      add_generator("dummy1", traj_awal, at(0), mon=2) %>%
      add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
      run() %>% invisible %>%
      wrap()
  })
  has <- get_mon_attributes(pert2)
  hasil <- subset(has, time != 0 & key != "waktu_pelaksanaan", 
                  select = c(key, time, replication))
  return(hasil)
}

dt2 <- prediksi_WP2()
dt2

maxTime <- tapply(dt2$time, factor(dt2$replication), max)
y <- data.frame(key="", time=0, replication=0)
for (j in as.numeric(names(maxTime))){
  x = subset(dt2, replication==j, select=c(key, time, replication))
  y = rbind(y, subset(x, time == maxTime[j], select=c(key, time, replication)))
}
ydat <- y[-1,]
table(factor(ydat$key))
subset(dt2, key=="waktu_pelaksanaan", select = c(key, time, replication))

maxTime <- tapply(dt2$time, factor(dt2$replication), max)
dt2[dt2$time == as.vector(maxTime),]
dt2$time

subset(dt2, time == maxTime, select=c(key, time, replication))

# ----------------------------------------------------------------
# menggunakan hasil dari dt <- prediksi_WP()

maxTime2 <- tapply(dt$time, factor(dt$replication), max)

k <- as.numeric(names(maxTime2))
yt <- data.frame(key="", time=0, replication=0)
for (j in k) { #as.numeric(names(maxTime2))){
  xt = subset(dt, replication==j, select=c(key, time, replication))
  yt = rbind(yt, subset(xt, time == maxTime2[j], select=c(key, time, replication)))
}
ydat2 <- yT[-1,]
head(ydat2)

table(factor(ydat2$key))

# subset(dt, key=="waktu_pelaksanaan", select = c(key, time, replication))

maxTime2 <- tapply(dt$time, factor(dt$replication), max)
dt[dt$time == as.vector(maxTime2),]
dt$time

subset(dt, time == maxTime, select=c(key, time, replication))


################################################################
# user define function untuk mencari waktu penyelesaian proyek #
# nama fungsi prediksi_WP                                            #
################################################################

prediksi_WP <- function(){
  require(simmer)
  require(EnvStats)
  require(parallel)
  
  pert2 <- simmer("PERT2")
  
  durasi_1 <- function() rtri(1, 1, 5, 3)
  durasi_2 <- function() rtri(1, 3, 9, 6)
  durasi_3 <- function() rtri(1, 10, 19, 13)
  durasi_4 <- function() rtri(1, 3, 12, 9)
  durasi_5 <- function() rtri(1, 1, 8, 3)
  durasi_6 <- function() rtri(1, 8, 16, 9)
  durasi_7 <- function() rtri(1, 4, 13, 7)
  durasi_8 <- function() rtri(1, 3, 9, 6)
  durasi_9 <- function() rtri(1, 1, 8, 3)
  
  traj_1 <- trajectory() %>%  
    set_global("waktu_1",durasi_1) 
  
  traj_2 <- trajectory() %>%
    set_global("waktu_2", durasi_2) 
  
  traj_3 <- trajectory() %>%
    set_global("waktu_3", durasi_3) 
  
  traj_4 <- trajectory() %>%  
    set_global("waktu_4",durasi_4) 
  
  traj_5 <- trajectory() %>%
    set_global("waktu_5", durasi_5) 
  
  traj_6 <- trajectory() %>%
    set_global("waktu_6", durasi_6) 
  
  traj_7 <- trajectory() %>%  
    set_global("waktu_7",durasi_7) 
  
  traj_8 <- trajectory() %>%
    set_global("waktu_8", durasi_8) 
  
  traj_9 <- trajectory() %>%
    set_global("waktu_9", durasi_9) 
  
  traj_148 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_4") %>%
    timeout_from_global("waktu_8") %>%
    set_attribute("waktu_148", function() now(pert2))
  
  traj_156 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_5") %>%
    timeout_from_global("waktu_6") %>%
    set_attribute("waktu_156", function() now(pert2))
  
  traj_1579 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_5") %>%
    timeout_from_global("waktu_7") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_1579", function() now(pert2))
  
  traj_26 <- trajectory() %>%
    timeout_from_global("waktu_2") %>%
    timeout_from_global("waktu_6") %>%
    set_attribute("waktu_26", function() now(pert2))
  
  traj_279 <- trajectory() %>%
    timeout_from_global("waktu_2") %>%
    timeout_from_global("waktu_7") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_279", function() now(pert2))
  
  traj_39 <- trajectory() %>%
    timeout_from_global("waktu_3") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_39", function() now(pert2))
  
  traj_pert2 <- trajectory() %>%
    clone(
      n = 6,
      traj_148,
      traj_156,
      traj_1579,
      traj_26,
      traj_279,
      traj_39
    ) %>%
    synchronize(wait = TRUE) %>% 
    set_attribute("waktu_pelaksanaan", function() now(pert2))
  
  traj_awal <- trajectory() %>%
    clone(
      n = 9,
      traj_1,
      traj_2,
      traj_3,
      traj_4,
      traj_5,
      traj_6,
      traj_7,
      traj_8,
      traj_9
    )
  
  pert2 <- mclapply(1:100, function(i) {
    simmer("PERT2") %>%
      add_generator("dummy1", traj_awal, at(0), mon=2) %>%
      add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
      run() %>% invisible %>%
      wrap()
  })
  has <- get_mon_attributes(pert2)
  hasil <- subset(has, key == "waktu_pelaksanaan", 
                  select = c(key, time, replication))
  return(hasil)
}
dt <- prediksi_WP()
head(dt)

# win.graph(width=4.875,height=3,pointsize=8)

win.graph(width=4.6,height=5,pointsize=10)
hist(dt$time, main="Predicted Time of Project \n Completion", 
     xlab="Completion Time")

shapiro.test(dt$time) # Uji kenormalan

win.graph(width=4.875,height=3,pointsize=8)
qqnorm(dt$time); qqline(dt$time, col="red4") # plot kuantil-kuantil

(SDev <- sd(dt$time)) # Simpangan Baku
(rata2 <- mean(dt$time)) # rata-rata waktu penyelesaian

# SK95 <- mean(dt$time) + c(-1, 1)*2*SDev/sqrt(length(dt$time))) # Selang Kepercayaan 95% untuk WP
SK95 <- rata2 + c(-1, 1)*2*SDev/sqrt(length(dt$time))
SK95


# tingkat risiko jika waktu penyelesaian proyek melebihi 22 minggu
Time <- seq(21, 27, 0.5)
risk <- pnorm(Time, mean=rata2, sd=SDev, lower.tail=FALSE)
names(risk) <- Time
round(risk,3)

(WS <- qnorm(0.01, mean=rata2, sd=SDev, lower.tail=FALSE))

##################################################################################
# user define function untuk mencari sebaran rata-rata waktu penyelesaian proyek #
# nama fungsi PERT.                                                              #
##################################################################################

PERT. <- function(){
  require(simmer)
  require(EnvStats)
  require(parallel)
  
pert2 <- simmer("PERT2")
  
  durasi_1 <- function() rtri(1, 1, 5, 3)
  durasi_2 <- function() rtri(1, 3, 9, 6)
  durasi_3 <- function() rtri(1, 10, 19, 13)
  durasi_4 <- function() rtri(1, 3, 12, 9)
  durasi_5 <- function() rtri(1, 1, 8, 3)
  durasi_6 <- function() rtri(1, 8, 16, 9)
  durasi_7 <- function() rtri(1, 4, 13, 7)
  durasi_8 <- function() rtri(1, 3, 9, 6)
  durasi_9 <- function() rtri(1, 1, 8, 3)
  
  traj_1 <- trajectory() %>%  
    set_global("waktu_1",durasi_1) 
  
  traj_2 <- trajectory() %>%
    set_global("waktu_2", durasi_2) 
  
  traj_3 <- trajectory() %>%
    set_global("waktu_3", durasi_3) #%>% 
  # log_("traj_3") 
  
  traj_4 <- trajectory() %>%  
    set_global("waktu_4",durasi_4) 
  
  traj_5 <- trajectory() %>%
    set_global("waktu_5", durasi_5) #%>% 
  # log_("traj_5") 
  
  traj_6 <- trajectory() %>%
    set_global("waktu_6", durasi_6) 
  
  traj_7 <- trajectory() %>%  
    set_global("waktu_7",durasi_7) 
  
  traj_8 <- trajectory() %>%
    set_global("waktu_8", durasi_8) #%>% 
  # log_("traj_5") 
  
  traj_9 <- trajectory() %>%
    set_global("waktu_9", durasi_9) 
  
  traj_148 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_4") %>%
    timeout_from_global("waktu_8") %>%
    set_attribute("waktu_148", function() now(pert2))
  
  traj_156 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_5") %>%
    timeout_from_global("waktu_6") %>%
    set_attribute("waktu_156", function() now(pert2))
  
  traj_1579 <- trajectory() %>%
    timeout_from_global("waktu_1") %>%
    timeout_from_global("waktu_5") %>%
    timeout_from_global("waktu_7") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_1579", function() now(pert2))
  
  traj_26 <- trajectory() %>%
    timeout_from_global("waktu_2") %>%
    timeout_from_global("waktu_6") %>%
    set_attribute("waktu_26", function() now(pert2))
  
  traj_279 <- trajectory() %>%
    timeout_from_global("waktu_2") %>%
    timeout_from_global("waktu_7") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_279", function() now(pert2))
  
  traj_39 <- trajectory() %>%
    timeout_from_global("waktu_3") %>%
    timeout_from_global("waktu_9") %>%
    set_attribute("waktu_39", function() now(pert2))
  
  traj_pert2 <- trajectory() %>%
    clone(
      n = 6,
      traj_148,
      traj_156,
      traj_1579,
      traj_26,
      traj_279,
      traj_39
    ) %>%
    synchronize(wait = TRUE) %>% 
    set_attribute("waktu_pelaksanaan", function() now(pert2))
  
  traj_awal <- trajectory() %>%
    clone(
      n = 9,
      traj_1,
      traj_2,
      traj_3,
      traj_4,
      traj_5,
      traj_6,
      traj_7,
      traj_8,
      traj_9
    )
  
  pert2 <- mclapply(1:100, function(i) {
    simmer("PERT2") %>%
      add_generator("dummy1", traj_awal, at(0), mon=2) %>%
      add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
      run() %>% invisible %>%
      wrap()
  })
  has <- get_mon_attributes(pert2)
  hasil <- subset(has, key == "waktu_pelaksanaan", 
                    select = c(key, time, replication))
 rata <- mean(hasil$time)
 return(rata)
}

PERT.() # fungsi untuk mendapatkan rata-rata waktu penyelesaian proyek

rata2 <- replicate(n=100, PERT.()) # mendapatkan 100 rata-rata
hist(rata2) #histogram
SE <- sd(rata2) # Galat baku
SK95 <- mean(rata2) + c(-1, 1)*2*SE  # Selang Kepercayaan 95% untuk rata-rata

shapiro.test(rata2) # Uji kenormalan
qqnorm(rata2); qqline(rata2, col="red4") # plot kuantil-kuantil

# Dari paper ------------
maxTime2
maxTime2 <- tapply(dt$time,
                    factor(dt$replication), max)
k <- as.numeric (names(maxTime2))
k
yt <- data.frame (key="", time=0,
                  replication=0)
for (j in k) {
  xt = subset( dt2, replication ==j,
               select= c( key, time, replication))
  yt = rbind( yt, subset(xt,
                         time == maxTime2[j],
                         select= c( key, time, replication)))
}
yt



