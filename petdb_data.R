setwd("~/") # рабочая директория где лежит исходный csv файл

#======= подключение библиотек ======
library(dplyr)
library(ggplot2)
library(latex2exp)

data <- read.csv("earthchem_download_14558.txt", sep = "\t", header = T) # читаем файл и записываем в массив data
ox.el <- data.frame(data[,c(18:30)]) # делаем таблицу с оксидами
tas <- ox.el[,c(1,2,11,12)] # оставляем название обстановки, SiO2, K2O и Na2O для построения TAS диаграммы


# ==== имеет смысл посмотреть данные и немного их пофильтровать ====
tas <- filter(tas, K2O < 10)
tas <- filter(tas, SIO2 > 30)
tas <- tas %>% filter(!is.na(tas$NA2O)) # удаление строк с отсутствующими значениями в ячейках
tas <- data.frame(tas) 


plot(tas$SIO2,tas$K2O) # проверка обработки таблицы

#---------------- здесь скрипт по созданию полей значений на  TAS диаграмме ----------------------
library(plyr)
points <- data.frame(
  rbind(c( 1,41,0),
        c( 2,41,3),
        c( 3,41,7),
        c( 4,45,9.4),
        c( 5,48.4,11.5),
        c( 6,52.5,14),
        c( 7,48.4,16),
        c( 8,57.6,11.7),
        c( 9,53,9.3),
        c( 10,49.4,7.3),
        c( 11,45,5),
        c( 12,45,3),
        c( 13,45,0),
        c( 14,52,0),
        c( 15,52,5),
        c( 16,57,5.9),
        c( 17,57,0),
        c( 18,63,0),
        c( 19,63,7),
        c( 20,69,8),
        c( 21,75,0),
        c( 22,61,13.5),
        c( 23,35,16),
        c( 24,35,0),
        c( 25,66,16),
        c( 26,69,16),
        c( 27,78,0),
        c( 28,78,16)
  )
)
colnames(points) = c ("IDPoint","X","Y")

base <- ggplot(data=points, aes(X,Y)) +
  theme_bw() + 
  geom_point(shape=21, size=10, color="blue", fill="white") +
  geom_text(aes(label=IDPoint), color="blue")
print(base)

polygon.labels <- data.frame(
  Label=c("picro-\nbasalt",
          "tephrite\nbasanite",
          "basalt",
          "trachy-\nbasalt",
          "basaltic\nandesite",
          "basaltic\ntrachy-\nandesite",
          "phono-\ntephrite",
          "tephri-\nphonolite",
          "trachy-\nandesite",
          "andesite",
          "foidite",
          "phonolite",
          "trachite",
          "dacite",
          "rhyolite")
)
polygon.labels$IDLabel=1:nrow(polygon.labels)

polygons <- data.frame(
  rbind(c(1,1),c(1,2),c(1,12),c(1,13),                 #picro-basalt
        c(2,2),c(2,3),c(2,4),c(2,10),c(2,11),c(2,12),  #tephrite basanite
        c(3,13),c(3,12),c(3,11),c(3,15),c(3,14),       #basalt
        c(4,11),c(4,10),c(4,15),                       #trachy-basalt
        c(5,15),c(5,16),c(5,17),c(5,14),               #basaltic andesite
        c(6,10),c(6,15),c(6,16),c(6,9),                #basaltic trachy andesite  
        c(7,4),c(7,5),c(7,9),c(7,10),                  #phono-tephrite
        c(8,5),c(8,6),c(8,8),c(8,9),                   #tephri-phonolite
        c(9,9),c(9,8),c(9,19),c(9,16),                 #trachy-andesite  
        c(10,16),c(10,19),c(10,18),c(10,17),          #andesite
        c(11,1),c(11,2),c(11,3),c(11,4),c(11,5),c(11,6),c(11,7),c(11,23),c(11,24), #foidite
        c(12,7),c(12,6),c(12,8),c(12,22),c(12,25),   #phonolite
        c(13,8),c(13,22),c(13,25),c(13,26),c(13,20),c(13,19), #trachite
        c(14,18),c(14,19),c(14,20),c(14,21), #dacite
        c(15,28),c(15,27),c(15,21),c(15,20),c(15,26) #rhyolite
  )
)
polygons$PointOrder <- 1:nrow(polygons)
colnames(polygons) = c("IDLabel","IDPoint","PointOrder")

df <- merge(polygons,points)
df <- merge(df,polygon.labels)
df <- df[order(df$PointOrder),]

Labs = ddply(df,"Label",function(x){c(c(mean(x$X),mean(x$Y)))})
colnames(Labs) = c("Label","X","Y")
Labs[6,3] <- 13
Labs[12,3] <- 7
Labs[2,3] <- 1.5
# -------------------------------- шаблон готов -------------------------

# строим с помощью библиотеки ggplot
tas_dia <- ggplot(tas, aes(x=SIO2, y=K2O+NA2O, color=TECTONIC.SETTING)) + geom_point() +
  geom_polygon(data=df,aes(X,Y, group=Label),color="black", alpha=0.01, show.legend=F) +
  geom_text(data=Labs,aes(X,Y,label=Label),size=2.7,color="black", alpha=0.8) +
  theme_bw()

tas_dia # вывод на экран диаграммы

ggsave("tas.pdf", plot=tas_dia, width = 25, height = 15, units = c("cm")) # запись диаграммы в pdf. файл
#========================

#======================== подготовка таблицы с редкими элементами ========

tr.el <- data[,c(18,31:70)]
tr.el <- data[,c("TECTONIC.SETTING","RB","BA","TH","U","TA","NB","LA","CE","PB","PR","SR","ND","ZR","HF","SM","EU","GD","TB","DY","Y","HO","ER","TM","YB","LU")]
tr.el <- na.omit(tr.el)


#------------------------- диаграмма Pearce -------------------------------------------------
# подготовим столбцы с нужными отношениями (вообще это не обязательно, но так мы проверим в порядке ли данные)
tr.el$nb_yb <- tr.el$NB/tr.el$YB
tr.el$th_yb <- tr.el$TH/tr.el$YB

# займемся шаблоном
# Рисуем линии 
pearce1 <- data.frame(
  rbind(c( 1,0.1,0.01),
        c( 2,70,10)))
colnames(pearce1) = c ("IDPoint","X","Y")

pearce2 <- data.frame(
  rbind(c( 1,0.3,0.01),
        c( 2,100, 4.5)))
colnames(pearce2) = c ("IDPoint","X","Y")

pearce3 <- data.frame(
  rbind(c( 1,0.1,1.25),
        c( 2,0.8, 10)))
colnames(pearce3) = c ("IDPoint","X","Y")


MORB <- data.frame(
  rbind(c(1,0.7,0.045),
        c(2,3.5,0.25),
        c(3,23,2)))

# собственно графики со всей бижутерией
n <- ggplot() +
  geom_point(data=tr.el, aes(nb_yb, y=th_yb, color=TECTONIC.SETTING), shape=21, size=3) +
  scale_x_continuous(trans = 'log10', limits=c(0.1, 100)) +
  scale_y_continuous(trans = 'log10', limits=c(0.01, 10)) +
  annotation_logticks()  +
  labs(title = NULL, fill = NULL) +
  ylab("Th/Yb")+
  xlab("Nb/Yb") +
  theme_bw() +
  theme(legend.position = c(.85, .2),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3))

pearce <- n + geom_point(data=MORB, aes(x=X2,y=X3), shape = 23, size = 4, col = "red", fill = "yellow" ) +
  geom_path(data=pearce1, aes(X,Y)) + geom_path(data=pearce2, aes(X,Y)) + geom_path(data=pearce3, aes(X,Y)) +
  annotate("text", x = 0.7, y = 0.03, label = "N-MORB", angle = 45, size = 3, fontface='bold') +
  annotate("text", x=3.5, y = 0.18, label = "E-MORB", angle= 45, size = 3, fontface='bold') +
  annotate("text", x=0.3, y = 3, label = "Volcanic arc array", angle= 45) +
  annotate("text", x=27, y = 1.7, label = "OIB", angle= 45, size = 3, fontface='bold') +
  geom_segment(aes(x = 1.5, y = 0.15, xend = 1.5, yend = 0.4),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 30, y = 3, xend = 30, yend = 10),
               arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=0.6, y = 0.3, label = "Magma-crust \ninteraction", angle= 0) +
  annotate("text", x=12, y = 7, label = "Deep-crustal \nrecycling", angle= 0) 


pearce

ggsave("pearce.pdf", plot=pearce, width = 18, height = 17, units = c("cm"))


#--------------Condie------------------
# при обработке данных выяснилось, что Zr записан не как числа. Поэтому тут придется немного доработать этот момент. Также готовим столбцы с готовыми отношениями.
tr.el <- filter(tr.el, ZR > 0)
zr <- as.numeric(tr.el$ZR)
tr.el$zr <- zr
tr.el$nb_th <- tr.el$NB/tr.el$TH
tr.el$zr_nb <- tr.el$zr/tr.el$NB

#-------- построение полей шаблона -----
library(plyr)

points <- data.frame(
  rbind(c(1,0.5,30),
        c(2,1,95),
        c(3,4.5,83),
        c(4,8.5,19),
        c(5,8.2,12),
        c(6,3,10.5),
        c(7,1,17),
        c(8,2,24),
        c(9,15,24),
        c(10,28.2,19),
        c(11,28.2,12),
        c(12,20,7),
        c(13,13,8),
        c(14,5.2,10.5),
        c(15,2.2,14),
        c(16,19.7,15),
        c(17,13.5,20.5),
        c(18,10.7,36),
        c(19,13.6,60),
        c(20,19,70),
        c(21,26,50),
        c(22,26.5,33),
        c(23,4.8,8.3),
        c(24,8,10),
        c(25,13.1,8.3),
        c(26,20,5),
        c(27,19.6,4.5),
        c(28,9.3,4.5),
        c(29,5.7,5)
  )
)
colnames(points) = c ("IDPoint","X","Y")
polygon.labels <- data.frame(
  Label=c("ARC",
          "Oceanic Plateau Basalts",
          "N-MORB",
          "OIB"
  ))
polygon.labels$IDLabel=1:nrow(polygon.labels)
polygons <- data.frame(
  rbind(c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),c(1,7),
        c(2,8),c(2,9),c(2,10),c(2,11),c(2,12),c(2,13),c(2,14),c(2,15),
        c(3,16),c(3,17),c(3,18),c(3,19),c(3,20),c(3,21),c(3,22),
        c(4,23),c(4,24),c(4,25),c(4,26),c(4,27),c(4,28),c(4,29)
  )
)
polygons$PointOrder <- 1:nrow(polygons)
colnames(polygons) = c("IDLabel","IDPoint","PointOrder")

df <- merge(polygons,points)
df <- merge(df,polygon.labels)
df <- df[order(df$PointOrder),]

Labs = ddply(df,"Label",function(x){c(c(mean(x$X),mean(x$Y)))})
colnames(Labs) = c("Label","X","Y")

Labs[3,2] <- 15
Labs[3,3] <- 11


mantle <- data.frame(
  rbind(c(2,18),
        c(8.3,15),
        c(19.3,30.5),
        c(25,15),
        c(13.2,5))
)
colnames(mantle) = c("X","Y")

# строим график
p3 <- ggplot(tr.el) + 
  # scale_x_continuous(limits=c(0, 10)) +
  # scale_y_continuous(limits=c(0.4, 1)) +
  labs(fill = NULL) +
  scale_fill_manual(values = 'TECTONIC.SETTING') +
  theme_bw()
p3

condy_plot <- p3 + geom_point(data=tr.el,aes(nb_th, zr_nb, color=TECTONIC.SETTING), shape = 20, alpha=0.5, size = 2, stroke = 0.5) +
  geom_point(data=mantle, aes(X,Y), shape=19, size=5) +
  geom_polygon(data=df,aes(X,Y, group=Label),color="black", alpha=0.01, show.legend=F) +
  geom_text(data=Labs,aes(X,Y,label=Label),size=2.7,color="black", alpha=0.8) + 
  annotate("text", x = 3.5, y = 20.5, label = "EN", size = 2.7, fontface='bold') + 
  annotate("text", x = 9.7, y = 13.5, label = "PM", size = 2.7, fontface='bold') +
  annotate("text", x = 21, y = 35, label = "DM", size = 2.7, fontface='bold') +
  annotate("text", x = 26, y = 12, label = "DEP", size = 2.7, fontface='bold') +
  annotate("text", x = 11, y = 5, label = "REC", size = 2.7, fontface='bold') +
  xlab(TeX("Nb/Th")) +
  ylab(TeX("Zr/Nb")) +
  scale_x_continuous(limits=c(0, 30)) +
  scale_y_log10(limits=c(4, 100))  +
  theme(legend.position="none") 

condy_plot
ggsave("condy.pdf", plot=condy_plot, width = 20, height = 15, units = c("cm")) 
 

# предыдущие графики показались сложными, поскольку на них много всяких полей и оформления.
# последний график La/Nb -- Nb/Th покажется супер простым!

last_plot <- ggplot(tr.el) + geom_point(aes(LA/NB,NB/TH, color=TECTONIC.SETTING), alpha=0.5) +
  scale_x_continuous(limits=c(0, 10)) +
  scale_y_continuous(limits=c(0, 50)) 
last_plot 

ggsave("plot3.pdf", plot=last_plot, width = 20, height = 15, units = c("cm"))
