setwd("~/OneDrive/14-r/petdb/") 

library(dplyr)
library(plyr)
library(ggplot2)
library(latex2exp)
library(tidyverse)
library(sf)
library(mapview)
library(reshape2)

#======= загрузка данных ========
morb_and_arc <- read.csv("earthchem_download_14558.txt", sep = "\t", header = T)
lip <- read.csv("earthchem_download_62582.csv", sep = ",", header = T)

#======= построение карты =======
morb_and_arc <- morb_and_arc %>% 
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))

lip <- lip %>% 
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))


mapview(morb_and_arc, xcol = "LONGITUDE", ycol = "LATITUDE", crs = 4269, grid = FALSE)

#========== посмотрим статистики, проверим и подготовим данные по оксидам для построения TAS диаграммы ======

ma_ox.el <- data.frame(morb_and_arc[,c(18:30)])
summary(ma_ox.el$SIO2)
summary(ma_ox.el$TIO2)
summary(ma_ox.el$K2O)
summary(ma_ox.el$NA2O)

plot(ma_ox.el$SIO2, ma_ox.el$K2O+ma_ox.el$NA2O)

ma_tas <- ma_ox.el[,c(1,2,11,12)] # выберем данные

ma_tas <- ma_tas %>%  # удаление строк с отсутствующими значениями в ячейках
  filter(!is.na(NA2O), !is.na(K2O), !is.na(SIO2))

ma_tas <- ma_tas %>%  # немного отфильтруем
  filter(K2O < 10, K2O > 0.01, SIO2 > 30)

ma_tas <- data.frame(ma_tas) # запись таблицы как dataframe

plot(ma_tas$SIO2, ma_tas$K2O+ma_tas$NA2O)
ggplot(ma_tas, aes(x=SIO2, y=K2O+NA2O, color=TECTONIC.SETTING)) + geom_point()

#===========

lip_ox.el <- data.frame(lip[,c(19:32)])

summary(lip_ox.el$SIO2)
summary(lip_ox.el$TIO2)

lip_tas <- lip_ox.el[,c(1,12,13)]
lip_tas['TECTONIC.SETTING'] = 'LIP'

lip_tas <- lip_tas %>%  
  filter(!is.na(NA2O), !is.na(K2O), !is.na(SIO2)) %>%
  filter(K2O < 10, K2O > 0.01, SIO2 > 30, NA2O >0.01)

summary(lip_tas$K2O)

lip_tas <- transform(            # если по каким то причинам в данных закрались non numeric данные
  lip_tas,K2O = as.numeric(K2O),
          NA2O = as.numeric(NA2O),
          SIO2 = as.numeric(SIO2))

lip_tas <- data.frame(lip_tas)

summary(lip_tas$K2O)

#===== plot TAS on LIP setting ========
plot(lip_tas$SIO2, lip_tas$K2O+lip_tas$NA2O)
ggplot(lip_tas, aes(x=SIO2, y=K2O+NA2O, color=TECTONIC.SETTING)) + geom_point()

#====== combine together: Spreading, Arcs and LIPs ======
whole.data_tas <- rbind(ma_tas, lip_tas)

ggplot(whole.data_tas, aes(x=SIO2, y=K2O+NA2O, color=TECTONIC.SETTING)) + geom_point()

#---------------- TAS diagram with rock fields----------------------
#----------------Firstly create the field polygons -----------------
#library(plyr)
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
#--------------- we prepared the polygons, ready to plot our data -------------------

tas_dia1 <- ggplot(whole.data_tas, aes(x=SIO2, y=K2O+NA2O, color=TECTONIC.SETTING)) + geom_point(shape=20, size=2, alpha=0.5) +
  geom_polygon(data=df,aes(X,Y, group=Label),color="black", alpha=0.01, show.legend=F) +
  geom_text(data=Labs,aes(X,Y,label=Label),size=2.7,color="black", alpha=0.8) +
  theme_bw()

tas_dia1

ggsave("tas.pdf", plot=tas_dia1, width = 25, height = 15, units = c("cm"))

tas_density <- ggplot(whole.data_tas, aes(x=SIO2, y=K2O+NA2O)) + 
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = TECTONIC.SETTING)) +
  geom_polygon(data=df,aes(X,Y, group=Label),color="black", alpha=0.01, show.legend=F) +
  geom_text(data=Labs,aes(X,Y,label=Label),size=2.7,color="black", alpha=0.8) +
  theme_bw()

tas_density

ggplot(whole.data_tas, aes(x=SIO2, y=K2O+NA2O)) + geom_bin2d(bins = 400) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

#======== Trace elements pattern ====

ma_tr.el <- morb_and_arc[,c("TECTONIC.SETTING","RB","BA","TH","U","TA","NB","LA","CE","PB","PR","SR","ND","ZR","HF","SM","EU","GD","TB","DY","Y","HO","ER","TM","YB","LU")]
colnames(ma_tr.el) <- c("TECTONIC.SETTING","Rb","Ba","Th","U","Ta","Nb","La",
                     "Ce","Pb","Pr","Sr","Nd","Zr","Hf","Sm","Eu","Gd","Tb",
                     "Dy","Y","Ho","Er","Tm","Yb","Lu")
ma_tr.el$K <- 10000*ma_ox.el$K2O*39/(2*39+16)
ma_tr.el$P <- 10000*ma_ox.el$P2O5*2*31/(2*31+5*16)
ma_tr.el$Ti <- 10000*ma_ox.el$TIO2*48/(48+2*16)

ma_tr.el <- na.omit(ma_tr.el)

ma_tr.el.num <- as.data.frame(lapply(ma_tr.el, as.numeric))
ma_tr.el.num$TECTONIC.SETTING <- ma_tr.el$TECTONIC.SETTING

spreading <- filter(ma_tr.el.num, TECTONIC.SETTING == 'SPREADING_CENTER')
arcs <- filter(ma_tr.el.num, TECTONIC.SETTING == 'VOLCANIC_ARC')

# нормируем данные
x <- read.table("PM_1989.csv", header = TRUE, sep = ",", row.names = 1)

primitive.mantle <- as.numeric(x)
names(primitive.mantle) <- names(x)

norm <- function(x,pm) {
  z <- t(x[,names(pm)])/pm
  return(z)
}
norm.data.spreading <- norm(spreading,primitive.mantle)
norm.data.arcs <- norm(arcs,primitive.mantle)

# нужно "расплавить" таблицы, чтобы получилось 3 колонки
melt.spreading <- melt(norm.data.spreading) # reshape data
colnames(melt.spreading) <- c("element","sample","values")

melt.arcs <- melt(norm.data.arcs)
colnames(melt.arcs) <- c("element","sample","values")

# подгружаем референтные значения и нормируем
ref <- read.csv("morb.csv",  sep = ",", row.names = 1)
y1 <- norm(ref,primitive.mantle)
ref.data <- melt(y1)  
colnames(ref.data) <- c("element","sample","values")

nmorb <- ref.data[c(1:28),]
emorb <- ref.data[c(57:84),]
kamch2 <- ref.data[c(113:140),]
kamch1 <- ref.data[c(85:112),]
bhvo <- ref.data[c(141:168),]
oib <- ref.data[c(169:196),]
iab <- ref.data[c(253:280),]
iab1 <- ref.data[c(281:308),]
iab2 <- ref.data[c(309:336),]


cols <- c("Spreading" = "darkgreen", 
          "Arcs"= "#EF7F1A", 
          "OIB"="#6FC597", 
          "NMORB" = "#948780", 
          "EMORB" = "#FFCA43", 
          "SMT" = "darkorchid4", 
          "SSP" = "darkviolet", 
          "LIP" = "coral2")

base <- ggplot(oib, aes(x=element, y=values, group=sample, colour = "OIB")) + 
  #     geom_path(size = 1.5) +
  #     geom_path(data=emorb, aes(element, values, colour = "EMORB"), size = 1.5) +
  #     geom_path(data=iab1, aes(element, values, colour = "SMT"), size = 1.5) +
  #     geom_path(data=iab2, aes(element, values, colour = "SSP"), size = 1.5) +
  scale_y_log10(limits=c(0.5, 1000)) + 
  theme_bw() +
  scale_color_manual(values = cols) +
  labs(title = NULL, colour = NULL) +
  ylab("Sample / Primitive Mantle (PM)") +
  theme(axis.title.x=element_blank()) +
  theme(legend.position = c(.8, .8),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(1, 1, 1, 1))
base

spider <- base + 
  geom_path(data=melt.spreading, aes(x=element, y=values, group=sample, colour = "Spreading"), alpha=0.5) +
  #geom_path(data=melt.arcs, aes(element, values, colour = "Arcs"), alpha=0.5) +
  geom_path(data=iab1, aes(element, values, colour = "SMT"), size = 1.5) +
  geom_path(data=oib, aes(element, values, colour = "OIB"), size = 1.5) +
  geom_path(data=nmorb, aes(element, values, colour = "NMORB"), size = 1.5) 

spider

#===== LIP trace elements pattern ======

lip_tr.el <- lip[,c("RB","BA","TH","U","TA","NB","LA","CE","PB","PR","SR","ND","ZR","HF","SM","EU","GD","TB","DY","Y","HO","ER","TM","YB","LU")]
lip_tr.el['TECTONIC.SETTING'] = 'LIP'
lip_tr.el <- lip_tr.el[,c(26,1:25)]
colnames(lip_tr.el) <- c("TECTONIC.SETTING","Rb","Ba","Th","U","Ta","Nb","La",
                        "Ce","Pb","Pr","Sr","Nd","Zr","Hf","Sm","Eu","Gd","Tb",
                        "Dy","Y","Ho","Er","Tm","Yb","Lu")


lip_ox.el <- transform(
  lip_ox.el,K2O = as.numeric(K2O),
  P2O5 = as.numeric(P2O5),
  TIO2 = as.numeric(TIO2))

lip_tr.el$K <- 10000*lip_ox.el$K2O*39/(2*39+16)
lip_tr.el$P <- 10000*lip_ox.el$P2O5*2*31/(2*31+5*16)
lip_tr.el$Ti <- 10000*lip_ox.el$TIO2*48/(48+2*16)

lip_tr.el <- na.omit(lip_tr.el)

norm.data.lip <- norm(lip_tr.el,primitive.mantle)
melt.lip <- melt(norm.data.lip) # reshape data
colnames(melt.lip) <- c("element","sample","values")

spider_lip <- base + 
  #geom_path(data=melt.spreading, aes(x=element, y=values, group=sample, colour = "Spreading"), alpha=0.5) +
  geom_path(data=melt.lip, aes(element, values, colour = "LIP"), alpha=0.5) +
  geom_path(data=iab1, aes(element, values, colour = "SMT"), size = 1.5) +
  geom_path(data=oib, aes(element, values, colour = "OIB"), size = 1.5) +
  geom_path(data=nmorb, aes(element, values, colour = "NMORB"), size = 1.5) 

spider_lip



#===== Plot 1 (Th/Nb vs TiO2/Yb) =======

x1 <- lip_ox.el$TIO2/lip_tr.el$Yb
y1 <- lip_tr.el$Th/lip_tr.el$Nb

ggplot(lip_tr.el, aes(x1,y1)) + geom_point()

reduced <- lip_tr.el %>% 
  filter(!is.na(Yb), !is.na(Th), !is.na(Ti), !is.na(Nb))



#================== Trace elements ======

ma_tr.el <- morb_and_arc[,c("TECTONIC.SETTING","RB","BA","TH","U","TA","NB","LA","CE","PB","PR","SR","ND","ZR","HF","SM","EU","GD","TB","DY","Y","HO","ER","TM","YB","LU")]
colnames(ma_tr.el) <- c("TECTONIC.SETTING","Rb","Ba","Th","U","Ta","Nb","La",
                        "Ce","Pb","Pr","Sr","Nd","Zr","Hf","Sm","Eu","Gd","Tb",
                        "Dy","Y","Ho","Er","Tm","Yb","Lu")
#ma_tr.el <- na.omit(ma_tr.el)

lip_tr.el <- lip[,c("RB","BA","TH","U","TA","NB","LA","CE","PB","PR","SR","ND","ZR","HF","SM","EU","GD","TB","DY","Y","HO","ER","TM","YB","LU")]
#lip_tr.el <- na.omit(lip_tr.el)
lip_tr.el['TECTONIC.SETTING'] = 'LIP'
lip_tr.el <- lip_tr.el[,c(26,1:25)]
colnames(lip_tr.el) <- c("TECTONIC.SETTING","Rb","Ba","Th","U","Ta","Nb","La",
                        "Ce","Pb","Pr","Sr","Nd","Zr","Hf","Sm","Eu","Gd","Tb",
                        "Dy","Y","Ho","Er","Tm","Yb","Lu")

whole_tr.el <- rbind(ma_tr.el, lip_tr.el)

#------------------------- Pearce -------------------------------------------------
#==================================================================================
# в данной диаграмме x=Nb/Yb, y=Th/Yb. Как вариант, можно прямо в параметрах ggplot их вычислить. 
# Но не менее удобно их сразу вычислить и записать в отдельные столбцы. 

whole_tr.el$nb_yb <- whole_tr.el$Nb/whole_tr.el$Yb
whole_tr.el$th_yb <- whole_tr.el$Th/whole_tr.el$Yb

pearce_data <- whole_tr.el[,c("TECTONIC.SETTING", "nb_yb","th_yb")]

pearce_data <- pearce_data %>% 
  filter(!is.na(nb_yb), !is.na(th_yb))

# собственно графики 
# сначала выведем точки с данными, здесь уже настроены оси в лог-ом масштабе, легенда, подписи осей
p <- ggplot() +
  geom_point(data=pearce_data, aes(x=nb_yb, y=th_yb, color=TECTONIC.SETTING), shape=20, size=2, alpha=0.5) +
  scale_x_continuous(trans = 'log10', limits=c(0.1, 100)) +
  scale_y_continuous(trans = 'log10', limits=c(0.01, 10)) +
  annotation_logticks()  +
  labs(title = NULL, fill = NULL) +
  ylab("Th/Yb") +
  xlab("Nb/Yb") +
  theme_bw() +
  theme(legend.position = c(.85, .2),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3))

p

#====== сделаем заготовки линий и обозначений на диаграмме ======
p_line1 <- data.frame(
  rbind(c( 1,0.1,0.01),
        c( 2,70,10)))
colnames(p_line1) = c ("IDPoint","X","Y")

p_line2 <- data.frame(
  rbind(c( 1,0.3,0.01),
        c( 2,100, 4.5)))
colnames(p_line2) = c ("IDPoint","X","Y")

p_line3 <- data.frame(
  rbind(c( 1,0.1,1.25),
        c( 2,0.8, 10)))
colnames(p_line3) = c ("IDPoint","X","Y")


MORB <- data.frame(
  rbind(c(1,0.7,0.045),
        c(2,3.5,0.25),
        c(3,23,2)))

# и добавим их на диаграмму:

pearce <- p + geom_point(data=MORB, aes(x=X2,y=X3), shape = 21, size = 4, col = "red", fill = "yellow" ) +
  geom_path(data=p_line1, aes(X,Y)) + geom_path(data=p_line2, aes(X,Y)) + geom_path(data=p_line3, aes(X,Y)) +
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

pearce # вывод диаграммы

# игра с 2d density
pearce_dp <- ggplot(pearce_data, aes(nb_yb, th_yb)) + 
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = TECTONIC.SETTING))+
  scale_x_continuous(trans = 'log10', limits=c(0.1, 100)) +
  scale_y_continuous(trans = 'log10', limits=c(0.01, 10)) +
  annotation_logticks()  +
  labs(title = NULL, fill = NULL) +
  ylab("Th/Yb")+
  xlab("Nb/Yb") +
  theme_bw() +
  theme(legend.position = c(.85, .2),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3)) +
  geom_point(data=MORB, aes(x=X2,y=X3), shape = 21, size = 4, col = "red", fill = "yellow" ) +
  geom_path(data=p_line1, aes(X,Y)) + geom_path(data=p_line2, aes(X,Y)) + geom_path(data=p_line3, aes(X,Y)) +
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

pearce_dp
# ============ Pearce 2 -- TiO2/Yb vs Th/Nb =============== Pearce et al., 2021 ==========

lip_ox.el['TECTONIC.SETTING'] = 'LIP'
lip_ti <- lip_ox.el[,c(15,2)]
ma_ti <- ma_ox.el[,c(1,3)]
ti <- rbind(ma_ti,lip_ti)

vars <- whole_tr.el[,c(4,7,25)]
pearce_data2 <- cbind(ti,vars)
pearce_data2 <- na.omit(pearce_data2)

pearce_data2$ti_yb <- pearce_data2$TIO2/pearce_data2$Yb
pearce_data2$th_nb <- pearce_data2$Th/pearce_data2$Nb

p1 <- ggplot() +
  geom_point(data=pearce_data2, aes(x=ti_yb, y=th_nb, color=TECTONIC.SETTING), shape=20, size=2, alpha=0.5) +
  scale_x_continuous(trans = 'log10', limits=c(0.1, 10)) +
  scale_y_continuous(trans = 'log10', limits=c(0.03, 3)) +
  annotation_logticks()  +
  labs(title = NULL, fill = NULL) +
  ylab("Th/Yb") +
  xlab("Nb/Yb") +
  theme_bw() +
  theme(legend.position = c(.81, .85),
        legend.box.background = element_rect(color="black", size=0.5),
        legend.box.margin = margin(3, 3, 3, 3))

p1

# 
p2_line1 <- data.frame(
  rbind(c(1,0.2,0.03),
        c(2,0.2,3)))
colnames(p2_line1) = c ("IDPoint","X","Y")

p2_line2 <- data.frame(
  rbind(c(1,0.8,0.03),
        c(2,0.8,3)))
colnames(p2_line2) = c ("IDPoint","X","Y")

p2_line3 <- data.frame(
  rbind(c(1,4,0.03),
        c(2,4,0.2)))
colnames(p2_line3) = c ("IDPoint","X","Y")

p2_line4 <- data.frame(
  rbind(c(1,0.2,0.14),
        c(2,4,0.14)))
colnames(p2_line4) = c ("IDPoint","X","Y")

p2_line5 <- data.frame(
  rbind(c(1,0.8,0.2),
        c(2,4,0.2)))
colnames(p2_line5) = c ("IDPoint","X","Y")

p2_line6 <- data.frame(
  rbind(c(1,0.6,0.03),
        c(2,0.6,0.14)))
colnames(p2_line6) = c ("IDPoint","X","Y")

p2 <- p1 + 
  geom_path(data=p2_line1, aes(X,Y)) + 
  geom_path(data=p2_line2, aes(X,Y)) + 
  geom_path(data=p2_line3, aes(X,Y)) +
  geom_path(data=p2_line4, aes(X,Y)) +
  geom_path(data=p2_line5, aes(X,Y)) +
  geom_path(data=p2_line6, aes(X,Y)) +
  annotate("text", x=0.4, y = 2, label = "IAB array", angle= 0) +
  annotate("text", x=0.34, y = 0.034, label = "MORB-OPB", angle= 0) +
  annotate("text", x=1.8, y = 0.034, label = "OIB-OPB", angle= 0) +
  annotate("text", x=1.8, y = 0.17, label = "EM-OIB", angle= 0) +
  annotate("text", x=0.7, y = 0.05, label = "transition", angle= 90)

p2

#------------------------- Pearce end-------------------------------------------------



#--------------Condie------------------
library(plyr)

# ==== Nb/Th vs Zr/Nb

condie_data <- whole_tr.el[,c("TECTONIC.SETTING", "Zr","Nb","Th")]
condie_data <- na.omit(condie_data)

# при обработке данных выяснилось, что Zr записан не как числа. 
# Поэтому тут придется немного доработать этот момент. 
# Также готовим столбцы с готовыми отношениями.

zr <- as.numeric(condie_data$Zr)
condie_data$Zr <- zr

condie_data$nb_th <- condie_data$Nb/condie_data$Th
condie_data$zr_nb <- condie_data$Zr/condie_data$Nb

p3 <- ggplot(condie_data) + 
  # scale_x_continuous(limits=c(0, 10)) +
  # scale_y_continuous(limits=c(0.4, 1)) +
  labs(fill = NULL) +
  scale_fill_manual(values = 'TECTONIC.SETTING') +
  theme_bw() 
 # geom_point(aes(nb_th,zr_nb))

p3
# ========= строим поля на диаграмме =========
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

#names <- as.data.frame(
#          rbind(c("EN",3.5,17),
#                c("PM",10,14),
#                c("DM",21,35),
#                c("DEP",26,12),
#                c("REC",11,5)
#          )
#          )
#colnames(names) = c("Label","X","Y")


mantle <- data.frame(
  rbind(c(2,18),
        c(8.3,15),
        c(19.3,30.5),
        c(25,15),
        c(13.2,5))
)
colnames(mantle) = c("X","Y")

# =======

plot3_1 <- p3 + geom_point(data=condie_data,aes(nb_th, zr_nb, color=TECTONIC.SETTING), shape = 20, alpha=0.5, size = 2, stroke = 0.5) +
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
#  theme(legend.position="none") +
 theme(legend.position = c(.85, .75),
        legend.box.background = element_rect(color="black", size=0.3),
        legend.box.margin = margin(1, 1, 1, 1))
plot3_1

condie_dp <- ggplot(data=condie_data,aes(nb_th, zr_nb)) + 
  stat_density_2d(geom = "polygon", aes(alpha = 0.2, fill = TECTONIC.SETTING)) +
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

condie_dp


ggsave("condy.pdf", plot=plot3_1, width = 20, height = 15, units = c("cm")) 
ggsave("pearce.pdf", plot=pearce, width = 18, height = 17, units = c("cm")) 
ggsave("pearce_dp.pdf", plot=pearce_dp, width = 18, height = 17, units = c("cm")) 

# ====== еще один график La/Nb vs Nb/Th =============

plot3_2 <- ggplot(whole_tr.el) + geom_point(aes(La/Nb,Nb/Th, color=TECTONIC.SETTING), alpha=0.5) +
  scale_x_continuous(limits=c(0, 10)) +
  scale_y_continuous(limits=c(0, 50)) 
plot3_2

ggsave("plot3.pdf", plot=plot3_2, width = 20, height = 15, units = c("cm"))

#========
