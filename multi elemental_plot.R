library(reshape2)
library(ggplot2)

data <- read.csv("earthchem_download_14558.txt", sep = "\t", header = T)
ox.el <- data.frame(data[,c(18:30)])
tr.el <- data[,c(18,31:70)]

tr.el <- data[,c("TECTONIC.SETTING","RB","BA","TH","U","TA","NB","LA","CE","PB","PR","SR","ND","ZR","HF","SM","EU","GD","TB","DY","Y","HO","ER","TM","YB","LU")]
colnames(tr.el) <- c("TECTONIC.SETTING","Rb","Ba","Th","U","Ta","Nb","La",
                    "Ce","Pb","Pr","Sr","Nd","Zr","Hf","Sm","Eu","Gd","Tb",
                    "Dy","Y","Ho","Er","Tm","Yb","Lu")
tr.el$K <- 10000*ox.el$K2O*39/(2*39+16)
tr.el$P <- 10000*ox.el$P2O5*2*31/(2*31+5*16)
tr.el$Ti <- 10000*ox.el$TIO2*48/(48+2*16)

tr.el <- na.omit(tr.el)

# оказалось, что не все значения записаны в численном формате, нужно исправить
tr.el.num <- as.data.frame(lapply(tr.el, as.numeric))
tr.el.num$TECTONIC.SETTING <- tr.el$TECTONIC.SETTING

# разбиваем на обстановки
spreading <- filter(tr.el.num, TECTONIC.SETTING == 'SPREADING_CENTER')
arcs <- filter(tr.el.num, TECTONIC.SETTING == 'VOLCANIC_ARC')

# нужно прочитать файл с значениями в PM
x <- read.table("PM_1989.csv", header = TRUE, sep = ",", row.names = 1)
primitive.mantle <- as.numeric(x)
names(primitive.mantle) <- names(x)

# функция для нормирования
norm <- function(x,pm) {
  z <- t(x[,names(pm)])/pm
  return(z)
}
#нормируем каждый из файлов
norm.data.spreading <- norm(spreading,primitive.mantle)
norm.data.arcs <- norm(arcs,primitive.mantle)

# важный момент -- нужно расплавить таблицы
melt.spreading <- melt(norm.data.spreading)
colnames(melt.spreading) <- c("element","sample","values")

melt.arcs <- melt(norm.data.arcs)
colnames(melt.arcs) <- c("element","sample","values")

# читаем, нормируем и расплавляем данные по известным обстановкам
morb <- read.csv("morb.csv",  sep = ",", row.names = 1)
y1 <- norm(morb,primitive.mantle)

new.data <- melt(y1)  # нужно расплавить таблицу. ggplot ест только 3 столбца. 
colnames(new.data) <- c("element","sample","values")

# выбираем нужные строки для каждой из (очень костыльно, кто знает -- предложите красивый способ)
nmorb <- new.data[c(1:28),]
emorb <- new.data[c(57:84),]
kamch2 <- new.data[c(113:140),]
kamch1 <- new.data[c(85:112),]
bhvo <- new.data[c(141:168),]
oib <- new.data[c(169:196),]
iab <- new.data[c(253:280),]
iab1 <- new.data[c(281:308),]
iab2 <- new.data[c(309:336),]

# задаем цветовые коды для всего, что может быть на графике
cols <- c("Spreading" = "darkgreen", "Arcs"= "#EF7F1A", 
          "OIB"="#6FC597", "NMORB" = "#948780", "EMORB" = "#FFCA43", 
          "SMT" = "darkorchid4", "SSP" = "darkviolet"
          )
# строим шаблон
base <- ggplot(oib, aes(x=element, y=values, group=sample, colour = "OIB")) + 
  #geom_path(data=emorb, aes(element, values, colour = "EMORB"), size = 1.5) +
  #     geom_path(data=iab1, aes(element, values, colour = "SMT"), size = 1.5) +
  #    geom_path(data=iab2, aes(element, values, colour = "SSP"), size = 1.5) +
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

# добавляем на шаблон спрединг, дуги и 3 стандартные обстановки. 
spider <- base + 
  geom_path(data=melt.spreading, aes(x=element, y=values, group=sample, colour = "Spreading"), alpha=0.5) +
  geom_path(data=melt.arcs, aes(element, values, colour = "Arcs"), alpha=0.5) +
  geom_path(data=iab1, aes(element, values, colour = "SMT"), size = 1.5) +
  geom_path(data=oib, aes(element, values, colour = "OIB"), size = 1.5) +
  geom_path(data=nmorb, aes(element, values, colour = "NMORB"), size = 1.5) 

# получилась каша :) слишком много данных...
spider
