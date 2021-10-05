rm(list = ls())
library(foreign)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##==== Function ====##
# Custom theme
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',size = 0.5),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(5,5,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(size = 0.2),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.title.y = element_blank(),
                   axis.title.x = element_text(size = 9,margin = margin(0,5,0,0),face = 'bold',family = 'Times'),
                   axis.text.y = element_text(size = 9,margin = margin(0,5,0,0),family = 'Times',color = '#000000'),
                   axis.text.x = element_text(size = 9,margin = margin(5,0,0,0),family = 'Times',color = '#000000'))
  return(myTheme)
}
##==== Function ====##

samplePoint <- read.dbf('Sample_point_Proj.dbf')
dataMatrix <- read.csv('TNPP_LatSum.csv')

dataMatrix$POINT_Y <- 0
for(i in seq(1,nrow(dataMatrix))){
  tempLat <- dataMatrix$Lat[i]
  selectIndex <- which(samplePoint$Lat == tempLat)
  dataMatrix$POINT_Y[i] <- samplePoint$POINT_Y[selectIndex]
}

CIPolygon <- as.data.frame(matrix(data = 0,nrow = (2*nrow(dataMatrix) + 1),ncol = 2))
colnames(CIPolygon) <- c('x','y')
count <- 1
for(i in seq(1,nrow(dataMatrix))){
  CIPolygon$x[count] <- dataMatrix$POINT_Y[i]
  CIPolygon$y[count] <- dataMatrix$Mean[i] + dataMatrix$SD[i]
  count <- count + 1
}
for(i in seq(nrow(dataMatrix),1,-1)){
  CIPolygon$x[count] <- dataMatrix$POINT_Y[i]
  CIPolygon$y[count] <- dataMatrix$Mean[i] - dataMatrix$SD[i]
  count <- count + 1
}
CIPolygon$x[count] <- dataMatrix$POINT_Y[1]
CIPolygon$y[count] <- dataMatrix$Mean[1] - dataMatrix$SD[1]

# Draw
Fig_3c_Lat <- ggplot()+
  geom_polygon(data = CIPolygon,mapping = aes(x = x,y = y),fill = '#3567FF',alpha = 0.2)+
  geom_line(data = dataMatrix,mapping = aes(x = POINT_Y,y = Mean),color = '#3567FF',size = 0.3)+
  scale_x_continuous(limits = c(min(samplePoint$POINT_Y),max(samplePoint$POINT_Y)),breaks = samplePoint$POINT_Y[seq(1,181,30)],
                     labels = c('90','60','30','0','-30','-60','-90'),expand = c(0,0))+
  scale_y_continuous(limits = c(0,2000),breaks = seq(0,2000,500),labels = c('0','500','1000','1500','2000'),expand = c(0,0))+
  ylab('TNPP')+
  coord_flip()+
  theme_custom()
pdfName <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Fig_3c_Lat.pdf')
pdf(pdfName,width = 1.3,height = 1.92)
print(Fig_3c_Lat)
dev.off()