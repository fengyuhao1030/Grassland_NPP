rm(list = ls())
library(ggplot2)
library(grid)
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
                   axis.title.y = element_text(size = 9,margin = margin(0,0,0,0),face = 'bold',family = 'Times'),
                   axis.title.x = element_text(size = 9,margin = margin(5,0,0,0),face = 'bold',family = 'Times'),
                   axis.text.y = element_text(size = 9,margin = margin(0,6,0,0),family = 'Times',color = '#000000'),
                   axis.text.x = element_text(size = 9,margin = margin(8,0,0,0),family = 'Times',color = '#000000'))
  return(myTheme)
}
##==== Function ====##

# ANPP
anppMatrix <- read.csv('ANPP_VarImp.csv')
colnames(anppMatrix) <- c('Variable','Value','Class')
valueOrder <- order(anppMatrix$Value)
newValues <- anppMatrix$Value[valueOrder]
newVariables <- anppMatrix$Variable[valueOrder]
newClass <- anppMatrix$Class[valueOrder]
figAData <- data.frame(XLocation = seq(1,length(newValues)),Value = newValues,VarName = newVariables,Class = newClass)
figAData$Class <- as.factor(figAData$Class)
Fig_5a <- ggplot(data = figAData,mapping = aes(x = XLocation,y = Value,fill = Class))+
  geom_col(width = 0.75,color = '#000000',size = 0.3)+
  scale_x_continuous(breaks = seq(1,nrow(figAData)),labels = figAData$VarName)+
  scale_y_continuous(limits = c(0,18),breaks = seq(0,15,5),labels = c('0','5','10','15'),expand = expansion(mult = 0.04))+
  scale_fill_manual(values = c('#5d9dce','#be7424','#c0ca4c','#f08080'))+
  coord_flip()+
  ylab('%IncMSE')+
  theme_custom()+
  theme(axis.title.y = element_blank())
pdfName <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Fig_5a.pdf')
pdf(pdfName,width = 3,height = 3.2)
print(Fig_5a)
dev.off()

# BNPP
bnppMatrix <- read.csv('BNPP_VarImp.csv')
colnames(bnppMatrix) <- c('Variable','Value','Class')
valueOrder <- order(bnppMatrix$Value)
newValues <- bnppMatrix$Value[valueOrder]
newVariables <- bnppMatrix$Variable[valueOrder]
newClass <- bnppMatrix$Class[valueOrder]
figBData <- data.frame(XLocation = seq(1,length(newValues)),Value = newValues,VarName = newVariables,Class = newClass)
figBData$Class <- as.factor(figBData$Class)
Fig_5b <- ggplot(data = figBData,mapping = aes(x = XLocation,y = Value,fill = Class))+
  geom_col(width = 0.75,color = '#000000',size = 0.3)+
  scale_x_continuous(breaks = seq(1,nrow(figBData)),labels = figBData$VarName)+
  scale_y_continuous(limits = c(0,21.7),breaks = seq(0,20,5),labels = c('0','5','10','15','20'),expand = expansion(mult = 0.04))+
  scale_fill_manual(values = c('#5d9dce','#be7424','#c0ca4c','#f08080'))+
  coord_flip()+
  ylab('%IncMSE')+
  theme_custom()+
  theme(axis.title.y = element_blank())
pdfName <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Fig_5b.pdf')
pdf(pdfName,width = 3,height = 3.2)
print(Fig_5b)
dev.off()

# TNPP
tnppMatrix <- read.csv('TNPP_VarImp.csv')
colnames(tnppMatrix) <- c('Variable','Value','Class')
valueOrder <- order(tnppMatrix$Value)
newValues <- tnppMatrix$Value[valueOrder]
newVariables <- tnppMatrix$Variable[valueOrder]
newClass <- tnppMatrix$Class[valueOrder]
figCData <- data.frame(XLocation = seq(1,length(newValues)),Value = newValues,VarName = newVariables,Class = newClass)
figCData$Class <- as.factor(figCData$Class)
Fig_5c <- ggplot(data = figCData,mapping = aes(x = XLocation,y = Value,fill = Class))+
  geom_col(width = 0.75,color = '#000000',size = 0.3)+
  scale_x_continuous(breaks = seq(1,nrow(figCData)),labels = figCData$VarName)+
  scale_y_continuous(limits = c(0,21),breaks = seq(0,20,5),labels = c('0','5','10','15','20'),expand = expansion(mult = 0.04))+
  scale_fill_manual(values = c('#5d9dce','#be7424','#c0ca4c','#f08080'))+
  coord_flip()+
  ylab('%IncMSE')+
  theme_custom()+
  theme(axis.title.y = element_blank())
pdfName <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Fig_5c.pdf')
pdf(pdfName,width = 3,height = 3.2)
print(Fig_5c)
dev.off()

# fBNPP
fbnppMatrix <- read.csv('fBNPP_VarImp.csv')
colnames(fbnppMatrix) <- c('Variable','Value','Class')
valueOrder <- order(fbnppMatrix$Value)
newValues <- fbnppMatrix$Value[valueOrder]
newVariables <- fbnppMatrix$Variable[valueOrder]
newClass <- fbnppMatrix$Class[valueOrder]
figDData <- data.frame(XLocation = seq(1,length(newValues)),Value = newValues,VarName = newVariables,Class = newClass)
figDData$Class <- as.factor(figDData$Class)
Fig_5d <- ggplot(data = figDData,mapping = aes(x = XLocation,y = Value,fill = Class))+
  geom_col(width = 0.75,color = '#000000',size = 0.3)+
  scale_x_continuous(breaks = seq(1,nrow(figDData)),labels = figDData$VarName)+
  scale_y_continuous(limits = c(0,47),breaks = seq(0,40,10),labels = c('0','10','20','30','40'),expand = expansion(mult = 0.04))+
  scale_fill_manual(values = c('#5d9dce','#be7424','#c0ca4c','#f08080'))+
  coord_flip()+
  ylab('%IncMSE')+
  theme_custom()+
  theme(axis.title.y = element_blank())
pdfName <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'\\Fig_5d.pdf')
pdf(pdfName,width = 3,height = 3.2)
print(Fig_5d)
dev.off()