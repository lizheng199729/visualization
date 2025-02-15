setwd('F:/BaiduNetdiskDownload/024矩阵饼图+分组注释')
#devtools::install_local('F:/BaiduNetdiskDownload/024矩阵饼图+分组注释/jjPlot-main.zip')
library(jjPlot)
library(readxl)
library(jjAnno)

data_long <- read.table("处理后数据.txt",sep = "\t", header = TRUE)
data_long <- read_excel("氧化应激.xlsx")
# 绘图：
p <- ggplot(data_long, aes(x = Mutated_Gene,
                 y = Subtype,
                 group = group)) +
  geom_jjPointPie(aes(pievar = Mutations_number,
                      fill = Correlation_Type),
                  width = 1,
                  color = NA,
                  line.size = 0)+
  scale_fill_manual(values = c("Co-occurrence" = "#cc0000",
                               "Exclusivity" = "#0090b4",
                               "Grey" = "#dddddd",
                               "Not significant" = "#666666"))+
  scale_y_discrete(expand = c(0.9,0.9))+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 80, hjust=0.5, vjust = 1,
                                   face = "italic",size = 25),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(t = 2,r = 1, l = 1, b = 0.001, unit = 'cm'))+
  coord_cartesian(clip = 'off')
print(p)
write.table(data_long,file = "处理后数据.txt",sep = "\t",row.names=T,col.names = T)
p1 <- annoSegment(object = p,
            annoPos = 'top',
            aesGroup = T,
            aesGroName = 'Grobname',
            yPosition = 10.5,
            segWidth = 0.5,
            pCol = rep("black", 6),
            addBranch = T,
            addText = T,
            textCol = rep("black", 6),
            textSize = 30,
            lwd = 1,
            branDirection = -1)
print(p1)
ggsave("plot氧化应激4.png", plot = p, height = 8, width = 20)


