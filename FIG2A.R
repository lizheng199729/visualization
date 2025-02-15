library(readxl)
library(tidyverse)
library(ggplot2)
#读入数据
data <- read_excel("工作簿1.xlsx")
# 统计代谢物及其分类出现的次数，以确定散点的大小：
metabolite_count <- as.data.frame(table(data$`Serum metabolite`))
Polutant_count <- as.data.frame(table(data$Pollutant))
Outcome_count <- as.data.frame(table(data$Outcome))

# 给每个散点赋予一个坐标轴位置：
metabolite_count$x[1:nrow(metabolite_count)] <- 1:nrow(metabolite_count)
metabolite_count$y[1:nrow(metabolite_count)] <- 1.5
# 这个数字根据后面的图形自行修改：
Polutant_count$x[1:nrow(Polutant_count)] <- c(10, 25, 40, 55)
Polutant_count$y[1:nrow(Polutant_count)] <- 4
# 这个数字根据后面的图形自行修改：
Outcome_count$x[1:nrow(Outcome_count)] <- c(20, 40)
Outcome_count$y[1:nrow(Outcome_count)] <- -5

# 合并数据：
data_count <- rbind(rbind(metabolite_count, Polutant_count), Outcome_count)
colnames(data_count)[1] <- "Serum metabolite"
# 加入分组信息 -- 到这绘制散点的数据算是完成了！
data_count <- left_join(data_count, data[,1:2], by = "Serum metabolite")
data_count$`Super pathway`[c(78:78)] <- "BP"
data_count$`Super pathway`[c(79:79)] <- "CC"
data_count$`Super pathway`[c(80:80)] <- "MF"
data_count$`Super pathway`[c(81:81)] <- "MF"
data_count$`Super pathway`[c(82:83)] <- "CC"
########## 先绘制散点 ----------------
colors <- c("#2da3d1", "#806766", "#d2b698", "#806766",
            "#55bfe2", "#b2bec5", "#d2b698")
names(colors) <- unique(data_count$`Super pathway`)[1:3]
#colors <- c(colors, "BP" = "#1999a9", "CC" = "#efc000",
"MF" = "#9a8419")

p <- ggplot(data_count)+
  geom_point(aes(x, y, size = Freq, color = `Super pathway`))+
  geom_text(data = data_count[1:77,],
            aes(x, y-0.5, label = `Serum metabolite`, color = `Super pathway`),
            angle = 90, hjust = 0.95, vjust = 0.1, size = 2.4, show.legend = F)+
  geom_text(data = data_count[78:81,],
            aes(x+3, y-0.3, label = `Serum metabolite`, color = `Super pathway`),
            angle = 0, hjust = 0, size = 4, show.legend = F)+
  geom_text(data = data_count[82:83,],
            aes(x, y-0.5, label = `Serum metabolite`, color = `Super pathway`),
            angle = 0, hjust = 0.5, vjust = 0.5, size = 4, show.legend = F)+
  scale_color_manual(name = "Class", values = colors)+
  theme_void()

p

########### 折线的数据 ---------------
data_line <- data[, c(1,3)]
data_line[78:(77*2),] <- data[, c(1,4)]
data_line$group <- paste0("group", 1:nrow(data_line))
data_line <- pivot_longer(data_line, cols = -group,
                          names_to = "Class", values_to = "Serum metabolite")

data_line <- left_join(data_line, unique(data_count[,c(1,3,4)]), by = "Serum metabolite")

p+geom_line(data = data_line, aes(x, y, group = group, color = "#b7bfcb"),
            linewidth = 0.2, alpha = 0.3, show.legend = F)

ggsave("plot.png", height = 5.7, width = 10)
