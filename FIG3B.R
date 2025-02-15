setwd('F:/BaiduNetdiskDownload')
immgene <- function(genenames){
  data_matrix <- genedata[genenames,c(1:899)]
  data_matrix <- na.omit(data_matrix)
  data_matrix <- t(data_matrix)
  cor_list <- rcorr(as.matrix(data_matrix),as.matrix(immunedata))
  m<-ncol(data_matrix)
  n<-ncol(immunedata)
  #提取相关性与显著性
  cor_value <- cor_list$r[1:m, (m+1):(m+n)]
  p_value <- cor_list$P[1:m, (m+1):(m+n)]
  #转换长型矩阵
  data_cor <- gather(as.data.frame(t(cor_value)),key = "genename",value = "cor")
  data_cor$immune_cell <- rep(paste(colnames(cor_value)),m)
  data_p <- gather(as.data.frame(t(p_value)),key = "genename",value = "pvalue")
  data_p$immune_cell <- rep(paste(colnames(p_value)),m)
  data_result <- cbind(data_cor,data_p$pvalue)
  colnames(data_result)[4] <- "pvalue"
  #修改pos表示方法
  rows_count <- nrow(data_result)
  Neg_Pos <- rep("Neg",rows_count)
  Neg_Pos[which(data_result$cor>0)] <- "Pos"
  data_result$Neg_Pos <- Neg_Pos
  # 修改p值表示方法：
  pvalue <- rep(NA,rows_count)
  pvalue[which(data_result$pvalue > 0.05)] <- ">0.05"
  pvalue[which(data_result$pvalue < 0.05)] <- "<0.05"
  pvalue[which(data_result$pvalue < 0.01)] <- "<0.01"
  pvalue[which(data_result$pvalue < 0.001)] <- "<0.001"
  pvalue[which(data_result$pvalue < 0.0001)] <- "<0.0001"
  data_result$pvalue2 <- pvalue
  p <- ggplot(data_result,aes(immune_cell,genename))+
    # 蓝色气泡图：
    geom_point(aes(fill=pvalue2, size=abs(cor)),color = "#999999",shape=21)+
    scale_fill_manual(values = c("#212c5f","#3366b1","#42b0e4","#7bc6ed","#dfe1e0"))+
    # 红色气泡图：
    geom_point(data = data_result[which(data_result$Neg_Pos == "Pos"),],
               aes(color=pvalue2, size=abs(cor)),shape=16)+
    scale_color_manual(values = c("#f26666","#f49699","#facccc","#facccc","#d9dbd9"))+
    # 主题：
    theme_bw()+
    theme(panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),
          # 坐标轴label方向：
          axis.text.x = element_text(angle = 45, hjust = 1),
          # 图例间距：
          legend.margin = margin(20,unit = 'pt'))+
    xlab("")+
    ylab("")+
    # 图例：
    guides(size = guide_legend(title = "Spearman's"),
           fill = guide_legend(title = expression("Negtive \ncorrelation \nFDR q-value")),
           col = guide_legend(title = expression("Positive \ncorrelation \nFDR q-value")))
  selected_columns <- data_result[, c(1, 2, 3, 4)]
  result <- list(plotData = p, tableData = selected_columns)
  return(p)
}

