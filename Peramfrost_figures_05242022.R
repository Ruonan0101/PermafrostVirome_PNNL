##fig1: ds, postive, negative
library(ggtree)
library(ggplot2)
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_1/')
###ds
info<-read.csv('ds_id_list_sum_avg.csv')
tree<-read.tree('ds_tree_final_newick.txt')
head(info)
colnames(info)<-c("id","taxa",'Sum','Avg')
library(RColorBrewer)
library(ggnewscale)
library(pheatmap)
library(viridis)
nb.cols <- 21
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

ggtree(tree) +geom_tiplab(size=2, offset = 0.5)+geom_text(aes(label=node), hjust=-0.5, size=2)

p<-ggtree(tree) %<+% info+
  geom_cladelabel(node=48, label="Reoviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=55, label="Hypoviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=22, label="Cystoviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=57, label="Picobirnaviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=61, label="Totiviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=44, label="Endornaviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=34, label="Partitiviridae",color="blue", offset=0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=1, label="OutGroup",color="blue", offset=0.1, align=TRUE,fontsize=2)

p

heatmapData=read.csv("ds_id_list_new_sum_avg_heatmap.csv", row.names=1)
plot1<-gheatmap(p, log(heatmapData), offset = 5,colnames=FALSE, width=0.1,color = FALSE)+
  scale_fill_viridis_c(option="plasma",na.value = '#000000')+
  theme(legend.position='bottom',plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
        legend.key.height= unit(0.2, 'cm'),legend.text=element_text(size=5),legend.title = element_blank())
plot1
###netagtive tree
info<-read.csv('negative_id_list_sum_avg.csv')
tree<-read.tree('negative_newick.txt')
head(info)
colnames(info)<-c("id","taxa",'Sum','Avg')
nb.cols <- 21
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)


ggtree(tree) +geom_tiplab(size=2, offset = 0.5)+geom_text(aes(label=node), hjust=-0.5, size=2)

p2<-ggtree(tree) %<+% info+
  geom_cladelabel(node=26, label="Peribunyaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=6, label="Rhabdoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=5, label="Hantaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=13, label="Phasmaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=12, label="Nairoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=14, label="Phenuiviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=11, label="Tospoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=32, label="Arenaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=4, label="Pneumoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=3, label="Paramyxoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=2, label="Orthomyxoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=1, label="OutGroup",color="blue", offset=-0.1, align=TRUE,fontsize=2)

p2

heatmapData=read.csv("negative_id_list_sum_avg_heatmap.csv", row.names=1)
plot2<-gheatmap(p2, log(heatmapData), offset =5,colnames=FALSE, width=0.1,color = FALSE)+
  scale_fill_viridis_c(option="plasma",na.value = '#000000')+
  theme(legend.position='bottom',plot.margin = unit(c(0.5, 0, 0, 0), "cm"),legend.key.height= unit(0.2, 'cm')
        ,legend.text=element_text(size=5),legend.title = element_blank())
plot2
###positive
info<-read.csv('positive_id_list_newnew_sum_avg.csv')
tree<-read.tree('positive_finafinal_newick.txt')
head(info)
colnames(info)<-c("id","taxa",'Sum','Avg')
nb.cols <- 21
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


ggtree(tree, size=0.1) +geom_tiplab(size=0.5, offset = 0.5)+geom_text(aes(label=node), hjust=-0.5, size=0.5)

p_positive<-ggtree(tree)  %<+% info+
  geom_cladelabel(node=164, label="Tombusviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=162, label="Narnaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=85, label="Closteroviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=86, label="Closteroviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=87, label="Deltaflexiviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=229, label="Solemoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=230, label="Astroviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  #geom_cladelabel(node=230, label="Astroviridae",color="blue", offset=-35, align=TRUE,fontsize=2)+
  geom_cladelabel(node=146, label="Flaviviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=3, label="Alphaflexiviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=15, label="Alphaflexiviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=2, label="Tobaniviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=261, label="Picornaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  #geom_cladelabel(node=261, label="Picornaviridae",color="blue", offset=-27, align=TRUE,fontsize=2)+
  geom_cladelabel(node=277, label="Potyviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=120, label="Caliciviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=119, label="Tymoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=248, label="Virgaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=111, label="Iflaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=257, label="Leviviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=115, label="Mitoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=114, label="Coronaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=113, label="Nodaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=112, label="Coronaviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=118, label="Secoviridae",color="blue", offset=-0.1, align=TRUE,fontsize=2)+
  geom_cladelabel(node=1, label="OutGroup",color="blue", offset=-0.1, align=TRUE,fontsize=2)


p3<-rotate(p_positive, 255)

p3<-collapse(p3, node=164)+geom_point2(aes(subset=(node==164)), shape=15, size=2, fill='grey')
p3<-collapse(p3, node=230)+geom_point2(aes(subset=(node==230)), shape=15, size=2, fill='grey')
p3<-collapse(p3, node=261)+geom_point2(aes(subset=(node==261)), shape=15, size=2, fill='grey')
heatmapData=read.csv("positive_id_list_newnew_sum_avg_heatmap.csv", row.names=1)
plot3<-gheatmap(p3, log(heatmapData), offset =20,colnames=FALSE, width=0.1,color = FALSE)+
  scale_fill_viridis_c(option="plasma",na.value = '#000000')+
  theme(legend.position='right',plot.margin = unit(c(0.5, 0, 0, 0), "cm"), legend.key.width = unit(0.2, 'cm'),
        legend.text=element_text(size=5),legend.title = element_blank())
plot3


p4<-viewClade(p_positive, node=164)
p4
p5<-viewClade(p_positive, node=230)
p6<-viewClade(p_positive, node=261)

plot4<-gheatmap(p4, log(heatmapData), offset = 1.8,colnames=FALSE, width=0.01,color = FALSE)+
  scale_fill_viridis_c(option="plasma",na.value = '#000000', name="log")+
  theme(legend.position='NA',plot.margin = unit(c(1, 0, 0, 0), "lines"))
plot5<-gheatmap(p5, log(heatmapData), offset = -30,colnames=FALSE, width=0.03,color = FALSE)+
  scale_fill_viridis_c(option="plasma",na.value = '#000000', name="log")+
  theme(legend.position='NA',plot.margin = unit(c(1, 0, 0, 0), "lines"))
plot6<-gheatmap(p6, log(heatmapData), offset = -23,colnames=FALSE, width=0.025,color = FALSE)+
  scale_fill_viridis_c(option="plasma",na.value = '#000000', name="log")+
  theme(legend.position='NA',plot.margin = unit(c(1, 0, 0, 0), "lines"))



library(gridExtra)
gridExtra::grid.arrange(arrangeGrob(plot1,plot2), plot3,ncol=2)
gridExtra::grid.arrange(plot4,arrangeGrob(plot5,plot6),ncol=2)

####Fig2a: venn
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_2/ForVenn_dir/1234_CDHIT99_CDHIT70/')
#install.packages("remotes")
#remotes::install_github("yanlinlin82/ggvenn")
library(ggvenn)
library(grid)
d<-read.csv("1234_ForVenn.csv",check.names=FALSE)

figure2 <- ggvenn(d,c('Permafrost','California_Grassland','Kansas_Grassland','Invertebrate'), fill_color = c("#00BFFF", "#694402",'#FFA500',"#F8766D"), 
                  stroke_size = 0.15, set_name_size = 3, text_size=2) + theme_void() + coord_fixed()
figure2

#####Fig2b relative richness#########
library(ggplot2)
data <- data.frame(RelativeRichness=c(
  0.010770505,	0.010814708,
  0.291666667,	0.05,
  0.170886076,	0.098837209,
  0.283687943,	0.137254902), 
  habitat=rep(c("Califonia_Grassland","Invertebrate", "Kansas_Grassland", "Permafrost"), each =2),
  RelativeToClustersClassifiedWith=rep(c("Family" , "Phylogenetic group") , 4))

# Grouped
ggplot(data, aes(fill=RelativeToClustersClassifiedWith, y=RelativeRichness, x=habitat)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c('#BDC3C7','#17202A'))+
  theme_bw()+ 
  theme(axis.text.x = element_text(size=12), legend.text = element_text(size=12))+
  scale_y_continuous(labels = scales::percent_format())

###fig3a: host 
library(ggalluvial)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(colorRamps)
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_4')

d<-read.csv("Cluster_Group_Host1_Host2_forR.csv",check.names=FALSE)
d
fig3a<-ggplot(data = d,aes(axis1 =Group, axis2 = Host, axis3 = HostLineage)) +
  scale_x_discrete(limits = c("Phylogenetic Group", "Host", "Host lineage"),expand = c(0.03, .095)) +
  geom_alluvium(aes(fill = HostLineage), alpha=0.6) +
  geom_stratum(width=1/12, alpha=.6, color='#666666') +
  geom_text_repel(stat = "stratum", size=3, aes(label = after_stat(stratum))) +
  theme_minimal()+
  scale_fill_manual(values = colorRampPalette(brewer.pal(7, "Set1"))(7))+
  theme(panel.background = element_blank(), panel.grid = element_blank(), 
        axis.title.y = element_text(vjust=5),legend.position='right')

fig3a

##fig3b_Eakar group heatmap
library(pheatmap)
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_4')
data<-read.csv("eukaryote_table_corrected_heatmapR.csv",row.names=1,check.names=FALSE)
pheatmap(data)
cal_z_score <- function(x){
  (log(x+1))
}

data_norm <- t(apply(data, 1, cal_z_score))
pheatmap(data_norm)
my_hclust_gene <- hclust(dist(data_norm), method = "complete")
library(dendextend)
as.dendrogram(my_hclust_gene) %>%
  plot(horiz = TRUE)
my_gene_col <- cutree(tree = as.dendrogram(my_hclust_gene), k = 4)
my_sample_col <- data.frame(sample = rep(c("t14", "t15","t16","t17"), c(11,6,10,6)))
row.names(my_sample_col) <- colnames(data_norm)
my_sample_col
pheatmap(data_norm, 
         annotation_col = my_sample_col,cluster_cols = F, fontsize = 7)

#####fig4_same color########################
library(ggtree)
library(ggplot2)
library(ggtree)
library(RColorBrewer)
library(gridExtra)
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/Revision/Progress/')
##################
info_PG <- read.csv("PG_ref_verified_cdhit70_exp_cdhit90_aligned_tree_info_cleanupNew.csv")
colnames(info_PG)<-c("id","taxa",'type')
tree_PG<-read.tree('PG_newick_tree2_midpointTooted_cleaned.txt')
##################
info_CW <- read.csv("CellWallHydrolase_newick_info.csv")
colnames(info_CW)<-c("id","taxa",'type')
tree_CW<-read.tree('CellWallHydrolase_newick.txt')
##################
alltaxa<-union(info_PG$taxa,union(info_CW$taxa,info_ubiq$taxa))
length(alltaxa)
#Iwanthue; get color combo
colorpick<-c("#e9f900",
             "#ffc86c",
             "#8063d7",
             "#717171",
             "#c5bcac",
             "#9bdeff",
             "#4ab198",
             "#fab0c6",
             "#59a9dd",
             "#3ec1d8",
             "#cb522b",
             "#657ecd",
             "#d39130",
             "#46e640",
             "#d38fd5",
             "#527d38",
             "#a3cb86",
             "#ffa73c")
length(colorpick)
alltaxa.col<-colorpick
names(alltaxa.col)<-alltaxa
###################
fig4_PG<-ggtree(tree_PG) %<+% info_PG + 
  geom_tippoint(aes(color=taxa),size=2)+ 
  theme(legend.position="right", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  scale_color_manual(values = alltaxa.col)+
  guides(color=guide_legend(ncol=1))

fig4_CellWallHydrolase<-ggtree(tree_CW) %<+% info_CW + 
  geom_tippoint(aes(color=taxa),size=2)+ 
  theme(legend.position="right", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  scale_color_manual(values = alltaxa.col)+
  guides(color=guide_legend(ncol=1))

grid.arrange(fig4_PG, fig4_CellWallHydrolase, ncol=2)
