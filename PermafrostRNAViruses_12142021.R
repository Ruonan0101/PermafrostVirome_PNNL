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

####check node number and family
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

####check node number and family
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

####check node number and family
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
#d <- tibble(value   = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 12, 13),
#            `Set 1` = c(T, F, T, T, F, T, F, T, F,  F,  F),
#            `Set 2` = c(T, F, F, T, F, F, F, T, F,  F,  T),
#            `Set 3` = c(T, T, F, F, F, F, T, T, F,  F,  F),
#            `Set 4` = c(F, F, F, F, T, T, F, F, T,  T,  F))
#d

figure2 <- ggvenn(d,c('Permafrost','California_Grassland','Kansas_Grassland','Invertebrate'), fill_color = c("#00BFFF", "#694402",'#FFA500',"#F8766D"), 
                  stroke_size = 0.15, set_name_size = 3, text_size=2) + theme_void() + coord_fixed()
figure2



###fig2b:flow
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_2/ForPairing_PhylogeneticGroup_family_system_host_dir/')
library(ggalluvial)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(colorRamps)
d<-read.csv("Cluster_Family_Group_Sample_forR.csv",check.names=FALSE)
d
figure2b<-ggplot(data = d,aes(axis1=Cluster, axis2 = Ecosystem, axis3 = Group)) +
  scale_x_discrete(limits = c("Viral cluster","Ecosystem",'Phylogenetic group')) +
  geom_alluvium(aes(fill = Group), alpha=0.9) +
  geom_stratum(width=1/12, alpha=.8, color='#666666') +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 1) +
  theme_minimal()+
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Set3"))(28))+
  theme(panel.background = element_blank(), panel.grid = element_blank(), 
        axis.title.y = element_text(vjust=5),legend.position='right', legend.key.size = unit(3, 'mm'), 
        legend.text = element_text(size=8), legend.title = element_text(size=9))

figure2b


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

##fig3c:variancePartioning analysis
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_4/')
fertil.spe <- read.delim ('RNA_V_com_forVariancePartionaling.txt', row.names = 1)
fertil.env <- read.delim ('Euk_oxygen_forVariancePartioning.txt', row.names = 1)
rda.all <- rda (fertil.spe ~ Discoba+Fungi+Metamonada+Metazoa+Sar+Viridiplantae+Oxygen, data = fertil.env)

rda.Euk <- rda (fertil.spe ~ Discoba+Fungi+Metamonada+Metazoa+Sar+Viridiplantae, data = fertil.env)
rda.oxygen <- rda (fertil.spe ~ Oxygen, data = fertil.env)
RsquareAdj (rda.all)
abc <- RsquareAdj (rda.all)$adj.r.squared
RsquareAdj (rda.Euk)
ab <- RsquareAdj (rda.Euk)$adj.r.squared
RsquareAdj (rda.oxygen)
bc <- RsquareAdj (rda.oxygen)$adj.r.squared
b <- ab + bc - abc  
b
a <- ab - b   
a
c <- bc - b 
c

varp <- varpart (fertil.spe, ~ Discoba+Fungi+Metamonada+Metazoa+Sar+Viridiplantae, ~ Oxygen, data = fertil.env)
varp
plot (varp, digits = 2, Xnames = c('Host lineage', 'Oxygen'), bg = c('navy', 'tomato'))
rda.Euk.oxygen <- rda (fertil.spe ~ Discoba+Fungi+Metamonada+Metazoa+Sar+Viridiplantae + Condition (Oxygen), data = fertil.env)
rda.oxygen.Euk <- rda (fertil.spe ~ Oxygen + Condition (Discoba+Fungi+Metamonada+Metazoa+Sar+Viridiplantae), data = fertil.env)
anova (rda.all)
anova (rda.Euk)
anova (rda.oxygen)
anova (rda.Euk.oxygen)
anova (rda.oxygen.Euk)

###fig4a:protein heatmap
library(pheatmap)
setwd('/Users/wuru978/Desktop/Project/P4_Alaska_multiomics_dir/')
data<-read.delim("RNA_V_prot_annotated_abd_normalized.txt",header=T, row.names="gene")
pheatmap(data)
cal_z_score <- function(x){
  (log(x+1))
}
data_norm <- t(apply(data, 1, cal_z_score))
pheatmap(data_norm)
my_hclust_gene <- hclust(dist(data_norm), method = "complete")
my_hclust_gene
library(dendextend)
as.dendrogram(my_hclust_gene) %>%
  plot(horiz = TRUE)
my_gene_col <- cutree(tree = as.dendrogram(my_hclust_gene), k = 4)
my_gene_col <- data.frame(cluster = ifelse(test = my_gene_col == 1, yes = "cluster 1", no = "cluster 2"))

my_sample_col <- data.frame(sample = rep(c("t14", "t15","t16","t17"), c(11,6,10,6)))
row.names(my_sample_col) <- colnames(data_norm)
my_sample_col
pheatmap(data_norm, annotation_row = my_gene_col, annotation_col = my_sample_col)
pheatmap(data_norm,
         annotation_row = my_gene_col,
         annotation_col = my_sample_col,
         cutree_cols = 6,fontsize = 8, legend_labels = 'log of normalized 18s rRNA counts', cluster_rows = T, cluster_cols = F)

###fig4b:PG_tree
library(ggtree)
library(ggplot2)
setwd('/Users/wuru978/Desktop/2021_five_Manuscripts/PNNL_Manuscript_Revision/P4_PermafrostRNAVirome/08202021/analysis_new_dir/Figure_5/')
info <- read.csv("PG_cut3_info.csv")
colnames(info)<-c("id1","Clade")
tree<-read.tree('AMG_tree_dir/PG_cut3_newick.txt')

fig4b<-ggtree(tree, layout='daylight', branch.length='rate') %<+% info + 
  geom_tippoint(aes(color=Clade),size=2)+ 
  theme(legend.position="right", legend.text = element_text(size=10))+
  guides(color = guide_legend(override.aes = list(size = 2)))
fig4b
















