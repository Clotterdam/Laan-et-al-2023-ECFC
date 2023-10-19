## Libraries ----
library("dplyr")
library("heatmaply")
library("cetcolor")
library("shiny")
library("dendextend")
library("ggplot2")
library("gplots")
library("RColorBrewer")
library("readxl")
library("tidyr")
library("knitr")
library("seriation")
library("pheatmap")



##Read datafile
qPCR_data <- read_excel("YOUR PATH HERE"),.name_repair = "universal")

#Transform the data frame into a long format so it can be read.
qPCR_data_long <- qPCR_data %>% pivot_longer(cols= c("YOUR GENES HERE"),
                            names_to="Target",
                          values_to="nFOLD")
#Remove all samples that have NA for nFOLD.
qPCR_data_long <- qPCR_data_long[!is.na(qPCR_data_long$nFOLD),]


#EXPERIMENTS, SAMPLES, GENES OR OTHER FACTORS CAN BE ADDED HERE. ADD ALL POSSIBLE VARIABLES HERE.
allExperiment <- c("EXPERIMENT NUMBERS HERE")
allSample <- c("SAMPLE NUMBER HERE")
allGene <- c("GENE CODE HERE")

#Filters can be added, or adjusted here to customize the dataset.
filterExperiment <- c(...)
filterSample <- c(...)
filterGene <- c(...)

#set up the heatmap 
heatmap_data <- qPCR_data_long %>% subset(experiment %in% filterExperiment & 
                          Sample %in% filterSample & 
                          Target %in% filterGene)

## Build Heatmap matrix
l.sample <- unique(dfu$Sample)
l.gene <- unique(dfu$Target)
df.heatmap <- data.frame(matrix(ncol=length(l.sample), nrow=length(l.gene)))
colnames(df.heatmap) <- l.sample
rownames(df.heatmap) <- l.gene
for (i in 1:nrow(heatmap_data)) {
  df.heatmap[heatmap_data$Target[i], heatmap_data$Sample[i]] <- log(heatmap_data$nFOLD[i])
}


# optional make subset of the data to get side bars. This is needed for the ColSideColors.
df.select <- heatmap_data %>% subset(Target == "...")
new_list <- df.select %>%
  group_by(Sample) %>%
  slice(1) %>%
  ungroup()

heatmaply(
  df.heatmap,
  colors = viridis(n = 250,  option = "magma"),
  #revC = TRUE, #optional, to reverse order of the dendogram
  #grid_gap = 1, #optional, to create gaps between values.
  #grid_color = "black",
  width = 20,
  seriate = "OLO",
  show_dendrogram = c(FALSE, TRUE),
  ColSideColors = new_list[,c("...", "...")], #only possible when defined in df.select
  )

 