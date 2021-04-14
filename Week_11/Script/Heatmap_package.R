# heat maps

library(devtools)
install_github('zwdzwd/wheatmap')
library(wheatmap)
library(here)

mtcars_matrix <- as.matrix(mtcars)
head(mtcars_matrix)

mtcars_scale <- scale(mtcars_matrix)
head(mtcars_scale)

heatmap <-both.cluster(mtcars_scale)
heatmap$mat[,1:4]

BaseHeatmap<- WHeatmap(heatmap$mat, name="h1", 
                       yticklabels = TRUE, yticklabel.side = 'b', yticklabel.fontsize = 11,
                       xticklabels = TRUE, xticklabel.side = 'b', xticklabel.fontsize = 20,
                       cmp = CMPar(brewer.name = 'BuPu'))
BaseHeatmap

BaseHeatmap_1<- WHeatmap(heatmap$mat, name="h1", 
                       yticklabels = TRUE, yticklabel.side = 'b', yticklabel.fontsize = 11,
                       xticklabels = TRUE, xticklabel.side = 'a', xticklabel.fontsize = 20,
                       cmp = CMPar(brewer.name = 'Spectral'))
BaseHeatmap_1

Dendrogram <- BaseHeatmap +
  WDendrogram(heatmap$row.clust, 
              LeftOf('h1'), 
              facing = 'right')
Dendrogram

Dendrogram_1 <-BaseHeatmap +
  WDendrogram(heatmap$column.clust, 
              TopOf('h1'), 
              facing = 'bottom')
Dendrogram_1

Legend <- Dendrogram +
  WLegendV('h1', BottomLeftOf('h1', h.pad = -0.3), 'l1')
Legend

Highlight <- Legend +
  WRect('h1', c(2,5), c(2,3), col = 'yellow')
Highlight
