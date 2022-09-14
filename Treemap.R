# Trying Treemap graph
# install.packages("treemap")

rm(list = ls())

# create "figures" and "data" folders in the directory
# dir.create("figures")
# dir.create("data")

# list the files in the directory
list.files()



library(treemap)
library(treemapify)
library(ggplot2)

# FY23 budget data
# import data
data <- read.csv("fy23budget.csv")

treemap(data, index=c("heads", "subheads", "subsubheads"), vSize = "values", 
        
        type = "index",                      # How you color the treemap. Type help(treemap) for more info
        fontsize.labels = c(15,12,8),          # size of labels, size for group and subgroup
        fontcolor.labels = c("white", "yellow", "white"), # color of labels
        fontface.labels = c(2,1,3),            # font for labels: 1:4, for normal, bold, italic, bold-italic
        bg.labels = 0,                       # 0 means transparent
        align.labels = list(
          c("left", "top"),                  # horizontal, then vertical
          c("center", "center"),
          c("right", "bottom")),
        border.col = c("gray", "white", "white"),
        border.lwds = c(5,3,1),
        palette = "Set3",
        title = "Budget FY23",
        fontsize.title = 12
)

library(ggthemes)
library(dplyr)
plotdata <- data %>% 
  mutate(lbl = recode(heads, "Revenue and Foreign Grants" = 1, 
                "Financing" = 3, 
                "Expenditure" = 2))

plotdata$lbl2 <- c(10, 12, 14, 18, 40, 44, 46, 48, 80, 82, 86)

ggplot(plotdata, aes(area = values, 
                 fill = subsubheads, 
                 label = values, 
                 subgroup = heads,
                 subgroup2 = subheads,
                 subgroup3 = subsubheads)) +
  geom_treemap() +
  geom_treemap_subgroup3_border(color = "white", size = 1) +
  geom_treemap_subgroup2_border(color = "white", size = 2) +
  geom_treemap_subgroup_border(color = "white",
                               size = 5) +
  geom_treemap_subgroup_text(place = "topleft", 
                             alpha = 0.75,
                             color = "white",
                             # size = 15,
                             grow = TRUE,
                             family = "sans",
                             fontface = "bold") + 
  geom_treemap_subgroup3_text(place = "bottomleft",
                              alpha = 0.6,
                              color = "indianred",
                              size = 10,
                              family = "mono",
                              fontface = "italic")+
  # geom_treemap_text()+
  scale_fill_brewer(palette = "Set2")
  




# create data

group <- c(rep("group-1", 4), rep("group-2", 2), rep("group-3", 3))
subgroup <- paste("subgroup", c(1,2,3,4,1,2,1,2,3), sep = "-")
value <- c(13, 5, 22, 12, 11, 7, 3, 1, 23)
data <- data.frame(group, subgroup, value)

# custom labels: 
treemap(data, index=c("group", "subgroup"), vSize = "value", 
        
        type = "index",                      # How you color the treemap. Type help(treemap) for more info
        fontsize.labels = c(15,12),          # size of labels, size for group and subgroup
        fontcolor.labels = c("white", "orange"), # color of labels
        fontface.labels = c(2,1),            # font for labels: 1:4, for normal, bold, italic, bold-italic
        bg.labels = 0,                       # 0 means transparent
        align.labels = list(
          c("left", "top"),                  # horizontal, then vertical
          c("right", "bottom")),
        border.col = c("black", "white"),
        border.lwds = c(5,2),
        palette = "Set2",
        title = "My Treemap",
        fontsize.title = 12
        )



#example from treemap package


data(GNI2014)
treemap(GNI2014,
        index=c("continent", "iso3"),
        vSize="population",
        vColor="GNI",
        type="value",
        format.legend = list(scientific = FALSE, big.mark = " "))

data(business)

#########################################
### treemap types
#########################################

# index treemap: colors are determined by the index argument
## Not run: 
# large example which takes some time...
treemap(business, 
        index=c("NACE1", "NACE2", "NACE3"), 
        vSize="turnover", 
        type="index")

treemap(business[business$NACE1=="C - Manufacturing",],
        index=c("NACE2", "NACE3"),
        vSize=c("employees"),
        type="index")

treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        title.legend="number of NACE4 categories",
        type="value")

treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.prev",
        type="comp")

treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="turnover",
        vColor="employees/1000",
        type="dens")

treemap(business,
        index=c("NACE1", "NACE2", "NACE3"), 
        vSize="turnover",
        type="depth")
# categorical treemap: colors are determined by a categorical variable
business <- transform(business, data.available = factor(!is.na(turnover)), x = 1)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="x",
        vColor="data.available",
        type="categorical")

business$color <- rainbow(nlevels(business$NACE2))[business$NACE2]
treemap(business,
        index=c("NACE1", "NACE2"), 
        vSize="x",
        vColor="color",
        type="color")


business$color <- rainbow(nlevels(business$NACE2))[business$NACE2]
treemap(business,
        index=c("NACE1", "NACE2"), 
        vSize="turnover",
        vColor="employees",
        type="manual",
        palette=terrain.colors(10))


## Not run: 
# draw labels of first index at fontsize 12 at the center, 
# and labels of second index at fontsize 8 top left
treemap(business, 
        index=c("NACE1", "NACE2"), 
        vSize="employees", 
        fontsize.labels=c(12, 8), 
        align.labels=list(c("center", "center"), c("left", "top")),
        lowerbound.cex.labels=1)


# draw all labels at fontsize 12 (only if they fit)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        fontsize.labels=12,
        lowerbound.cex.labels=1)

# draw all labels at fontsize 12, and if they don't fit, reduce to a minimum of .6*12
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        fontsize.labels=12,
        lowerbound.cex.labels=.6)

# draw all labels at maximal fontsize
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        lowerbound.cex.labels=0,
        inflate.labels = TRUE)

# draw all labels at fixed fontsize, even if they don't fit
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        fontsize.labels=10,
        lowerbound.cex.labels=1,
        force.print.labels=TRUE)

#########################################
### graphical options: color palettes
#########################################

## for comp and value typed treemaps all diverging brewer palettes can be chosen
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.prev",
        type="comp",
        palette="RdBu")

## draw warm-colored index treemap
palette.HCL.options <- list(hue_start=270, hue_end=360+150)
treemap(business, 
        index=c("NACE1", "NACE2"),
        vSize="employees",
        type="index",
        palette.HCL.options=palette.HCL.options)

# terrain colors
business$employees.growth <- business$employees - business$employees.prev
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.growth",
        type="value",
        palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined legend range
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.growth",
        type="value",
        palette="-RdGy",
        range=c(-20000,30000))

# More control over the color palette can be achieved with mapping
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.growth",
        type="value",
        palette="RdYlGn",
        range=c(-20000,30000),           # this is shown in the legend
        mapping=c(-30000, 10000, 40000)) # Rd is mapped to -30k, Yl to 10k, and Gn to 40k

## End(Not run)


ggplot(data, aes(area = value, fill = group, group=group, subgroup=subgroup, label = value)) +
  geom_treemap() +
  geom_treemap_text() +
  geom_treemap_subgroup_border()
