# Trying Treemap graph for budget FY23
# install.packages("treemap")

rm(list = ls())

# setwd(getwd())

# create "figures" and "data" folders in the directory
# dir.create("figures")
# dir.create("data")

# list the files in the directory
list.files()

library(treemap)
library(treemapify)
library(tidyverse)
library(ggthemes)
library(glue)
library(ggtext)
library(showtext)

library(hrbrthemes)


#Treemap graph------------------------------------------------------------------

# FY23 budget data
# import data
data <- read.csv("data/fy23budget.csv")

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




# data for ggplot type treemap (didn't use it finally)
plotdata <- data %>% 
  mutate(lbl = recode(heads, "Revenue and Foreign Grants" = 1, 
                      "Financing" = 3, 
                      "Expenditure" = 2))

plotdata$lbl2 <- c(10, 12, 14, 18, 40, 44, 46, 48, 80, 82, 86)



#Treemap using ggplot-----------------------------------------------------------

ggplot(data, aes(area = values, 
                     #fill = subsubheads, 
                     label = values, 
                     subgroup = heads,
                     subgroup2 = subheads,
                     subgroup3 = subsubheads,
                     layout = "fixed",
                     start = "topleft"
                     )) +
  geom_treemap(aes(fill=heads, alpha = c(1, 0.8, 0.6, 0.4, 1, 0.8, 0.7, 0.6, 0.6, 1, 0.9))) +
  geom_treemap_subgroup3_border(color = "white", size = 1) +
  geom_treemap_subgroup2_border(color = "white", size = 3) +
  geom_treemap_subgroup_border(color = "white",
                               size = 5, alpha = 0.8) +
  geom_treemap_subgroup_text(place = "center", 
                             alpha = 0.6,
                             color = "white",
                             # size = 15,
                             grow = TRUE,
                             family = "sans",
                             fontface = "italic") + 
  geom_treemap_subgroup3_text(place = "center",
                              alpha = 0.8,
                              color = "yellow",
                              size = 12,
                              min.size = 2,
                              reflow = TRUE,
                              family = "mono",
                              #fontface = "italic",
                              ) +
  # geom_treemap_text()+
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")



# bar graph---------------------------------------------------------------------

# for checking the percentage calculation for barbarplot
data %>%
  mutate(type = if_else(heads == "Expenditure", 
                        "Expenditure", 
                        "Revenue, Grants and Financing")) %>% 
  group_by(type, subsubheads) %>%
  summarize(tot = sum(values)) %>% 
  mutate(pct = tot/sum(tot),
         lbl = scales::percent(pct)) %>% 
  summarize(perct = tot/sum(tot)*100) %>% 
  group_by(type) %>% 
  summarize(totpct = sum(perct))


# Initial graph

data %>%
  mutate(type = if_else(heads == "Expenditure", 
                        "Expenditure", 
                        "Revenue, Grants and Financing")) %>% 
  group_by(type, subsubheads) %>%
  summarize(tot = sum(values)) %>% 
  mutate(pct = round(tot/sum(tot), 3),
         lbl = scales::percent(pct)) %>% 
  #filter(pct > 0.01) %>% 
  mutate(lbl2 = paste(subsubheads, lbl, sep = " : ")) %>% 
  ggplot(aes(x = type,
             y = pct,
             fill = subsubheads))+
  geom_bar(stat = "identity", 
           position = "fill",
           width = 0.7,
           color = "white") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = scales::percent)+
  geom_text(aes(label = lbl2), 
            size = 3, 
            position = position_stack(vjust = 0.5),
            color = "black") +
  #scale_fill_brewer(palette = "Set2") +
  labs(x = NULL,
       y = NULL) +
  # coord_flip()+
  theme_gdocs()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, color = "gray"),
        panel.grid.major.x = element_blank()) 



# copy the previous code until I need it (103-114)
barplotdata <- data %>%
  mutate(type = if_else(heads == "Expenditure", 
                        "Expenditure", 
                        "Revenue, Grants and Financing")) %>% 
  group_by(type, subsubheads) %>%
  summarize(tot = sum(values)) %>% 
  mutate(pct = round(tot/sum(tot), 3),
         lbl = scales::percent(pct)) %>% 
  filter(pct > 0.01) %>% 
  mutate(lbl2 = paste(subsubheads, lbl, sep = " : "))

barplotdata %>% 
  ggplot(aes(x = type,
             y = pct,
             fill = factor(subsubheads,
                           levels = c("Other Dev. Exp.", 
                                      "Operating Expenditure",
                                      "ADP",
                                      "NBR Tax Revenue",
                                      "Non-NBR Tax Revenue",
                                      "Non-Tax Revenue",
                                      "Borrowing from Banking System (net)",
                                      "Non-Bank Borrowing (net)",
                                      "Foreign Borrowing (net)"
                                      ))))+
  geom_bar(stat = "identity", 
           position = "fill",
           width = 0.7,
           color = "white") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = scales::percent)+
  geom_text(aes(label = lbl2), 
            size = 3, 
            position = position_stack(vjust = 0.5),
            color = "black") +
  scale_fill_manual(values = c("#2171b5",
                                "#6baed6",
                                "#bdd7e7",
                                "#2171b5",
                                "#6baed6",
                                "#bdd7e7",
                                "#31a354",
                                "#a1d99b",
                                "#fdbb84")) +
  #scale_fill_brewer(palette = "Set2") +
  labs(x = NULL,
       y = NULL) +
  # coord_flip()+
  theme_gdocs()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, color = "gray"),
        panel.grid.major.x = element_blank()) 



# Full category bar graph (final)-----------------------------------------------
data %>%
  mutate(type = if_else(heads == "Expenditure", 
                        "Expenditure", 
                        "Revenue, Grants and Financing")) %>% 
  group_by(type, subsubheads) %>%
  summarize(tot = sum(values)) %>% 
  mutate(pct = round(tot/sum(tot), 3),
         lbl = scales::percent(pct)) %>% 
  #filter(pct > 0.01) %>% 
  mutate(lbl2 = recode(subsubheads, 
                       "ADP" = "ADP",
                       "Operating Expenditure" = "Operating\nExpenditure",
                       "Other Dev. Exp." = "Other Dev. Exp.",
                       "Other Exp." = "Other Exp.",
                       "Borrowing from Banking System (net)" = "Borrowing from\nBanking System",
                       "Foreign Borrowing (net)" = "Foreign\nBorrowing",
                       "Foreign Grants" = "Foreign Grants",
                       "NBR Tax Revenue" = "NBR Tax Revenue",
                       "Non-Bank Borrowing (net)" = "Non-Bank Borrowing",
                       "Non-NBR Tax Revenue" = "Non-NBR Tax Revenue",
                       "Non-Tax Revenue" = "Non-Tax Revenue")) %>% 
  mutate(lbl3 = paste(lbl2, lbl, sep = " : ")) %>% 
  ggplot(aes(x = type,
             y = pct,
             fill = factor(subsubheads,
                           levels = c("Other Dev. Exp.", 
                                      "ADP",
                                      "Other Exp.",
                                      "Operating Expenditure",
                                      "Foreign Grants",
                                      "NBR Tax Revenue",
                                      "Non-NBR Tax Revenue",
                                      "Non-Tax Revenue",
                                      "Borrowing from Banking System (net)",
                                      "Non-Bank Borrowing (net)",
                                      "Foreign Borrowing (net)"
                                      )))) +
  geom_bar(stat = "identity", 
           position = "fill",
           width = 0.7,
           color = "white") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = scales::percent,
                     expand = c(0.01, 0.01))+
  scale_x_discrete(labels = c("Expenditure",
                              "Revenue, Grants\nFinancing"),
                   expand = c(0.4, 0.04)
                   ) +
  geom_text(aes(label = lbl3), 
            size = 3, 
            position = position_stack(vjust = 0.5),
            angle = 270, 
            color = "black") +
  scale_fill_manual(values = c("#2171b5",
                               "#6baed6",
                               "#fdbb84",
                               "#a1d99b",
                               "red",
                               "#2171b5",
                               "#6baed6",
                               "#bdd7e7",
                               "#31a354",
                               "#a1d99b",
                               "#fdbb84"
                               )) +
  annotate("text", 
           x = 1.6, 
           y = 0.25, 
           label = "Domestic Borrowing : 21.6%", 
           color = "#31a354", 
           size = 3,
           vjust = 0.5) +
  annotate("text", 
           x = 1.6, 
           y = 0.7, 
           label = "Tax Revenue : 57.3%", 
           color = "#2171b5", 
           size = 3,
           vjust = 0.5) +
  annotate("text", 
           x = 1.4, 
           y = 0.80, 
           label = "Development Expenditure : 38.3%", 
           color = "#2171b5", 
           size = 3,
           vjust = 0.5) +
  #scale_fill_brewer(palette = "Set2") +
  labs(x = NULL,
       y = NULL) +
  coord_flip() +
  theme_minimal() +
  #theme_gdocs()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10, 
                                   color = "black",
                                   angle = 270,
                                   vjust = 1,
                                   hjust = 0.5,
                                   face = "bold"),
        axis.text.x = element_text(size = 8, color = "gray"),
        #panel.grid.major.x = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()
        ) 


# Full category bar graph (final_Bangla)-----------------------------------------------

font_add(family = "kalpurush_ansi", regular = "kalpurush ANSI.ttf")

showtext_auto()

data %>%
  mutate(type = if_else(heads == "Expenditure", 
                        "Expenditure", 
                        "Revenue, Grants and Financing")) %>% 
  group_by(type, subsubheads) %>%
  summarize(tot = sum(values)) %>% 
  mutate(pct = round(tot/sum(tot), 3),
         lbl = scales::percent(pct)) %>% 
  #filter(pct > 0.01) %>% 
  mutate(lbl2 = recode(subsubheads, 
                       "ADP" = "GwWwc",
                       "Operating Expenditure" = "cwiPvjb e¨q",
                       "Other Dev. Exp." = "Ab¨vb¨ Dbœqb e¨q",
                       "Other Exp." = "Ab¨vb¨ e¨q",
                       "Borrowing from Banking System (net)" = "e¨vsK FY",
                       "Foreign Borrowing (net)" = "ˆe‡`wkK FY",
                       "Foreign Grants" = "ˆe‡`wkK Aby`vb",
                       "NBR Tax Revenue" = "GbweAvi Ki",
                       "Non-Bank Borrowing (net)" = "e¨vsK ewnf©‚Z FY",
                       "Non-NBR Tax Revenue" = "GbweAvi ewnf©‚Z Ki",
                       "Non-Tax Revenue" = "Ki ewnf©‚Z Avq")) %>% 
  mutate(lbl3 = paste(lbl2, lbl, sep = " : ")) %>% 
  ggplot(aes(x = type,
             y = pct,
             fill = factor(subsubheads,
                           levels = c("Other Dev. Exp.", 
                                      "ADP",
                                      "Other Exp.",
                                      "Operating Expenditure",
                                      "Foreign Grants",
                                      "NBR Tax Revenue",
                                      "Non-NBR Tax Revenue",
                                      "Non-Tax Revenue",
                                      "Borrowing from Banking System (net)",
                                      "Non-Bank Borrowing (net)",
                                      "Foreign Borrowing (net)"
                           )))) +
  geom_bar(stat = "identity", 
           position = "fill",
           width = 0.7,
           color = "white") +
  scale_y_continuous(breaks = seq(0, 1, 0.25),
                     labels = scales::percent,
                     expand = c(0.01, 0.01))+
  scale_x_discrete(labels = c("e¨q",
                              "ivR¯^ Avq, Aby`vb\nI A_©vqb"),
                   expand = c(0.4, 0.04)
                   ) +
  geom_text(aes(label = lbl3), 
            size = 4, 
            position = position_stack(vjust = 0.5),
            angle = 270, 
            color = "black",
            family = "kalpurush_ansi") +
  scale_fill_manual(values = c("#2171b5",
                               "#6baed6",
                               "#fdbb84",
                               "#a1d99b",
                               "red",
                               "#2171b5",
                               "#6baed6",
                               "#bdd7e7",
                               "#31a354",
                               "#a1d99b",
                               "#fdbb84"
  )) +
  annotate("text", 
           x = 1.6, 
           y = 0.25, 
           label = "Af¨š—ixY FY : 21.6%", 
           color = "#31a354", 
           size = 5,
           vjust = 0.5,
           family = "kalpurush_ansi") +
  annotate("text", 
           x = 1.6, 
           y = 0.7, 
           label = "Ki ivR¯^ : 57.3%", 
           color = "#2171b5", 
           size = 5,
           vjust = 0.5,
           family = "kalpurush_ansi") +
  annotate("text", 
           x = 1.4, 
           y = 0.80, 
           label = "Dbœqb e¨q : 38.3%", 
           color = "#2171b5", 
           size = 5,
           vjust = 0.5,
           family = "kalpurush_ansi") +
  #scale_fill_brewer(palette = "Set2") +
  labs(x = NULL,
       y = NULL,
       title = "GKbR‡i A_©eQi 23 Gi ev‡RU",
       subtitle = "cÖ¯—vweZ ev‡RU 6,78,064 †KvwU UvKv‡K 100 a‡i",
       caption = "Drm t ev‡R‡Ui msw¶ßmvi A_©eQi 20 I A_©eQi 23, A_©gš¿Yvjq|") +
  coord_flip() +
  theme_minimal() +
  #theme_gdocs()+
  theme(text = element_text(family = "kalpurush_ansi"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(family = "kalpurush_ansi",
                                   size = 16),
        plot.caption = element_text(family = "kalpurush_ansi",
                                    color = "gray",
                                    size = 12,
                                    hjust = 0),
        legend.position = "none",
        axis.text.y = element_text(size = 12, 
                                   color = "black",
                                   angle = 270,
                                   vjust = 1,
                                   hjust = 0.5,
                                   face = "bold"),
        axis.text.x = element_text(size = 10, color = "gray"),
        #panel.grid.major.x = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()
  ) 



# Cleveland dot plot or dumble plot---------------------------------------------
# load data

devexp <- read.csv("data/devexp.csv")

devexpp <- devexp %>% 
  arrange(FY23b)  %>% 
  mutate(sector = factor(sector, sector))

ggplot(devexpp) +
  geom_segment(aes(x = sector, 
                   xend = sector, 
                   y = FY23b, 
                   yend = FY18
                   ), 
               color = "gray",
               size = 3,
               alpha = 0.7) +
   geom_point(aes(x = sector,
                  y = FY23b,
                  color = rgb(0.2,0.7,0.1,0.5),
                  size = 3)) +
   geom_point(aes(x = sector,
                  y = FY18,
                  color = rgb(0.7,0.2,0.1,0.5),
                  size = 3)) +
  coord_flip() +
  labs(x=NULL, 
       y=NULL)+
  theme_minimal()+
  theme(
    legend.position = "none")





# similar using pivot long data
#-----most final graph----------------------------------------------------------
# col_fy23 <- c("#062275")
# col_fy18 <- c("#567EF7")

# col_fy23 <- c("#00b4d8")
# col_fy18 <- c("#03045e")

col_fy23 <- c("#3dbbd1")
col_fy18 <- c("#1c2b73")


# data manupulation
pdata <- devexpp %>% 
  select(sector, FY23b, FY18) %>% 
  filter(sector != "Others") %>% 
  rename(percent_FY23b = FY23b,
         percent_FY18 = FY18) %>% 
  mutate(bump_FY23b = if_else(percent_FY23b < percent_FY18,
                              percent_FY23b - 1.5, 
                              percent_FY23b + 1.5)) %>% 
  mutate(bump_FY18 = if_else(percent_FY18 < percent_FY23b,
                             percent_FY18 - 1.5,
                             percent_FY18 + 1.5))
  
pdata <- pdata %>% 
  pivot_longer(cols = -sector, names_to = c(".value", "FY"), 
               names_sep = "_")


#plotting figure
#fig_devexp <- 
  ggplot(pdata, 
       aes(x = percent,
           y = sector,
           color = FY)) +
  geom_line(color = "darkgrey",
            size = 2.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = TRUE) + 
  geom_text(aes(label = glue("{percent}%"), x = bump), show.legend = FALSE) +
  scale_color_manual(name = NULL, 
                     breaks = c("FY23b", "FY18"),
                     values = c(col_fy18, col_fy23),
                     labels = c("Budget-FY23", "Actual-FY18"),
                     guide = guide_legend(override.aes = list(size = 4))) +
  scale_x_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     labels = glue("{seq(0, 30, 5)}%"),
                     expand = c(0.01, 0.01)) +
  scale_y_discrete(labels = c("Social Security\nand Welfare",
                              "Agriculture",
                              "Public Administration",
                              "Health",
                              "Energy and Power",
                              "Local Government and\nRural Development",
                              "Education and\nTechnology",
                              "Transport and\nCommunication"),
                   expand = c(0.02, 0.02)) +
  labs(x = NULL,
       y = NULL, 
       title = "Sectorwise Development Expenditure",
       # subtitle = "A comparison with FY18 budget",
       subtitle ="A comparison between budget <span style = 'color: #1c2b73;'>**FY23**</span> and <span style = 'color: #3dbbd1;'>**FY18**</span><br>",
       caption = "<i>Source: Budget in Brief FY23 and FY20, Ministry of Finance.</i>") +

  theme(
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "gray", 
                                    hjust = 0,
                                    margin = margin(t = 8)),
    plot.caption.position = "plot",
    #legend.position = "top",
    plot.title = element_text(face = "bold", 
                              size=15,
                              color="#000000", 
                              #family = "patua-one",
                              margin = margin(t = 5, b = 2)),
    plot.subtitle = element_markdown(face = "italic",
                                 size = 12,
                                 #color = "black",
                                 margin = margin(b = 15)),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    #axis.line.y = element_line(color = "darkgrey"),
    #axis.text.x = element_text(color = "darkgrey"),
    axis.text.x = element_blank(),
    #axis.text.x = element_text(color="#686868", size=6),
    axis.text.y = element_text(face="bold"),
    legend.position = c(0.3, 1.04),
    legend.margin = margin(l=0),
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.background = element_blank(),
    #legend.title = element_text(size=9, lineheight = 1.3, margin = margin(r=35)),
    legend.key = element_blank(),
    legend.key.width = unit(3, "pt"),
    legend.text = element_markdown(margin = margin(r=10), 
                                   face = "bold"),
    #legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.x = element_line(color = "lightgrey", 
                                      #size = 0.1),
    panel.grid.major.y = element_line(color = "lightgrey", 
                                      size = 0.1, 
                                      linetype = "dotted")
  )
                                
#fig_devexp

ggsave("figures/devexp.tiff", width=7, height=4)



# Bangla devexp dumble chart----------------------------------------------------

# load fonts to use
font_files() %>%  tibble() %>%  filter(str_detect(family, "Kalpurush")) %>% pull(file)

font_add(family = "kalpurush_ansi", regular = "kalpurush ANSI.ttf")

#tiff("test.tiff", 800, 500)

showtext_auto()

#fig_devexp_bangla <- 
ggplot(pdata, 
       aes(x = percent,
           y = sector,
           color = FY)) +
  geom_line(color = "darkgrey",
            size = 2.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = TRUE) + 
  geom_text(aes(label = glue("{percent}%"), x = bump), 
            show.legend = FALSE, 
            family = "kalpurush_ansi",
            size = 5) +
  scale_color_manual(name = NULL, 
                     breaks = c("FY23b", "FY18"),
                     values = c(col_fy18, col_fy23),
                     labels = c("A_©eQi 23", "A_©eQi 18"),
                     guide = guide_legend(override.aes = list(size = 4))) +
  scale_x_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     labels = glue("{seq(0, 30, 5)}%"),
                     expand = c(0.01, 0.01)) +
  scale_y_discrete(labels = c("mvgvwRK wbivcËv\nI Kj¨vY",
                              "K…wl",
                              "Rb cÖkvmb",
                              "¯^v¯’¨",
                              "R¡vjvwb I we`y¨r",
                              "¯’vbxq miKvi\nI cwj­ Dbœqb",
                              "wk¶v I cÖhyw³",
                              "cwienb I\n†hvMv‡hvM"),
                   expand = c(0.03, 0.03)) +
  labs(x = NULL,
       y = NULL, 
       title = "LvZwfwËK Dbœqb e¨q",
       # subtitle = "A comparison with FY18 budget",
       subtitle ="<span style = 'color: #1c2b73;'>**A_©eQi 23**</span> I <span style = 'color: #3dbbd1;'>**A_©eQi 18**</span> Gi Dbœqb e¨q eiv‡Ïi Zzjbv",
       caption = "<i>Drm t ev‡R‡Ui msw¶ßmvi A_©eQi 20 I A_©eQi 23, A_©gš¿Yvjq|</i>") +
  
  theme(
    text = element_text(family = "kalpurush_ansi", size = 16),
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "gray", 
                                    hjust = 0,
                                    margin = margin(t = 10)),
    plot.caption.position = "plot",
    #legend.position = "top",
    plot.title = element_text(face = "bold", 
                              size=20,
                              color="#000000", 
                              #family = "patua-one",
                              margin = margin(t = 5, b = 2)),
    plot.subtitle = element_markdown(face = "italic",
                                     size = 16,
                                     #color = "black",
                                     margin = margin(b = 15)),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    #axis.line.y = element_line(color = "darkgrey"),
    #axis.text.x = element_text(color = "darkgrey"),
    axis.text.x = element_blank(),
    #axis.text.x = element_text(color="#686868", size=6),
    axis.text.y = element_text(face="bold"),
    legend.position = c(0.45, 1.04),
    legend.margin = margin(l=0),
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.background = element_blank(),
    #legend.title = element_text(size=9, lineheight = 1.3, margin = margin(r=35)),
    legend.key = element_blank(),
    legend.key.width = unit(3, "pt"),
    legend.text = element_markdown(margin = margin(r=10), 
                                   face = "bold"),
    #legend.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.x = element_line(color = "lightgrey", 
    #size = 0.1),
    panel.grid.major.y = element_line(color = "lightgrey", 
                                      size = 0.1, 
                                      linetype = "dotted")
  )

#dev.off()
#fig_devexp

ggsave("figures/devexp_bangla.tiff", width=4, height=2.5)




# budget_exp_decade graphs------------------------------------------------------
decadedata <- read.csv("data/budgetsize_decades.csv")

names(decadedata)

decadedata %>% 
  rename(budget_exp = budget_exp..in.billion.) %>% 
  mutate(fy = factor(fy, fy)) %>% 
  #filter(budget_exp > 100) %>% 
  ggplot(aes(x = fy,
             y = budget_exp,
             size = budget_exp)) +
  geom_point(shape = 21, fill = "cornflowerblue", color = "black", alpha = 0.8) +
  scale_size(range = c(2, 42)) +
  geom_text(aes(label = round(budget_exp, 0)), size = 4, color = "red") +
  scale_y_continuous(limits = c(0, 7800),
                     breaks = seq(0,7500,1500)) +
  labs(x = NULL,
       y = NULL,
       title = "Budget expenditure comparison") +
  theme_minimal() +
  theme(legend.position = "none")
  


fy10_23data <- read.csv("data/budgetsize_fy10_23.csv")

names(fy10_23data)

fy10_23data %>% 
  rename(budget_exp = expenditure..billion.tk.) %>% 
  select(fy, budget_exp) %>% 
  mutate(fy = factor(fy, fy)) %>% 
  #filter(budget_exp > 100) %>% 
  ggplot(aes(x = fy,
             y = budget_exp,
             size = budget_exp)) +
  geom_point(color = "steelblue") +
  scale_y_continuous() 

# Just add a new comment to see