# Basic R 

# Load packages
library(tidyverse)
library(glue)
library(ggtext)

# Cleveland dot plot or dumble plot --------------------------------------------

#[1] load data
devexp <- read.csv("data/devexp.csv")

#[7] re-ordering
devexp %>% 
  arrange(FY23b) %>% 
  mutate(sector = factor(sector, sector)) %>% 
#[2] start graphing
ggplot() +
  geom_segment(aes(x = sector,
                   xend = sector,
                   y = FY23b,
                   yend = FY18
                   ),
#[3] changing color and thickness
               color = "gray",
               size = 3,
               alpha = 0.7) +
#[4] adding points 
  geom_point(aes(x = sector,
                 y = FY23b,
                 color = rgb(0.2,0.7,0.1,0.5),
                 size = 3)) +
#[5] adding other points
  geom_point(aes(x = sector,
                 y = FY18,
                 color = rgb(0.7,0.2,0.1,0.5),
                 size = 3)) +
#[6] filliping and labs and theme
  coord_flip() +
  labs(x=NULL, 
       y=NULL)+
  theme_minimal()+
  theme(
    legend.position = "none")


# Full scale graph--------------------------------------------------------------

#[1] demo what we need
ggplot(devexp,
       aes(x = percent,
           y = sector,
           color = FY)) +
  geom_line() +
  geom_point()

#[2] Prepare the data as we wanted to be in the graph

devexp %>% 
  select(sector, FY23b, FY18) %>%
#[9] arranging by FY23 data
  arrange(FY23b)  %>% 
  mutate(sector = factor(sector, sector)) %>% 
  filter(sector != "Others") %>% # [10] filter out others
  
#[6] seperate co-ordinate for percent text
# create new pivot longer data
# comment out this section after #[5]
  rename(percent_FY23b = FY23b,
         percent_FY18 = FY18) %>% 
  mutate(txt_FY23b = if_else(percent_FY23b < percent_FY18,
                              percent_FY23b - 1.5, 
                              percent_FY23b + 1.5)) %>% 
  mutate(txt_FY18 = if_else(percent_FY18 < percent_FY23b,
                             percent_FY18 - 1.5,
                             percent_FY18 + 1.5)) %>%
# end of comment out
    
#[6] show the previous data and change the pivot longer data 
#comment out and in pivot longer data
#  pivot_longer(-sector,
#               names_to = "FY",
#               values_to = "percent") %>%  #[3] pipe to ggplot [additionally show the data]
#[7] new pivot longer code
  pivot_longer(cols = -sector, names_to = c(".value", "FY"), 
               names_sep = "_") %>% 

  ggplot(aes(x = percent,
             y = sector,
             color = FY)) + #[4] add geom_point and line
  geom_line(color = "darkgray") +
  geom_point() +
#[5] add percentages text  {comment out after #[7]}
#  geom_text(aes(label = percent)) 
  geom_text(aes(label = percent, x = txt)) #[8] change the geom_text code




#Final graph with all beautification -------------------------------------------
devexp %>% 
  select(sector, FY23b, FY18) %>%
  arrange(FY23b)  %>% 
  mutate(sector = factor(sector, sector)) %>% 
  filter(sector != "Others") %>%
  rename(percent_FY23b = FY23b,
         percent_FY18 = FY18) %>% 
  mutate(txt_FY23b = if_else(percent_FY23b < percent_FY18,
                             percent_FY23b - 1.5, 
                             percent_FY23b + 1.5)) %>% 
  mutate(txt_FY18 = if_else(percent_FY18 < percent_FY23b,
                            percent_FY18 - 1.5,
                            percent_FY18 + 1.5)) %>%
  
  pivot_longer(cols = -sector, names_to = c(".value", "FY"), 
               names_sep = "_") %>% 
  
  ggplot(aes(x = percent,
             y = sector,
             color = FY)) +
#[11] line size, and legend false
  geom_line(color = "darkgrey",
            size = 2.5, show.legend = FALSE) +  
#[12] point size and legend  
  geom_point(size = 3, show.legend = TRUE) + 
#[13] add percent sign with glue package show legend false  
  geom_text(aes(label = glue("{percent}%"), x = txt), show.legend = FALSE) +
#[14] manually chaning the color, breaks, legend label  
  scale_color_manual(name = NULL, 
                     breaks = c("FY23b", "FY18"),
                     values = c("#1c2b73", "#3dbbd1"),
                     labels = c("Budget-FY23", "Actual-FY18"),
                     guide = guide_legend(override.aes = list(size = 4))) +
#[15] manually chnage the x-axis grid line, label and percent sign  
  scale_x_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 5),
                     labels = glue("{seq(0, 30, 5)}%"),
                     expand = c(0.01, 0.01)) +
#[16] manually change the appearance of y-axis labels with line breaks  
  scale_y_discrete(labels = c("Social Security\nand Welfare",
                              "Agriculture",
                              "Public Administration",
                              "Health",
                              "Energy and Power",
                              "Local Government and\nRural Development",
                              "Education and\nTechnology",
                              "Transport and\nCommunication"),
                   expand = c(0.02, 0.02)) +
#[17] add all titles and caption [first show without coloring of FY23 and FY18]
# that is without element markdown  
# Do #17.A and #17.B after the change  
  labs(x = NULL,
       y = NULL, 
       title = "Sectorwise Development Expenditure",
       # subtitle = "A comparison with FY18 budget", 
       # Do normal subtitle first, then change markdwon in #17.B 
       subtitle ="A comparison between budget <span style = 'color: #1c2b73;'>**FY23**</span> and <span style = 'color: #3dbbd1;'>**FY18**</span><br>",
       caption = "<i>Source: Budget in Brief FY23 and FY20, Ministry of Finance.</i>") +
#[] changing the theme components  
  theme(
    plot.title.position = "plot",
    #[17.A] Do markdown with #17
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
    #[17.B] Do markdown with #17
    plot.subtitle = element_markdown(face = "italic",
                                     size = 12,
                                     #color = "black",
                                     margin = margin(b = 15)),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    #axis.line.y = element_line(color = "darkgrey"),
    axis.text.x = element_text(color = "darkgrey"),
    #axis.text.x = element_blank(),
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
    legend.text = element_markdown(margin = margin(r=10), face = "bold"),
    #legend.text = element_text(face = "bold"),
    #panel.grid.major.x = element_blank(),
    panel.grid.major.x = element_line(color = "lightgrey", size = 0.1),
    panel.grid.major.y = element_line(color = "lightgrey", 
                                      size = 0.1, 
                                      linetype = "dotted")
  )

#fig_devexp

ggsave("figures/devexp.tiff", width=7, height=4)

  

