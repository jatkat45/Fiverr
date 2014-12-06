###############################################
# Back to Back Plot
#

library(ggplot2)
library(gridExtra)
library(scales) # for axes
library(ggthemes) # background themes for ggplot
library(RColorBrewer)

source("~/GitHub/R Scripts/General_Scripts/Multiplot.R") # for multiplot() to create multiple plots on 1 page

setwd("~/Fiverr/Summary_Data/")

summary_by_gender <- read.csv("summary_by_gender.csv", header = TRUE, stringsAsFactors = FALSE)

##############################################################
# Back to Back Gender by District

## warning ok not using -count(-value)

color_palette_b<- c("#FFFFCC", "#C7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494") #, "#d9d9d9")
                    
g_female <- ggplot(summary_by_gender, aes(x = AGE_GROUP)) + 
  geom_bar(data = subset(summary_by_gender, SEXO == "Femenino"), 
           aes(y = count, fill = AGE_GROUP), stat = "identity") + # stacked bar chart
  #geom_hline(yintercept = 0, color = "red", size = 2) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_female, aes(x = AGE_GROUP, y = count, label = count), # adds total text
            size = 4, hjust = 1, vjust = 1) +
  #scale_fill_brewer(palette = "YlGnBu") + # Rcolorbrewer
  #scale_fill_manual(values = color_palette) +
  ylab("Femenino") + scale_y_reverse(labels = comma, expand = c(0,2500)) + # labels y axis, reverse moves scale to left side at 0,0
  #scale_x_discrete(expand = c(0,0)) +
  theme(text = element_text(size = 15, hjust = 0.5), # text size
        panel.background = element_blank(), # background panel
        panel.border = element_rect(fill = NA, size = 0.5, linetype = "dotted"), # adds border to panel
        panel.grid.major.x = element_line(color = "black", size = 0.25, linetype = "dotted"), # adds X major (vertical) lines
        panel.grid.major.y = element_line(color = "gray92", size = 0.5, linetype = "solid"), # adds Y major (horizontal)
        panel.grid.minor.y = element_line(color = "gray92", size = 0.1, linetype = "solid"),
        #panel.margin.y = unit(-2, "lines"), 
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.line.y = element_line(color = "red", size = 2.5),
        plot.margin = unit(c(0,0,0,0), "cm"), # changes margins of plot top, right, bottom, left
        legend.position = "none") +  # removes legend
  #coord_cartesian(ylim = c(0,150000))+      
  coord_flip() # flips chart to side

g_male <- ggplot(summary_by_gender, aes(x = AGE_GROUP)) + 
  geom_bar(data = subset(summary_by_gender, SEXO == "Masculino"),
           aes(y = count, fill = AGE_GROUP), stat = "identity") + 
  geom_hline(yintercept = 0, color = "red", size = 2) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_male, aes(x = AGE_GROUP, y = count, label = count), 
            size = 4, hjust = 0, vjust = 1) +
  #scale_fill_brewer(palette = "YlGnBu") +
  #scale_fill_manual(values = color_palette) +
  ylab("Masculino") + scale_y_continuous(labels = comma, expand = c(0,2500)) +
  #scale_x_discrete(expand = (0,0)) +
  theme(text = element_text(size = 15, hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "black", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line(color = "gray92", size = 0.5, linetype = "solid"),
        panel.grid.minor.y = element_line(color = "gray92", size = 0.1, linetype = "solid"),
        panel.margin.y = unit(0, "lines"),
        #axis.ticks.margin = unit(0, "lines"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_line(color = "red", size = 2.5),
        plot.margin = unit(c(0,0,0,-0.5), "cm"), # top, right, bottom, left
        legend.position = "right",
        axis.ticks.length = unit(0, "cm"),
        axis.ticks.y = element_blank()) + #coord_cartesian(xlim = c(0, 150000)) +
  coord_flip()



multiplot(g_female, g_male, cols = 2)

                    
                    
                    
                    
                   
                    
