# Loop for graphs

library(ggplot2)
library(gridExtra)
library(scales) # for axes
library(ggthemes) # background themes for ggplot
library(RColorBrewer)
# library(directlabels) # for easy label placement

theme_get() # information about the different opts() parameters

source("~/GitHub/R Scripts/General_Scripts/Multiplot.R") # for multiplot() to create multiple plots on 1 page
source("~/GitHub/R Scripts/General_Scripts/Proper.R") # to emulate Proper function in Excel

setwd("~/Fiverr/Summary_Data/District_age")

Distritoo <- read.csv("Distritoo.csv", header = TRUE, stringsAsFactors = FALSE)
names(Distritoo) <- c("X", "DISTRITOO")
Distritoo <- Distritoo$DISTRITOO
Distritoo[7] <- "BRENA" 
y_max <- max(summary_counts_gender_district$count) # to set same y-axis for all charts

for (factor in Distritoo) {
  
  
  ## Create plot
  cat("Creating plot for: ",factor,"\n") # create plot
  
  ## create tmp file
  tmp <- subset(districts_by_gender_by_age,DISTRITOO==factor) ## subset each market
  max_femenino <- length(which(tmp$SEXO == "Femenino"))
  max_masculino <- length(which(tmp$SEXO == "Masculino")) 
  
  png(paste(factor,".png",sep=""), width = 2200, height = 1080, units = 'px', res = 130) 
  
  ## Plot stacked barchart
  print(ggplot(tmp,aes(x = SEXO, fill = AGE_GROUP)) + ## Create plot
    geom_bar(stat = "bin") +  scale_fill_brewer(palette="YlGnBu") +  ## add type of chart
    xlab("SEXO")+ylab("AGE_GROUP")+labs(title = paste(factor," - By Age Group",sep=""))) + 
      
  dev.off()
}

tmp <- (subset(districts_by_gender_by_age, DISTRITOO == "ANCON"))

#####################################################
#
# Back to Back Charts 

ggplot(combine_summary, aes(x = DISTRITOO, y = count)) + geom_bar(stat = "identity")

## combines all gender data by District
ggplot(combine_summary, aes(x = DISTRITOO, y = SEXO)) + 
  geom_bar(aes(y = count, fill = AGE_GROUP), stat = "identity") + 
  scale_x_discrete(labels = abbreviate(x, minlength = 7)) +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 60, lineheight = .5))+
          coord_cartesian(ylim = c(0, 200000)) # sets y-axis at 0, eliminates space between labels and graph


png("Distritoo.png", width = 2200, height = 1080, units = 'px', res = 130) 

##############################################################
# Back to Back Gender by District

## warning ok not using -count(-value)

color_palette <- c()
color_palette_b<- c("#FFFFCC", "#C7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494") #, "#d9d9d9")
  

g1 <- ggplot(combine_summary, aes(x = DISTRITOO)) + 
  geom_bar(data = subset(combine_summary, SEXO == "Femenino", position = "stack"), 
           aes(y = count, fill = AGE_GROUP), stat = "identity") + # stacked bar chart
  #geom_hline(yintercept = 0, color = "red", size = 2) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_female_total, aes(x = DISTRITOO, y = total, label = total), # adds total text
            size = 4, hjust = 1, vjust = 1) +
  #scale_fill_brewer(palette = "YlGnBu") + # Rcolorbrewer
  #scale_fill_manual(values = color_palette) +
  ylab("Femenino") + scale_y_reverse(labels = comma, expand = c(0,500)) + # labels y axis, reverse moves scale to left side at 0,0
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

g2 <- ggplot(combine_summary, aes(x = DISTRITOO)) + 
  geom_bar(data = subset(combine_summary, SEXO == "Masculino"),
           aes(y = count, fill = AGE_GROUP), stat = "identity") + 
  geom_hline(yintercept = 0, color = "red", size = 2) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_male_total, aes(x = DISTRITOO, y = total, label = total), 
            size = 4, hjust = 0, vjust = 1) +
  #scale_fill_brewer(palette = "YlGnBu") +
  #scale_fill_manual(values = color_palette) +
  ylab("Masculino") + scale_y_continuous(labels = comma, expand = c(0,500)) +
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



multiplot(g1, g2, cols = 2)

#########################################
# Add in NAs to chart 
#

combine_summary_NA[617:618, 1] <- "BRENA"

labels <- c("18-20", 
            "21-25", "26-29", "30-35", 
            "36-45", "46-55", "55+", "No Data")

combine_summary_NA$AGE_GROUP <- factor(combine_summary_NA$AGE_GROUP, 
                                       levels = labels) # changes levels 

color_palette_NA <- c("#FFFFCC", "#C7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494", "#d9d9d9")


g1NA <- ggplot(combine_summary_NA, aes(x = DISTRITOO)) + 
  geom_bar(data = subset(combine_summary_NA, SEXO == "Femenino", position = "stack"), 
           aes(y = count, fill = AGE_GROUP), stat = "identity") + 
  geom_hline(yintercept = 0, color = "red", size = 2) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_total_NA_female, aes(x = DISTRITOO, y = total, label = total), 
            size = 4, hjust = 1, vjust = 1) +
  #scale_fill_brewer(palette ="YlGnBu") + 
  scale_fill_manual(values = color_palette_NA, na.value = "#d9d9d9") +
  ylab("Femenino") + scale_y_reverse(labels = comma, expand = c(0,0)) + 
  theme(text = element_text(size = 15, hjust = 0.5),
        panel.background = element_rect(fill = NA, color = "white"),
        panel.border = element_rect(fill = NA, size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "black", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line(color = "black", size = 0.25),
        axis.line.y = element_line(color = "red", size = 2.5),
        plot.margin = unit(c(0,0,0,0), "cm"), # top, right, bottom, left
        legend.position = "none") +       
  coord_flip()

g2NA <- ggplot(combine_summary_NA, aes(x = DISTRITOO)) + 
  geom_bar(data = subset(combine_summary_NA, SEXO == "Masculino"),
           aes(y = count, fill = AGE_GROUP), stat = "identity") + 
  geom_hline(yintercept = 0, color = "red", size = 2) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_total_NA_male, aes(x = DISTRITOO, y = total, label = total), 
            size = 4, hjust = 0, vjust = 1) +  
  #scale_fill_brewer(palette="YlGnBu") +
  scale_fill_manual(values = color_palette_NA, breaks = labels, labels = labels, na.value = "#d9d9d9") +
  ylab("Masculino") + scale_y_continuous(labels = comma, expand = c(0,0)) +
  theme(text = element_text(size = 15, hjust = 0.5),
        panel.background = element_rect(fill = NA, color = "white"),
        panel.border = element_rect(fill = NA, size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "black", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line(color = "black", size = 0.25),
        #panel.margin.y = unit(-2, "lines"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(color = "red", size = 2.5),
        plot.margin = unit(c(0,0,0,-0.6), "cm"), # top, right, bottom, left
        legend.position = "right") +
  coord_flip()

multiplot(g1NA, g2NA, cols = 2)


#print(g3)

#dev.off()

###########################################
# Sandbox
###########################################
# Moves the margins, adds the horizontal lines

g1 <- ggplot(combine_summary, aes(x = DISTRITOO)) + 
  geom_bar(data = subset(combine_summary_NA, SEXO == "Femenino"), # subsets for female
           aes(y = count, fill = AGE_GROUP), stat = "identity") + # fills stacked bar
  geom_hline(yintercept = 0, color = "red", size = 1) + # adds red vertical line at 0 (flipped)
  geom_text(data = summary_female_total, aes(x = DISTRITOO, y = count, label = count), size = 4, hjust = 1) +
  ylab("Femenino") + 
  scale_fill_brewer(palette="Greys") + # shades of gray
  scale_y_reverse(labels = comma) +  # labels x with numbers with commas
  theme(text = element_text(size = 12, hjust = 0.5),
        panel.background = element_rect(fill = NA, color = "white"),
        panel.border = element_rect(fill = NA, size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "black", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line(color = "black", size = 0.5),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,-1.5,0,0), "cm"), # top, right, bottom, left
        legend.position = "none") +
  coord_flip()

g2 <- ggplot(combine_summary_NA, aes(x = DISTRITOO)) + 
  geom_bar(data = subset(combine_summary_NA, SEXO == "Masculino"),
           aes(y = count, fill = AGE_GROUP), stat = "identity") + scale_fill_brewer(palette="Greys") +
  geom_text(data = summary_male_total, aes(x = DISTRITOO, y = count, label = count), size = 4, hjust = 0) +
  geom_hline(yintercept = 0, color = "red", size = 1.25) + # adds red vertical line at 0 (flipped)+
  ylab("Masculino") + scale_y_continuous(labels = comma) +
  theme(text = element_text(size = 12, hjust = 0.5),
        panel.background = element_rect(fill = NA, color = "white"),
        panel.border = element_rect(fill = NA, size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "black", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line(color = "black", size = 0.5),
        panel.margin.y = unit(-2, "lines"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0,0,-1.5), "cm"), # top, right, bottom, left
        legend.position = "right") +
  coord_flip()

multiplot(g1, g2, cols = 2)

