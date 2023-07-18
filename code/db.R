# ----- Load libraries ---- #
library(tidyverse)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(Cairo)
library(ggsci)
# ----- This section prepares and cleans our dataframe ---- #
db = read.csv("db.csv" , header = T)

# Build the plot

ggplot() +
  # reshape the data frame & get min value so you can draw an eye-tracking line (this is one geom)
  geom_segment(
    data = gather(db, measure, val, -commodity) %>%
      group_by(commodity) %>%
      top_n(-1) %>%
      slice(1) %>%
      ungroup(),
    aes(x = 0, xend = val, y = commodity, yend = commodity),
    linetype = "dotted", linewidth = 0, color = "white"
  ) +
  # reshape the data frame & get min/max category values so you can draw the segment (this is another geom)
  geom_segment(
    data = gather(db, measure, val, -commodity) %>% 
      group_by(commodity) %>% 
      summarise(start = range(val)[1], end = range(val)[2]) %>% 
      ungroup(),
    aes(x = start, xend = end, y = commodity, yend = commodity),
    color = "gray80", linewidth = 2
  ) +
  # reshape the data frame & plot the points
  geom_point(
    data = gather(db, measure, val, -commodity),
    aes(val, commodity, color = measure), 
    size = 4
  ) +
  #  Add labels to the left of each dumbbell
  geom_text(data = db, color= "black", size = 3.5, hjust = 2, aes(x= jul_2021, y = commodity, label= paste0('-N- ', jul_2021), family = "Tw Cen MT"))+
  #  Add labels to the mid of each dumbbell
  geom_text(data = db, color= "black", size = 3.5, vjust = -2, aes(x= jul_2022, y = commodity, label= paste0('-N- ', jul_2022), family = "Tw Cen MT"))+
  #  Add labels to the right of each dumbbell
  geom_text(data = db, color= "black", size = 3.5, hjust = -1.5, aes(x= jul_2023, y = commodity, label= paste0('-N- ', jul_2023), family = "Tw Cen MT"))+
  # Add limits to the x axis (this can help to center the plot)
  xlim(c(0,1450)) +
  # Add a color scheme
  scale_color_ft(name = "
                 Month - Year",
                    label = c('July - 2021', 'July - 2022', 'July - 2023'),
                    guide = guide_legend(title.hjust = 0.5, title.position = 'top', nrow = 1)
                 ) +
  #Add axis labels and caption
  labs(
    x = "
    
    Price in Naira", y = NULL,
    
    caption = "
    
    
    Data source : NBS | R {tidyverse} | @DOh_Bams "
  ) +
  
  # Add other theme customizations
  theme_classic() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12,  family = "Tw Cen MT"),
        legend.title = element_text(size = 14, family = "Tw Cen MT"),
        plot.title = element_text(size = 28, family = "Tw Cen MT", hjust = 0.5),
        plot.subtitle = element_text(size = 12, family = "Tw Cen MT", hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic", hjust = 0.5, vjust = 0,  family = "Tw Cen MT"),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = 'Tw Cen MT'),
        axis.title.x = element_text(size = 16, hjust = 0.5, vjust = 0,  family = "Tw Cen MT"),
        axis.text = element_text(size = 12, face = 'bold', hjust = 0.5, vjust = 0,  family = "Tw Cen MT"),
        plot.margin = margin(2,8,2,2,'cm'),
        axis.line = element_blank(),
        axis.ticks = element_blank()
        
        )


# save the plot

ggsave("db-chart2.png", type = 'cairo', width = 15, height = 12, dpi = 300)
