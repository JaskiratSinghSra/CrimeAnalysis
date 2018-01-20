df = read.csv("D:/Downloads/crimes-in-chicago/Chicago_Crimes_2012_to_2017.csv")
library(dplyr)
df_arrest <- df %>% filter(grepl("True", Arrest))
df_arrest_daily <- df_arrest %>%
                    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
                    group_by(Date) %>%
                    summarize(count = n()) %>%
                    arrange(Date)
library(ggplot2); library(scales); library(grid); library(RColorBrewer)
fte_theme <- function() {
      
# Generate the colors for the chart procedurally with RColorBrewer
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]
color.grid.major = palette[3]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]
      
# Begin construction of chart
theme_bw(base_size=9) +
        
# Set the entire chart region to a light gray color
theme(panel.background=element_rect(fill=color.background, color=color.background)) +
theme(plot.background=element_rect(fill=color.background, color=color.background)) +
theme(panel.border=element_rect(color=color.background)) +
      
# Format the grid
theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
theme(panel.grid.minor=element_blank()) +
theme(axis.ticks=element_blank()) +
      
# Format the legend, but hide by default
theme(legend.position="none") +
theme(legend.background = element_rect(fill=color.background)) +
theme(legend.text = element_text(size=7,color=color.axis.title)) +
      
# Set title and axis labels, and format these and tick marks
theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
      
# Plot margins
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

plot <- ggplot(df_arrest_daily, aes(x = Date, y = count)) +
    geom_line(color = "#F2CA27", size = 0.1) +
    geom_smooth(color = "#1A1A1A") +
    fte_theme() +
    scale_x_date(breaks = date_breaks("1 years"), labels = date_format("%Y")) +
    labs(x = "Date of Arrest", y = "# of Police Arrests", title = "Daily Police Arrests in Chicago from 2012 – 2017")

plot

library(ggmap)

bbox = c(-87.940102,41.643917,-87.523979,42.023026)
map <- get_map(location = bbox, source = "google", maptype = "terrain")

plot <- ggmap(map) +
            geom_point(data = df_arrest, aes(x=Longitude , y=Latitude  ), color = "red", size = 0.5, alpha = 0.01) +
            fte_theme() +
            labs(title = "Locations of Police Arrests Made in Chicago from 2012 – 2017")
plot

df_top_crimes <- df_arrest %>%
                    group_by(Primary.Type) %>%
                    summarize(count = n()) %>%
                    arrange(desc(count))

plot <- ggmap(map) +
            geom_point(data = df_arrest %>% filter(Primary.Type %in% df_top_crimes$Primary.Type[1:20]), aes(x=Longitude , y=Latitude  , color=Primary.Type ), size=0.75, alpha=0.05) +
            fte_theme() +
            labs(title = "Locations of Arrests in Chicago by Crime Type") +
            facet_wrap(~Primary.Type, nrow = 5)
plot