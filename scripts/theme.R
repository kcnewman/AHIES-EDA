# load packages
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gghighlight)
library(sysfonts)
library(showtextdb)
library(showtext)
library(glue)
library(scales)
library(kableExtra)
library(patchwork)
library(forcats)
library(tidyverse)
library(ggbump)
library(reshape2)
library(plotly)


# load the font
font_add("poppins", "./Font/Poppins-Bold.ttf")

# make sure ggplot recognizes the font 
# and set the font to high-res
showtext_auto()
showtext::showtext_opts(dpi = 300)

# set default colour for plots with multiple categories
options(ggplot2.discrete.colour = c("#210D69", "#DB2E76", "#586889", "#227C42"))
options(ggplot2.discrete.fill   = c("#210D69", "#DB2E76", "#586889", "#227C42"))

# set default colour for plots with a single category
update_geom_defaults("bar",   list(fill = "#27A0CC"))
update_geom_defaults("col",   list(fill = "#27A0CC"))

# update the font to show in geom_text()
update_geom_defaults("text",   list(family = "poppins", size = 4.5 ))
GSS_font <- "poppins"
# create a GGS theme based on the theme_gray()
gssthemes<-function(){
  theme_gray() %+replace%
    theme(
      text=element_text(family = "poppins",
                        colour="black",
                        size=10),
      plot.margin = margin(0.5,0.3, 0.3, 0.3, "cm"),
      aspect.ratio = 0.6,
      plot.title =element_textbox_simple(family="poppins", size=16,
                                          lineheight=1,
                                          margin=margin(b=10)),
       plot.title.position="plot",
      plot.caption=element_markdown(hjust=0, color="gray",
                                    lineheight=1.5,
                                    margin =margin(t=10)),
      plot.caption.position="plot",
      axis.title.y=element_text(color="black", angle=90, size = 10),
      axis.title.x=element_text(color="black",size = 10),
      axis.text.x=element_text(color="black", size = 10, vjust = 0, margin = margin(t = 5, r = 5, b = 0, l = 0, unit = "pt")),
      axis.text.y=element_text(color="black", size = 10, hjust = 1, margin = margin(t = 5, r = 5, b = 0, l = 0, unit = "pt")),
      legend.text=element_text(color="black",  size = 10),
      panel.grid.major.y=element_line(color="gray", size=0.25),
      panel.grid.major.x=element_blank(),
      panel.grid.minor=element_blank(),
      panel.background=element_rect(fill="white", color=NA),
      plot.background=element_rect(fill="white", color=NA),
      legend.background=element_rect(fill="white", color=NA),
      strip.background =element_rect(fill="white",  color=NA),
      legend.key = element_rect(fill = "white", color = NA), 
      strip.text = element_text( size = 20,  margin = margin(t = 5, r = 0, b = 10, l = 0, unit = "pt"))
    )
}

# this will make the labels of the bar chart a bit nicer, by ending above the highest data point
nicelimits <- function(x) {
  range(scales::extended_breaks(only.loose = TRUE)(x))
}


# Define color palette

statscolours_color_scheme  <- c("#382873", "#0168C8", "#00B050")

#locality
national_color <- "#27A0CC"
urban_color <- "#871A5B"
rural_color <- "#22D0B6"
urbanrural_color_scheme  <- c("#27A0CC", "#871A5B", "#22D0B6")

#Sex
male_color <- "#206095"
female_color <- "#F66068"
malefemale_color_scheme <- c("#27A0CC", "#206095", "#F66068")

#positive/negative
negative_color <- "#cc3333"
positive_color <- "#33cccc"

#pallete
neutral_color_scheme  <- c("#002060", "#0070C0", "#00B0F0", "#8EA9DB", "#9BC2E6", "#FFFFCC")
posneg_color_scheme  <- c("#38761D","#6AA84F","#93C47D","#F4CCCC","#E06666","#990000")
posneutralneg_color_scheme  <- c("#38761D","#6AA84F","#FFFFCC","#E06666","#990000")
population_color_scheme  <- c("#FFFFCC","#C7E9B4","#7FCDBB","#41B6C4","#2C7FB8")
incidence_color_scheme  <- c("#FECCCC","#FF9999","#FF6666","#FF3333","#CC0000","#990000")


#Economic sector colour
industry_color<-"#14607A"
agric_color<-"#07BB9E"
services_color<-"#F98B00"
economic_sectors<- c("#14607A","#07BB9E","#F98B00")

#food
food_colour<- "#3ECDB9"
nonfood_colour<-"#04BCFC"

# Region
Ahafo_color_code <- "#4B644B"
Ashanti_color_code <- "#7D96AF"
Bono_color_code <- "#EBA07E"
Bono_East_color_code <- "#09979B"
Central_color_code <- "#EA879C"
Eastern_color_code <- "#E5B9AD"
Greater_Accra_color_code <- "#CC9B7A"
Northern_color_code <- "#FDD835"
North_East_code <- "#0070C0"
Oti_color_code <- "#AE2B47"
Savannah_color_code <- "#F94240"
Upper_East_color_code <- "#903000"
Upper_West_color_code <- "#0F3B6C"
Volta_color_code <- "#59A77F"
Western_color_code <- "#FDAE6B"
Western_North_color_code <- "#B173A0"

saveplot <- function(plot, filename, width = 8, height = 6) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,  # Standard width
    height = height,  # Standard height
    dpi = 300,  # High resolution
    limitsize = FALSE  # Allow larger plots
  )
}
