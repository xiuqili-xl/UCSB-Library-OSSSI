# Load library ----
library(tidyverse)
library(readxl)
library(here)
library(circlize)


# Import data sets ----
rm(list = ls())        # clean out environment first

inventory <- read_xlsx("data_raw/OS-Inventory-list-20260202.xlsx")
glimpse(inventory)



# Wrangle data | OS Domain ----
list_domain <- inventory

for (domain in c("Data", "Method", "Source", "Access", "Review", "Education", "Infastructure")) {
  list_domain[[domain]] <- grepl(domain, list_domain$Domain)
}

glimpse(list_domain)

pairs_domain <- list_domain %>%
  select(Category.1 = Category, Item.1 = Item, Viz.ID.1= Viz.ID,
         Data, Method, Source, Access, Review, Education, Infastructure) %>%
  pivot_longer(cols = -c("Category.1", "Item.1", "Viz.ID.1"), 
               names_to = "Item.2", values_to = "Select") %>%
  filter(Select == TRUE) %>%
  mutate(Item.2 = case_when(Item.2 == "Data" ~ "Open data",
                            Item.2 == "Method" ~ "Open methodology",
                            Item.2 == "Source" ~ "Open source",
                            Item.2 == "Access" ~ "Open access",
                            Item.2 == "Review" ~ "Open peer review",
                            Item.2 == "Education" ~ "Open educational resource",
                            Item.2 == "Infastructure" ~ "Open infastructure")) %>%
  left_join(inventory %>% select(Category, Item, Viz.ID),
            by = join_by(Item.2 == Item)) %>%
  select(Category.1, Item.1, Viz.ID.1, Category.2 = Category, Item.2, Viz.ID.2 = Viz.ID)

pairs_domain



# Wrangle data | Provider ----
list_provider <- inventory

for (provider in c("CSD", "DREAM", "R&E", "RDS", "SRC", "T&L", "Materials")) {
  list_provider[[provider]] <- grepl(provider, list_provider$Provider)
}

glimpse(list_provider)

pairs_provider <- list_provider %>%
  select(Category.1 = Category, Item.1 = Item, Viz.ID.1= Viz.ID,
         CSD, DREAM, `R&E`, RDS, SRC, `T&L`, Materials) %>%
  pivot_longer(cols = -c("Category.1", "Item.1", "Viz.ID.1"), 
               names_to = "Item.2", values_to = "Select") %>%
  filter(Select == TRUE) %>%
  mutate(Item.2 = case_when(Item.2 == "DREAM" ~ "DREAM Lab",
                            Item.2 == "Materials" ~ "Open Course Materials Program",
                            TRUE ~ Item.2)) %>%
  left_join(inventory %>% select(Category, Item, Viz.ID),
            by = join_by(Item.2 == Item)) %>%
  select(Category.1, Item.1, Viz.ID.1, Category.2 = Category, Item.2, Viz.ID.2 = Viz.ID)

pairs_provider



# Wrangle data | Service ----
list_service <- inventory

for (service in c("Carpentries", "Dryad", "DMPTool", "eScholarship", "LibGuide",
                  "ORCiD", "Protocols.io", "RCL", "Agreement")) {
  list_service[[service]] <- grepl(service, list_service$Services)
}

glimpse(list_service)

pairs_service <- list_service %>%
  select(Category.1 = Category, Item.1 = Item, Viz.ID.1= Viz.ID,
         Carpentries, Dryad, DMPTool, eScholarship, LibGuide, 
         ORCiD, Protocols.io, RCL, Agreement) %>%
  pivot_longer(cols = -c("Category.1", "Item.1", "Viz.ID.1"), 
               names_to = "Item.2", values_to = "Select") %>%
  filter(Select == TRUE) %>%
  mutate(Item.2 = case_when(Item.2 == "Carpentries" ~ "Carpentries program",
                            Item.2 == "Agreement" ~ "Transformative Agreements",
                            Item.2 == "LibGuide" ~ "LibGuides, OS-themed",
                            Item.2 == "RCL" ~ "Reproducible and Collaborative Lab",
                            TRUE ~ Item.2)) %>%
  left_join(inventory %>% select(Category, Item, Viz.ID),
            by = join_by(Item.2 == Item)) %>%
  select(Category.1, Item.1, Viz.ID.1, Category.2 = Category, Item.2, Viz.ID.2 = Viz.ID)

pairs_service



# Create pairs dataset ----
pairs <- rbind(pairs_domain, pairs_provider, pairs_service) %>%
  arrange(Viz.ID.1, Viz.ID.2)



# Cord diagram | Define categories + colors ---- 
xlim_df <- inventory %>%
  group_by(Category) %>%
  summarize(min = min(Viz.ID) - 1.6, 
            max = max(Viz.ID) + 1.6)

xlim_df$Category

sector_colors <- c("OS Domains" = "#003660", 
                   "Provider" = "#09847A", 
                   "Services & Programs" = "#6D7D33", 
                   "Instruction & Consultation" = "#C43424",
                   "Engagement" = "#FEBC11")


# status_palette <- c("green", "yellow", "orange")
status_palette <- c("#64B5F6", "#B58CD2", "#D0D2D3")
# status_palette <- c("#47C27C", "#E7B93E", "#A1A5B0")

list_coded <- inventory %>%
  mutate(Status.Color = case_when(Status == "Active" ~ status_palette[1],
                                  Status == "In development" ~ status_palette[2],
                                  Status == "On hold" ~ status_palette[3],
                                  TRUE ~ "white"),
         Category.Color = plyr::revalue(Category, sector_colors),
  )



# Cord diagram | Define function ----
chord_diagram <- function(highlight_id = 0){
  # initialize
  circos.clear()
  circos.par(canvas.xlim = c(-1.1, 1.1), canvas.ylim = c(-1.1, 1.1),
             gap.after = rep(0, times = length(unique(list_coded$Category))),
             start.degree = -13)
  
  circos.initialize(sectors = list_coded$Category, 
                    xlim = as.matrix(xlim_df %>% select(min, max)))
  
  # add items and label
  circos.labels(sectors = list_coded$Category, x = list_coded$Viz.ID, 
                labels = list_coded$Item, side = "outside",
                cex = 0.3, padding = 0.0, connection_height = mm_h(0.3))
  
  # annotate status
  circos.track(sectors = list_coded$Category, ylim = c(0, 1),
               track.height = 0.04, cell.padding = c(0, 0, 0, 0), bg.border = NA)
  
  circos.trackPoints(sectors = list_coded$Category, 
                     x = list_coded$Viz.ID, y = rep(0.5, times = nrow(list_coded)), 
                     col = list_coded$Status.Color, 
                     pch = 16, cex = 0.6)
  
  # annotate buckets
  circos.trackPlotRegion(ylim = c(0, 1), track.height = 0.06,
                         panel.fun = function(x, y) {
                           # obtain cell meta data
                           sector_name <- get.cell.meta.data("sector.index")
                           xlim_cell <- CELL_META$xlim
                           ylim_cell <- CELL_META$ylim
                           # draw color coding rectangles
                           circos.rect(xlim_cell[1] + 1.2, ylim_cell[1], xlim_cell[2]-1.2, ylim_cell[2],
                                       col = sector_colors[sector_name],
                                       border = NA)
                           # add text label
                           circos.text(mean(xlim_cell), 0.5, labels = sector_name,
                                       facing = "bending.inside", niceFacing = TRUE,
                                       cex = 0.25, col = "white", font = 2)
                         },
                         # turn off default grid lines
                         bg.border = NA, cell.padding = c(0.01, 0, 0, 0)
  )
  
  # add chords to show pairs
  for(i in 1:nrow(pairs)){
    circos.link(sector.index1 = pairs$Category.1[i],
                point1 = c(pairs$Viz.ID.1[i] - 0.26, pairs$Viz.ID.1[i] + 0.26), 
                sector.index2 = pairs$Category.2[i],
                point2 = c(pairs$Viz.ID.2[i] - 0.26, pairs$Viz.ID.2[i] + 0.26), 
                col = "#BFBFBF30", h.ratio = 0.5, w = 0.8)
  }
  
  # highlight one projection
  pairs_selected <- pairs %>%
    filter(Viz.ID.1 == highlight_id | Viz.ID.2 == highlight_id)
  
  if(highlight_id == 0) {
    print("No higlight")
  } else if (nrow(pairs_selected) == 0) {
    print(paste0("Highlighting Viz.ID = ", highlight_id, ", but no links detected"))
  } else {
    print(paste0("Highlighting Viz.ID = ", highlight_id, ", ", inventory$Item[highlight_id]))
    for(i in 1:nrow(pairs_selected)){
      circos.link(sector.index1 = pairs_selected$Category.1[i],
                  point1 = c(pairs_selected$Viz.ID.1[i] - 0.26, pairs_selected$Viz.ID.1[i] + 0.26), 
                  sector.index2 = pairs_selected$Category.2[i],
                  point2 = c(pairs_selected$Viz.ID.2[i] - 0.26, pairs_selected$Viz.ID.2[i] + 0.26), 
                  col = list_coded$Category.Color[highlight_id], 
                  h.ratio = 0.5, w = 0.8)
    }
  }
}


# no highlights
chord_diagram()
# highlight RDS (Viz.ID = 1)
chord_diagram(highlight_id = 11)
# highlight Protocols.io (Viz.ID = 28)
chord_diagram(highlight_id = 28)
# highlight Carpentry community (Viz.ID = 45)
chord_diagram(highlight_id = 45)


# Cord diagram | Export visual w/o highlights ---- 
chord_diagram()
legend(
  x = "topright",
  inset = c(0.12, 0.1),
  legend = c("Active", "In development", "On hold"),
  col = status_palette,
  pch = 16,
  pt.cex = 0.6,
  cex = 0.35,
  bty = "o",          # draw box
  box.lwd = 0.2,      # thin border
  box.col = "grey55", # border color
  title = "Status",
  title.font = 2      # bold title
)

dev.copy(png, "images/static-png/00-no-highlight.png", 
         width = 5, height = 5, units = "in", res = 600)
dev.off()



# Chord diagram | Run loop and export visuals ----
for(id in 1:nrow(list_coded)){
  # create visuals with highlight
  chord_diagram(highlight_id = id)
  
  # add legend
  legend(
    x = "topright",
    inset = c(0.12, 0.1),
    legend = c("Active", "In development", "On hold"),
    col = status_palette,
    pch = 16,
    pt.cex = 0.6,
    cex = 0.35,
    bty = "o",          # draw box
    box.lwd = 0.2,      # thin border
    box.col = "grey55", # border color
    title = "Status",
    title.font = 2      # bold title
  )
  
  # export
  if(id < 10) {
    dev.copy(png, paste0("images/static-png/0", id, "-", 
                         str_sub(list_coded$Item[id], start = 1, end = 20), ".png"), 
             width = 5, height = 5, units = "in", res = 600)
  } else {
    dev.copy(png, paste0("images/static-png/", id, "-", 
                         str_sub(list_coded$Item[id], start = 1, end = 20), ".png"), 
             width = 5, height = 5, units = "in", res = 600)
  }
  
  dev.off()
}



# Create gif from png ----
library(gifski)
png_files <- list.files("images/static-png/", 
                        pattern = ".*png$", full.names = TRUE)

png_files_selected <- png_files[c(0, 1:4, 6:7, 8:14, 15:31, 45:55, 32:44) + 1]

gifski(png_files_selected, 
       gif_file = "images/chord-diagram.gif", 
       width = 2400, height = 2400, delay = 0.8, loop = TRUE)



# Calculate label degrees ----
# start with the initial steps of chord_diagram function; double check they match
# initialize
circos.clear()
circos.par(canvas.xlim = c(-1.1, 1.1), canvas.ylim = c(-1.1, 1.1),
           gap.after = rep(0, times = length(unique(list_coded$Category))),
           start.degree = -13)

circos.initialize(sectors = list_coded$Category, 
                  xlim = as.matrix(xlim_df %>% select(min, max)))

# add items and label
circos.labels(sectors = list_coded$Category, x = list_coded$Viz.ID, 
              labels = list_coded$Item, side = "outside",
              cex = 0.3, padding = 0.0, connection_height = mm_h(0.3))

# calculate degrees of tick marks
label_angles <- mapply(function(sec, x)
  circlize::circlize(x, y = 0, sector.index = sec, track.index = 1)[, "theta"],
  sec = list_coded$Category,
  x   = list_coded$Viz.ID
)

list_coded$label_degree <- label_angles

# calculate starting degree and ending degree
list_angles <- list_coded %>%
  select(Category, Item, Viz.ID, Label.Degree = label_degree) %>%
  group_by(Category) %>%
  mutate(Diff = Label.Degree - lag(Label.Degree)) %>%
  ungroup() %>%
  mutate(Diff = if_else(is.na(Diff), lead(Diff), Diff),
         Start.Degree = Label.Degree + 1/2 * Diff,
         End.Degree = Label.Degree - 1/2 * Diff,
         Start.Degree = round(Start.Degree, digits = 4),
         End.Degree = round(End.Degree, digits = 4))

list_angles

# export as csv
list_angles %>%
  select(Category, Item, Viz.ID, Start.Degree, End.Degree) %>%
  write_csv(here("data_processed", "label_angles.csv"))

