# Network analysis for HOVER study
# Background:
# This is a cross-sectional study of household hepatitis B transmission. Households were recruited from women presenting for antenatal care at a few maternities in the city.
# These women become "index mothers" for households and serve as egos for the network analysis
# If women agreed to participate, a single household visit was conducting, in which a survey was administered to all household members (alters) and a HBV (HBsAg) rapid test was done.
# Exposure: We use the index mother's antenatal HBV screening result as the exposure variable: "exposed" households = households of women who were HBV+ in antenatal care; "unexposed" households = households of women who were HBV- in antenatal care.
# Outcome: HBV (using HBsAg) result of each household member
# The purpose of the study is to look at demographic and behavioral factors associated with HBV infection to inform HBV prevention in this setting
# Because this is a household study, we are interested in a network approach to describe and visualize household structures
# Examples of what I have in mind for describing households: in Table 1, I have median hh size, median number of direct offspring of index mothers, number of households with male partners of index mother's enrolled, but are there other statistics a network analysis could provide to describe these households better?
# Examples of visualization: a directed, valued, multiplex graph - directed for direction of transmission, in this case starting with transmission from index mother's to others; valued: type of HBV transmission (vertical, sexual, other); multiplex: coloring or changing node shape HBV outcome status, female/male, or other HBV behavioral risk factors (the latter can come later)

#A. Load packages ----------------------------
# For a tidy framework
library(tidyverse)
library(glue)
library(scales)

# Our graphing libraries
library(igraph)
library(tidygraph)
library(ggraph)
## Loading packages via pacman and installing if needed

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(fs,
               tidyverse,
               igraph,
               sf,
               tmap,
               ggraph,
               sfnetworks,
               tidygraph)

#B. By exposure --------------------------------
# start with 200
## 1. Read in the hovernet200 data ----
hovernet200 <- inddata1 %>% select("hrhhid","pid","h10_hbv_rdt","hr3_relationship","hr4_sex","totalpositive","i27a_rdt_result","cpn_maternity","tab3hbv") #"i27a_rdt_result_f","age_combined", "hr4_sex_f","hdov",
# from step below that subsets these, but want to keep the same code
hovernet <- hovernet200 
# value for type of relationship
hovernet <- hovernet %>%
  mutate(
    value = case_when(
      hr3_relationship == 1 ~ 1, # index mothers
      hr3_relationship == 2 ~ 2, # male partners (sexual)
      hr3_relationship == 3 ~ 3, # offspring (vertical)
      hr3_relationship == 6 & hr4_sex == 1 ~ 3, # mother's mother (vertical)
      TRUE ~ 4
    ) %>% as.numeric()
  )
table(hovernet$value)

indexmoIDs <- hovernet %>% filter(hr3_relationship==1) %>% select(hrhhid, pid) %>% rename(sourceid = pid)

hovernet <- left_join(hovernet, indexmoIDs, by = "hrhhid")
# relocate the sourceID (index mothers's ID) and participat ID to front of dataframe
hovernet <- hovernet %>% relocate(sourceid, pid) 

#subset for edge list
hover_edge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship,cpn_maternity) %>% filter(hr3_relationship != 1) %>% rename(mat = cpn_maternity)
# add edges going from male partner to index mother to create two-way arrows
maleedge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship,cpn_maternity) %>% filter(hr3_relationship == 2) %>% rename(pidman = pid, pidindexmo = sourceid, mat = cpn_maternity)  %>% rename(pid = pidindexmo, sourceid = pidman) %>% relocate(sourceid, pid)
hover_edge <- rbind(hover_edge, maleedge)

# Create a dataframe of node attributes (hovernet_attr) using pid as the node ID
hovernet_attr <- hovernet %>%
  select(pid, hr4_sex, value,hr3_relationship, tab3hbv,i27a_rdt_result, totalpositive, cpn_maternity) %>%
  distinct()
hovernet_attr <- hovernet_attr %>% mutate(hbvnodecross = case_when(
  hr3_relationship==1 & tab3hbv==1 ~ 1, # index mother pos at cpn (exposed)
  hr3_relationship==1 & tab3hbv==0 ~ 2, # index mother neg at cpn (unexposed)
  hr3_relationship!=1 & tab3hbv==1 ~ 3, # alter pos 
  hr3_relationship!=1 & tab3hbv==0 ~ 4, # alter neg
))
table(hovernet_attr$hbvnodecross, useNA = "always")

## 2. Exposed subset------------
### 5.1 Subset-------------------------------
sub_exphh_edge <- hover_edge %>% filter(h10_hbv_rdt == 1)
hvr_ed_gsimp <- graph_from_data_frame(sub_exphh_edge, directed = TRUE)

### 5.2 Formatting-------------------------------
# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$maternity <-
  hovernet_attr$cpn_maternity[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                       ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#####use for edge list WITHOUT maternity edges
# node_colors <- c("#B2182B","black", "#f4a582","gray70")
# V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

#edge_colors <- c("gray70","#1f4a76", "#65b5c0","gray70")

#edge_colors <- c("gray70","#1f4a76", "#1F3B4D","gray70")
edge_colors <- c("gray70","#479cf8", "#1F3B4D","gray70")
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]
E(hvr_ed_gsimp)$width = E(hvr_ed_gsimp)$value

# alternatively could show transmission route by edge weight
# E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout_with_kk(hvr_ed_gsimp)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", quality=100)#, width=600, height=350)
# subset by structure

### 5.3 Plot -------------------------------
setEPS()
postscript("./plots/network_exp.eps")# quality=100)#, width=600, height=350)
plot(
  hvr_ed_gsimp,
  vertex.size = 3,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.05,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "Exposed households",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto
dev.off()

## 3. Unexposed subset------------
### 5.1 Subset-------------------------------
sub_unexphh_edge <- hover_edge %>% filter(h10_hbv_rdt == 0)
hvr_ed_gsimp <- graph_from_data_frame(sub_unexphh_edge, directed = TRUE)

### 5.2 Formatting-------------------------------
# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$maternity <-
  hovernet_attr$cpn_maternity[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                       ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#####use for edge list WITHOUT maternity edges
# node_colors <- c("#B2182B","black", "#f4a582","gray70")
# V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

#edge_colors <- c("gray70","#1f4a76", "#65b5c0","gray70")

#edge_colors <- c("gray70","#1f4a76", "#1F3B4D","gray70")
edge_colors <- c("gray70","#479cf8", "#1F3B4D","gray70")
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]
E(hvr_ed_gsimp)$width = E(hvr_ed_gsimp)$value

# alternatively could show transmission route by edge weight
# E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout_with_kk(hvr_ed_gsimp)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", quality=100)#, width=600, height=350)
# subset by structure

### 5.3 Plot -------------------------------
setEPS()
postscript("./plots/network_unexp.eps")# quality=100)#, width=600, height=350)
plot(
  hvr_ed_gsimp,
  vertex.size = 3,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.05,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "Unexposed households",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto
dev.off()







#C. By maternity --------------------------------
# start with 200
## 1. Read in the hovernet200 data ----
hovernet200 <- inddata1 %>% select("hrhhid","pid","h10_hbv_rdt","hr3_relationship","hr4_sex","totalpositive","i27a_rdt_result","cpn_maternity","tab3hbv") #"i27a_rdt_result_f","age_combined", "hr4_sex_f","hdov",
# tab3hbv: use CPN result for mothers, enrollment result for everyone else
# inddata1$tab3hbv <- ifelse(inddata1$hhmemcat==2, inddata1$h10_hbv_rdt, inddata1$i27a_rdt_result) # hhmemcat==2 are index moms

# hovernet200 <- readRDS("hovernet200.RDS")

## 2. New variables ----
# Subset to households with more than one HBV+ individual
hovernet <- hovernet200 

# Create a value variable for the edges (1 for ego, 2 for sexual relationship, 3 for vertical transmission, 4 for other relationship)
hovernet <- hovernet %>%
  mutate(
    value = case_when(
      hr3_relationship == 1 ~ 1, # index mothers
      hr3_relationship == 2 ~ 2, # male partners (sexual)
      hr3_relationship == 3 ~ 3, # offspring (vertical)
      hr3_relationship == 6 & hr4_sex == 1 ~ 3, # mother's mother (vertical)
      TRUE ~ 4
    ) %>% as.numeric()
  )
table(hovernet$value)


## 3. Edge list ----
# Create the igraph object (hvr_ed_gsimp) from the edge list (hover_edge)
# make mother's PID the source ID instead of the hh id (so that we get mother's attributes)
indexmoIDs <- hovernet %>% filter(hr3_relationship==1) %>% select(hrhhid, pid) %>% rename(sourceid = pid)

hovernet <- left_join(hovernet, indexmoIDs, by = "hrhhid")
# relocate the sourceID (index mothers's ID) and participat ID to front of dataframe
hovernet <- hovernet %>% relocate(sourceid, pid) 

#subset for edge list
hover_edge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship,cpn_maternity) %>% filter(hr3_relationship != 1) %>% rename(mat = cpn_maternity)
# add edges going from male partner to index mother to create two-way arrows
maleedge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship,cpn_maternity) %>% filter(hr3_relationship == 2) %>% rename(pidman = pid, pidindexmo = sourceid, mat = cpn_maternity)  %>% rename(pid = pidindexmo, sourceid = pidman) %>% relocate(sourceid, pid)
hover_edge <- rbind(hover_edge, maleedge)
# add edges from maternities to women
matnodes <- hovernet %>% filter(hr3_relationship == 1) %>% select(pid, value, h10_hbv_rdt,cpn_maternity) %>% rename(sourceid = pid, pid = cpn_maternity) %>% relocate(sourceid, pid)
matnodes$hr3_relationship <- 0 # give maternity edges a different value for relationship
matnodes$mat <- matnodes$pid # creating a column for maternity to subset plots
hover_edge <- rbind(hover_edge, matnodes)
table(matnodes$mat)

## 4. Attribute list--------------
# Create a dataframe of node attributes (hovernet_attr) using pid as the node ID
hovernet_attr <- hovernet %>%
  select(pid, hr4_sex, value,hr3_relationship, tab3hbv,i27a_rdt_result, totalpositive, cpn_maternity) %>%
  distinct()
hovernet_attr <- hovernet_attr %>% mutate(hbvnodecross = case_when(
  hr3_relationship==1 & tab3hbv==1 ~ 1, # index mother pos at cpn (exposed)
  hr3_relationship==1 & tab3hbv==0 ~ 2, # index mother neg at cpn (unexposed)
  hr3_relationship!=1 & tab3hbv==1 ~ 3, # alter pos 
  hr3_relationship!=1 & tab3hbv==0 ~ 4, # alter neg
))
table(hovernet_attr$hbvnodecross, useNA = "always")
# add maternities to list of attributes

mat_dist <- matnodes %>% group_by(pid) %>% select(pid) %>% distinct(pid)
mat_dist$hr4_sex <- NA # use different value for shape in igraph below
mat_dist$value <- 0 # use different value for shape in igraph below
mat_dist$hr3_relationship <- 0 # use different value for shape in igraph below
mat_dist$tab3hbv <- NA
mat_dist$i27a_rdt_result <- NA
mat_dist$totalpositive <- NA
mat_dist$cpn_maternity <- mat_dist$pid
mat_dist$hbvnodecross <- 0

ncol(hovernet_attr)
ncol(mat_dist)
hovernet_attr <- rbind(hovernet_attr, mat_dist)

view(hover_edge)

## 5. Binza igraph object-------------------------------
# need to subset by structure and then apply formatting
# the formatting wasn't coming through correctly with this approach
# sub_binza <-
#  names(V(hvr_ed_gsimp))[V(hvr_ed_gsimp)$maternity == "Binza"] # "Kingasani",  V(hvr_ed_gsimp)$maternity != "Kingasani" & V(hvr_ed_gsimp)$maternity != "Binza"
# binza_subgraph <-
#  subgraph(hvr_ed_gsimp, v = sub_binza)

### 5.1 Subset-------------------------------
sub_binza_2 <- hover_edge %>% filter(mat == "Binza")
hvr_ed_gsimp <- graph_from_data_frame(sub_binza_2, directed = TRUE)

### 5.2 Formatting-------------------------------
# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$maternity <-
  hovernet_attr$cpn_maternity[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

##  Plotting simple network, colored by positivity, shapes for sex, linetype for exposure
# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                         ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                                ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#####use for edge list WITHOUT maternity edges
# node_colors <- c("#B2182B","black", "#f4a582","gray70")
# V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

#edge_colors <- c("gray70","#1f4a76", "#65b5c0","gray70")

#edge_colors <- c("gray70","#1f4a76", "#1F3B4D","gray70")
edge_colors <- c("gray70","#479cf8", "#1F3B4D","gray70")
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]
E(hvr_ed_gsimp)$width = E(hvr_ed_gsimp)$value

# alternatively could show transmission route by edge weight
# E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout_with_kk(hvr_ed_gsimp)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", quality=100)#, width=600, height=350)
# subset by structure

### 5.3 Plot -------------------------------
setEPS()
postscript("./plots/network_binza.eps")# quality=100)#, width=600, height=350)
plot(
  hvr_ed_gsimp,
  vertex.size = 4,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.1,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "HOVER household networks recruited from Binza",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto
dev.off()

#legends
legend("topright",
  title = "Transmission mode",
  legend = c("Sexual", "Vertical", "Other"),
  col = c("#3b7e88", "#1F3B4D","gray70"),
  lty = 1, # line type 1-solid
  cex = 0.3, # relative size
  text.font = 10
 # lwd = 1.7,
  #box.lwd = 1,
  #x = 1.1,
  #y = 1
)

# vertices
legend(title = "Nodes",
  legend = c(
    "Index mother, HBsAg-",
    "Index mother, HBsAg+",
    "Male, HBsAg-",
    "Male, HBsAg+",
    "Female, HBsAg-",
    "Female, HBsAg+",
    "Maternity"
  ),
  pt.bg = c("black", "#B2182B", "gray70", "#f4a582", "gray70", "#f4a582", "#65b5c0"), # or #1f4a76 for maternities
  pch = c(21, 21, 22, 22, 21,21, 25),
  pt.cex = 1,
  cex = 0.7,
  box.lwd = 0,
  x = 1.1,
  y = 0.5)
##6. Kingasani-----
### 6.1 Subset-------------------------------
sub_king_2 <- hover_edge %>% filter(mat == "Kingasani")
hvr_ed_gsimp <- graph_from_data_frame(sub_king_2, directed = TRUE)

### 6.2 Formatting-------------------------------
# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$maternity <-
  hovernet_attr$cpn_maternity[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

##  Plotting simple network, colored by positivity, shapes for sex, linetype for exposure
# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                       ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#####use for edge list WITHOUT maternity edges
# node_colors <- c("#B2182B","black", "#f4a582","gray70")
# V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

#edge_colors <- c("gray70","#1f4a76", "#65b5c0","gray70")

#edge_colors <- c("gray70","#1f4a76", "#1F3B4D","gray70")
edge_colors <- c("gray70","#479cf8", "#1F3B4D","gray70")
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]

# alternatively could show transmission route by edge weight
# E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout_with_kk(hvr_ed_gsimp)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", quality=100)#, width=600, height=350)
# subset by structure

### 6.3 Plot -------------------------------
setEPS()
postscript("./plots/network_kinga.eps")# quality=100)#, width=600, height=350)
plot(
  hvr_ed_gsimp,
  vertex.size = 4,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.1,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "HOVER household networks recruited from Kingasani",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto
dev.off()

##7. Other structures-----
### 7.1 Subset-------------------------------
sub_oth_2 <- hover_edge %>% filter(mat != "Kingasani" & mat != "Binza")
hvr_ed_gsimp <- graph_from_data_frame(sub_oth_2, directed = TRUE)

### 7.2 Formatting-------------------------------
# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$maternity <-
  hovernet_attr$cpn_maternity[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

##  Plotting simple network, colored by positivity, shapes for sex, linetype for exposure
# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                       ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#####use for edge list WITHOUT maternity edges
# node_colors <- c("#B2182B","black", "#f4a582","gray70")
# V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

#edge_colors <- c("gray70","#1f4a76", "#65b5c0","gray70")

#edge_colors <- c("gray70","#1f4a76", "#1F3B4D","gray70")
edge_colors <- c("gray70","#479cf8", "#1F3B4D","gray70")
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]
E(hvr_ed_gsimp)$width = E(hvr_ed_gsimp)$value

# alternatively could show transmission route by edge weight
# E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout_with_kk(hvr_ed_gsimp)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", quality=100)#, width=600, height=350)
# subset by structure

### 7.3 Plot-------------------------------
setEPS()
postscript("./plots/network_oth.eps")# quality=100)#, width=600, height=350)
plot(
  hvr_ed_gsimp,
  vertex.size = 4,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.1,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "HOVER household networks recruited from Other structures",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto
dev.off()

##8. Clusters (n=14)-----------------------------------------------------------
### 8.1 Data processing ----
# Subset to households with more than one HBV+ individual
hovernet <- hovernet200 %>% filter(totalpositive > 1)
  
# Create a value variable for the edges (1 for ego, 2 for sexual relationship, 3 for vertical transmission, 4 for other relationship)
hovernet <- hovernet %>%
  mutate(
    value = case_when(
      hr3_relationship == 1 ~ 1, # index mothers
      hr3_relationship == 2 ~ 2, # male partners (sexual)
      hr3_relationship == 3 ~ 3, # offspring (vertical)
      hr3_relationship == 6 & hr4_sex == 1 ~ 3, # mother's mother (vertical)
      TRUE ~ 4
    ) %>% as.numeric()
  )
table(hovernet$value)

# Create the igraph object (hvr_ed_gsimp) from the edge list (hover_edge)
# make mother's PID the source ID instead of the hh id (so that we get mother's attributes)
indexmoIDs <- hovernet %>% filter(hr3_relationship==1) %>% select(hrhhid, pid) %>% rename(sourceid = pid)

hovernet <- left_join(hovernet, indexmoIDs, by = "hrhhid")
# relocate the sourceID (index mothers's ID) and participat ID to front of dataframe
hovernet <- hovernet %>% relocate(sourceid, pid) 

#subset for edge list
hover_edge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship,cpn_maternity) %>% filter(hr3_relationship != 1) %>% rename(mat = cpn_maternity)
# add edges going from male partner to index mother to create two-way arrows
maleedge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship,cpn_maternity) %>% filter(hr3_relationship == 2) %>% rename(pidman = pid, pidindexmo = sourceid, mat = cpn_maternity)  %>% rename(pid = pidindexmo, sourceid = pidman) %>% relocate(sourceid, pid)
hover_edge <- rbind(hover_edge, maleedge)
# add edges from maternities to women
matnodes <- hovernet %>% filter(hr3_relationship == 1) %>% select(pid, value, h10_hbv_rdt,cpn_maternity) %>% rename(sourceid = pid, pid = cpn_maternity) %>% relocate(sourceid, pid)
matnodes$hr3_relationship <- 0 # give maternity edges a different value for relationship
matnodes$mat <- matnodes$pid # creating a column for maternity to subset plots
hover_edge <- rbind(hover_edge, matnodes)
table(matnodes$mat)
# Create a dataframe of node attributes (hovernet_attr) using pid as the node ID

hovernet_attr <- hovernet %>%
  select(pid, hr4_sex, value,hr3_relationship, tab3hbv,i27a_rdt_result, totalpositive, cpn_maternity) %>%
  distinct()
hovernet_attr <- hovernet_attr %>% mutate(hbvnodecross = case_when(
  hr3_relationship==1 & tab3hbv==1 ~ 1, # index mother pos at cpn (exposed)
  hr3_relationship==1 & tab3hbv==0 ~ 2, # index mother neg at cpn (unexposed)
  hr3_relationship!=1 & tab3hbv==1 ~ 3, # alter pos 
  hr3_relationship!=1 & tab3hbv==0 ~ 4, # alter neg
))
table(hovernet_attr$hbvnodecross, useNA = "always")
# add maternities to list of attributes

mat_dist <- matnodes %>% group_by(pid) %>% select(pid) %>% distinct(pid)
mat_dist$hr4_sex <- NA # use different value for shape in igraph below
mat_dist$value <- 0 # use different value for shape in igraph below
mat_dist$hr3_relationship <- 0 # use different value for shape in igraph below
mat_dist$tab3hbv <- NA
mat_dist$i27a_rdt_result <- NA
mat_dist$totalpositive <- NA
mat_dist$cpn_maternity <- mat_dist$pid
mat_dist$hbvnodecross <- 0

ncol(hovernet_attr)
ncol(mat_dist)
hovernet_attr <- rbind(hovernet_attr, mat_dist)

view(hover_edge)

###8.2 make igraph object---------
hvr_ed_gsimp <- graph_from_data_frame(hover_edge, directed = TRUE)

### 8.3 Formatting--------------------
# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                       ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#use for edge list WITHOUT maternity edges
node_colors <- c("#B2182B","black", "#f4a582","gray70")
V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]
E(hvr_ed_gsimp)$width <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

###8.4 Plot-----
# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", 
setEPS()
postscript("./plots/network_clusters.eps")# quality=100)#, width=600, height=350)

plot(
  hvr_ed_gsimp,
  vertex.size = 4,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.1,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto
#dev.off()

#legends - need to figure out
legend("topright",
       title = "Transmission mode",
       legend = c("Sexual", "Vertical", "Other"),
       col = c("#3b7e88", "#1F3B4D","gray70"),
       lty = 1, # line type 1-solid
       lwd = 3, # line type 1-solid
       cex = 1, # relative size
       text.font = 10
       # lwd = 1.7,
       #box.lwd = 1,
       #x = 1.1,
       #y = 1
)
# vertices
legend(title = "Nodes",
       legend = c(
         "Index mother, HBsAg-",
         "Index mother, HBsAg+",
         "Male, HBsAg-",
         "Male, HBsAg+",
         "Female, HBsAg-",
         "Female, HBsAg+",
         "Maternity"
       ),
       pt.bg = c("black", "#B2182B", "gray70", "#f4a582", "gray70", "#f4a582", "#65b5c0"), # or #1f4a76 for maternities
       pch = c(21, 21, 22, 22, 21,21, 25),
       pt.cex = 1,
       cex = 0.7,
       #lwd = 1,
       box.lwd = 0,
       x = 1.1,
       y = 0.5)
legend("topright",
       title = "Transmission probability",
       legend = c("More likely", "Less likely"),
       col = c("black"),
       lty = c(1,3), # line type 1-solid
       cex = 1, # relative size
       text.font = 10,
       lwd = 5
       # lwd = 1.7,
       #box.lwd = 1,
       #x = 1.1,
       #y = 1
)
#C. Network stats----------------------------
# choosing these stats to analyze within communities to show they are the same
# Which community is the largest? The densest? The most interconnected? The most highly clustered? Has the shortest mean distances?
# some of these questions aren't relevant for us, but these stats from Abhi are the most relevant to our questions
t = matrix(nrow = 4, ncol = 6)

t[1, 1] <- "Community Number"
t[1, 2] <- "Size"
t[1, 3] <- "Density"
t[1, 4] <- "Mean Degree"
t[1, 5] <- "Clustering Coefficient"
t[1, 6] <- "Mean Distance"

## i) Calculation by subgraph
## Community 1 - Binza
# from above: hover_edge_binz <- hover_edge %>% filter(mat == "Binza")

hov_ed_stats_binz <- graph_from_data_frame(hover_edge_binz, directed = TRUE)

V(hov_ed_stats_binz)$degree <- degree(hov_ed_stats_binz)



t[2, 1] <- "Binza"
t[2, 2] <- length(V(hov_ed_stats_binz))
t[2, 3] <- round(edge_density(hov_ed_stats_binz), 2)
t[2, 4] <- round(mean(V(hov_ed_stats_binz)$degree), 2)
t[2, 5] <- round(transitivity(hov_ed_stats_binz), 2)
t[2, 6] <- round(mean_distance(hov_ed_stats_binz), 2)


## Community 2- Kingasani
# from above: hover_edge_king <- hover_edge %>% filter(mat == "Kingasani")

hov_ed_stats_king <- graph_from_data_frame(hover_edge_king, directed = TRUE)
V(hov_ed_stats_king)$degree <- degree(hov_ed_stats_king)


t[3, 1] <- "Kingasani"
t[3, 2] <- length(V(hov_ed_stats_king))
t[3, 3] <- round(edge_density(hov_ed_stats_king), 2)
t[3, 4] <- round(mean(V(hov_ed_stats_king)$degree), 2)
t[3, 5] <- round(transitivity(hov_ed_stats_king), 2)
t[3, 6] <- round(mean_distance(hov_ed_stats_king), 2)

## Community 3 - others
# from above: hover_edge_oth <- hover_edge %>% filter(mat != "Binza" & mat != "Kingasani")

hov_ed_stats_oth <- graph_from_data_frame(hover_edge_oth, directed = TRUE)
V(hov_ed_stats_oth)$degree <- degree(hov_ed_stats_oth)


t[4, 1] <- "Other structures"
t[4, 2] <- length(V(hov_ed_stats_oth))
t[4, 3] <- round(edge_density(hov_ed_stats_oth), 2)
t[4, 4] <- round(mean(V(hov_ed_stats_oth)$degree), 2)
t[4, 5] <- round(transitivity(hov_ed_stats_oth), 2)
t[4, 6] <- round(mean_distance(hov_ed_stats_oth), 2)

# 
# Fogarty network------------------------------------------------------------------
library(readxl)
fogedge <- read_excel("/Users/camillem/OneDrive - University of North Carolina at Chapel Hill/Epi PhD/Fogarty work/Data and analysis/FOGARTY DATASET.xlsx", sheet = "fog_edg_ex")

fogedge <- fogedge %>% filter(relationshipcat != "self")
fog_gr <- graph_from_data_frame(fogedge, directed = FALSE)


V(fogedge)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$maternity <-
  hovernet_attr$cpn_maternity[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

# Nodes, colors (w maternities)
V(hvr_ed_gsimp)$color <-
  ifelse(V(hvr_ed_gsimp)$hbsag==0,"#bbd458",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#B2182B", 
                ifelse(V(hvr_ed_gsimp)$hbsag == 2,"black",
                       ifelse(V(hvr_ed_gsimp)$hbsag == 3,"#f4a582",  "gray70"))))

#####use for edge list WITHOUT maternity edges
# node_colors <- c("#B2182B","black", "#f4a582","gray70")
# V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
# shape options; names(igraph:::.igraph.shapes)
# "pie" shape is interesting - use for household level plotting on map?
node_shapes <- c("square","circle",  "sphere") # "none" for maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]

# Edges
# solid vs dotted to distinguish sexual/vertical vs other
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$value == 4, "dotted", "solid")

# make sexual and vertical routes stand out
#edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

#edge_colors <- c("gray70","#1f4a76", "#65b5c0","gray70")

#edge_colors <- c("gray70","#1f4a76", "#1F3B4D","gray70")
edge_colors <- c("gray70","#479cf8", "#1F3B4D","gray70")
edge_colors <- c("gray70","#3b7e88", "#1F3B4D","gray70")

# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]
E(hvr_ed_gsimp)$width = E(hvr_ed_gsimp)$value

# alternatively could show transmission route by edge weight
# E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 1 # was 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
l <- layout_with_fr(hvr_ed_gsimp)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

l <- layout_with_kk(hvr_ed_gsimp)

node.size= c(10,10,10)
# Plot the igraph object
#png(file="./plots/network_hhclusters.png", quality=100)#, width=600, height=350)
# subset by structure

### 5.3 Plot -------------------------------

plot(
  hvr_ed_gsimp,
  vertex.size = 3,
  vertex.color = V(hvr_ed_gsimp)$color,
  #vertex.label = V(hvr_ed_gsimp)$label, # was NA
  vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  vertex.label.dist = 1,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.05,
  edge.width = (E(hvr_ed_gsimp)$width)*0.5,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  # main = "HOVER households with ≥2 HBV infections (n=14)",
  main = "Exposed households",
  layout=layout.auto,
  vertex.size=node.size*0.25) # layout_with_lgl, layout_nicely, layout.auto


plot(fog_gr)
