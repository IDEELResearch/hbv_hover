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

# hrhhid: household ID
# pid: participant ID (household ID with '-' and two digits)
# h10_hbv_rdt: exposure which is also index mothers HBV status: 1=exposed (index mother is HBV+), 0=unexposed (index mother HBV-)
# hr3_relationship: relationship of participant (alter) to index mother (ego). important ones: 1=index mother (ego); 2=male partner (sexual transmission route); 3=child of index mother (vertical transmission); 6=parent of index mother (restrict on gender variable to get mother for vertical relationship)
# hr4_sex: gender (1=female, 0=male)
# totalpositive: total HBsAg positives in household; use below to create a smaller more manageable subset as an example
# i27a_rdt_result: HBsAg result of each participant at the enrollment visit (study outcome)
# cpn_maternity: maternity center households were recruited from. could be used to create inter-household relationships (for now have focused on intra-household relationships)


# 1. SETUP ----

## 1.1 Load required packages ----
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

# 2. DATA TO NETWORK ----
hovernet200 <- inddata1 %>% select("hrhhid","pid","h10_hbv_rdt","hr3_relationship","hr4_sex","totalpositive","i27a_rdt_result","cpn_maternity","tab3hbv") #"i27a_rdt_result_f","age_combined", "hr4_sex_f","hdov",
# tab3hbv: use CPN result for mothers, enrollment result for everyone else
# inddata1$tab3hbv <- ifelse(inddata1$hhmemcat==2, inddata1$h10_hbv_rdt, inddata1$i27a_rdt_result) # hhmemcat==2 are index moms

## 2.1 Read in the hovernet200 data ----
# hovernet200 <- readRDS("hovernet200.RDS")

## 2.2 Data processing ----
# Subset to households with more than one HBV+ individual
hovernet <- hovernet200 %>% filter(totalpositive > 1)

# Create a value variable for the edges (1 for ego, 2 for sexual relationship, 3 for vertical transmission, 4 for other relationship)
# NOTE TO CAMILLE: I CHANGED THE VALUE ASSIGNMENTS TO MAKE THE COLORS EASIER
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

## 2.3 To Network ----
# Create a dataframe of node attributes (hovernet_attr) using pid as the node ID
hovernet_attr <- hovernet %>%
  select(pid, hr4_sex, value,hr3_relationship, tab3hbv,i27a_rdt_result, totalpositive) %>%
  distinct()
hovernet_attr <- hovernet_attr %>% mutate(hbvnodecross = case_when(
  hr3_relationship==1 & tab3hbv==1 ~ 1, # index mother pos at cpn (exposed)
  hr3_relationship==1 & tab3hbv==0 ~ 2, # index mother neg at cpn (unexposed)
  hr3_relationship!=1 & tab3hbv==1 ~ 3, # alter pos 
  hr3_relationship!=1 & tab3hbv==0 ~ 4, # alter neg
))
table(hovernet_attr$hbvnodecross, useNA = "always")

# Create the igraph object (hvr_ed_gsimp) from the edge list (hover_edge)
# make mother's PID the source ID instead of the hh id (so that we get mother's attributes)
indexmoIDs <- hovernet %>% filter(hr3_relationship==1) %>% select(hrhhid, pid) %>% rename(sourceid = pid)

hovernet <- left_join(hovernet, indexmoIDs, by = "hrhhid")
# relocate the sourceID (index mothers's ID) and participat ID to front of dataframe
hovernet <- hovernet %>% relocate(sourceid, pid) 

#subset for edge list
hover_edge <- hovernet %>% select(sourceid, pid, value, h10_hbv_rdt, hr3_relationship) %>% filter(hr3_relationship != 1)

hvr_ed_gsimp <- graph_from_data_frame(hover_edge, directed = TRUE)

# Add node attributes to the igraph object
V(hvr_ed_gsimp)$sex <-
  hovernet_attr$hr4_sex[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$hbsag <-
  hovernet_attr$hbvnodecross[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]
V(hvr_ed_gsimp)$totalpos <-
  hovernet_attr$totalpositive[match(V(hvr_ed_gsimp)$name, hovernet_attr$pid)]

hovernet_attr$pid

# 3. PLOTS ----
## 3.1 Plotting simple network, colored by positivity, shapes for sex, linetype for exposure ----

# Assign colors and shapes for vertices
# abhi
V(hvr_ed_gsimp)$color <-
  ifelse(is.na(V(hvr_ed_gsimp)$hbsag),
         "ghostwhite",
         ifelse(V(hvr_ed_gsimp)$hbsag == 1, "#f4a582", "#878787"))

# my attempt to color ego by exp/unexp
node_colors <- c("#B2182B","black", "#f4a582","gray70")

node_colors <- c("#CF6D49","black","#dd8b3f","gray70")
node_colors <- c("#ae5d22","black","#f3b05c","gray70")
V(hvr_ed_gsimp)$color <- node_colors[V(hvr_ed_gsimp)$hbsag]

# Define node shapes
node_shapes <- c( "square" ,"circle") # "circle",maternity (will have NA in attribute table), square=male, circle=female

V(hvr_ed_gsimp)$shape <-
  node_shapes[ifelse(is.na(V(hvr_ed_gsimp)$sex), 3, V(hvr_ed_gsimp)$sex + 1)]


# Define edge color palette
edge_colors <- c("gray70","#60bfc1","#5d8eb6","gray70") # 1-mothers (doesn't exist), 2 sexual, 3 vertical, 4 other

edge_colors <- c("black", "tomato","gray70", "#0BC9CD")

# Add edge colors and types
E(hvr_ed_gsimp)$weight <- E(hvr_ed_gsimp)$value
# edge color seems distracting
E(hvr_ed_gsimp)$color <- edge_colors[E(hvr_ed_gsimp)$value]

# solid vs dotted by another attribute - this better for type of relationship
E(hvr_ed_gsimp)$lty <-
  ifelse(E(hvr_ed_gsimp)$h10_hbv_rdt == 0, "dotted", "solid")

# Add vertex labels
V(hvr_ed_gsimp)$label.cex <- 0.5
V(hvr_ed_gsimp)$label.family <- "Helvetica"
V(hvr_ed_gsimp)$label <- V(hvr_ed_gsimp)$name

# Compute the layout with minimal overlap
layout <- layout_with_fr(hvr_ed_gsimp)

# If you want to label houses by their id, uncomment this chunk below and the three lines in the plot, and comment out "vertex.label = NA"
# # Define node label positions
# label_pos <- ifelse(is.na(V(hvr_ed_gsimp)$sex),
#                     1,-1)

# Plot the igraph object
plot(
  hvr_ed_gsimp,
  vertex.size = 4,
  vertex.color = V(hvr_ed_gsimp)$color,
  vertex.label = NA,
  # vertex.label = ifelse(is.na(V(hvr_ed_gsimp)$sex), V(hvr_ed_gsimp)$name, NA),
  # vertex.label.dist = label_pos,
  # vertex.label.cex = 0.6,
  edge.arrow.size = 0.1,
  edge.width = 3,
  edge.color = E(hvr_ed_gsimp)$color,
  edge.lty = E(hvr_ed_gsimp)$lty,
  vertex.shape = V(hvr_ed_gsimp)$shape,
  main = "Figure 1: Household-level Exposure Network"
)

# Add legends
legend(
  legend = c("Ego", "Sexual", "Offspring/Mother", "Other"),
  col = c("black", "tomato", "gray70","#0BC9CD"),
  lty = 1,
  cex = 0.7,
  lwd = 1.7,
  title = "Relationship",
  box.lwd = 1,
  x = 1.1,
  y = 1
)

legend(
  legend = c("Unexposed", "Exposed"),
  lty = c(3, 1),
  col = "black",
  cex = 0.7,
  title = "HBV Exposure",
  box.lwd = 0,
  x.intersp = 1.5,
  horiz = FALSE,
  x = 1.1,
  y = 0.7
)

legend(
  legend = c(
    "Female, Negative",
    "Female, Positive",
    "Male, Negative",
    "Male, Positve",
    "Ego/Household"
  ),
  pt.bg = c("#E4F1EE", "#C2948A", "#E4F1EE", "#C2948A",  "black"),
  pch = c(21, 21, 22, 22, 19),
  pt.cex = 1,
  cex = 0.7,
  title = "Sex",
  box.lwd = 0,
  x = 1.1,
  y = 0.5
)

# ## 3.2 Mapped based on specific locations ----
# # Import shapefile (for this, I used something random)
# poland_shp <-
#   sf::st_read("shapefile.shp") %>% filter(CNTR_CODE == "PL")
# 
# # Abhi's way of attaching random coords to the hrhhids below!
# ## Get coords (I did this just to get a random set of coords)
# locations <-
#   st_centroid(poland_shp) %>%
#   dplyr::mutate(lon = sf::st_coordinates(.)[, 1],
#                 lat = sf::st_coordinates(.)[, 2])
# 
# ## Sample 14 random rows of locations
# locations <- locations[sample(nrow(locations), 14),]
# 
# # Get the vertex names from the igraph object for vertices with sex = NA (these are the households, not hte people)
# ego <- V(hvr_ed_gsimp)[is.na(V(hvr_ed_gsimp)$sex)]$name
# 
# ## Add the ego to the locations dataset
# locations <- cbind(locations, ego)
# 
# # Normalize the coordinates to fit in the igraph plot
# normalize_coords <- function(coords) {
#   (coords - min(coords, na.rm = TRUE)) / (max(coords, na.rm = TRUE) - min(coords, na.rm = TRUE))
# }
# 
# normalized_lon <- normalize_coords(locations$lon)
# normalized_lat <- normalize_coords(locations$lat)
# 
# # Create the layout matrix
# layout_matrix <-
#   matrix(NA, nrow = length(V(hvr_ed_gsimp)), ncol = 2)
# 
# names(layout_matrix) <- V(hvr_ed_gsimp)$name
# 
# # Now, I want only the households mapped by their location, and the rest mapped using the FR
# fr_layout <- layout_with_fr(hvr_ed_gsimp)
# scaling_factor <- 0.01  # Adjust this value to make the edges shorter or longer.
# 
# # Set the ego nodes coordinates
# for (i in 1:length(ego)) {
#   ego_index <- which(names(layout_matrix) == ego[i])
#   fr_layout[ego_index,] <-
#     c(normalized_lon[i], 1 - normalized_lat[i])
# }
# 
# # And I want the edge lengths to be small, else they'll spread out too much
# limit_edge_length <- function(layout, graph, max_length) {
#   for (edge in E(graph)) {
#     from_node <- as.numeric(edge[1])
#     to_node <- as.numeric(edge[2])
#     from_coords <- layout[from_node,]
#     to_coords <- layout[to_node,]
#     
#     distance <- sqrt(sum((from_coords - to_coords) ^ 2))
#     if (!is.na(distance) && distance > max_length) {
#       direction <- (to_coords - from_coords) / distance
#       new_to_coords <- from_coords + direction * max_length
#       layout[to_node,] <- new_to_coords
#     }
#   }
#   return(layout)
# }
# 
# 
# # Set the ego nodes coordinates
# for (i in 1:length(ego)) {
#   ego_index <- which(names(layout_matrix) == ego[i])
#   fr_layout[ego_index,] <- c(normalized_lon[i], 1 - normalized_lat[i])
# }
# 
# # Set the alter's coordinates
# find_connected_ego <- function(node, graph) {
#   ego_nodes <- V(graph)[is.na(V(graph)$sex)]$name
#   connected_ego <- ""
#   for (ego in ego_nodes) {
#     if (are_adjacent(graph, ego, node)) {
#       connected_ego <- ego
#       break
#     }
#   }
#   return(connected_ego)
# }
# 
# for (i in 1:nrow(fr_layout)) {
#   node_name <- names(layout_matrix)[i]
#   if (!node_name %in% ego) {
#     connected_ego <- find_connected_ego(node_name, hvr_ed_gsimp)
#     ego_index <- which(names(layout_matrix) == connected_ego)
#     fr_layout[i,] <- fr_layout[i,] * scaling_factor + fr_layout[ego_index,]
#   }
# }
# 
# 
# # Plot the igraph object
# plot(
#   hvr_ed_gsimp,
#   vertex.size = 4,
#   vertex.color = V(hvr_ed_gsimp)$color,
#   vertex.label = NA,
#   edge.arrow.size = 0.3,
#   edge.width = 1.7,
#   edge.color = E(hvr_ed_gsimp)$color,
#   edge.lty = E(hvr_ed_gsimp)$lty,
#   vertex.shape = V(hvr_ed_gsimp)$shape,
#   main = "Figure 2: Mapped Household Network",
#   layout = fr_layout
# )
# 
# # Add legends
# legend(
#   legend = c("Ego", "Sexual", "Offspring/Mother", "Other"),
#   col = c("black", "tomato", "#0BC9CD", "gray70"),
#   lty = 1,
#   cex = 0.7,
#   lwd = 1.7,
#   title = "Relationship",
#   box.lwd = 1,
#   x = 1.1,
#   y = 1
# )
# 
# legend(
#   legend = c("Unexposed", "Exposed"),
#   lty = c(3, 1),
#   col = "black",
#   cex = 0.7,
#   title = "HBV Exposure",
#   box.lwd = 0,
#   x.intersp = 1.5,
#   horiz = FALSE,
#   x = 1.1,
#   y = 0.7
# )
# 
# legend(
#   legend = c(
#     "Female, Negative",
#     "Female, Positive",
#     "Male, Negative",
#     "Male, Positve",
#     "Ego/Household"
#   ),
#   pt.bg = c("#E4F1EE", "#C2948A", "#E4F1EE", "#C2948A",  "black"),
#   pch = c(21, 21, 22, 22, 19),
#   pt.cex = 1,
#   cex = 0.7,
#   title = "Sex",
#   box.lwd = 0,
#   x = 1.1,
#   y = 0.5
# )

# 