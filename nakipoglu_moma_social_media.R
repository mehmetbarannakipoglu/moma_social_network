# Load necessary libraries
library(tidyverse)  # For data manipulation
library(igraph)     # For graph creation

# Step 1: Load Dataset
moma_data <- read.csv("/Users/mehmetbarannakipoglu/Downloads/social-media-project/exhibitions-master/MoMAExhibitions1929to1989.csv", stringsAsFactors = FALSE)

# Inspect the dataset
head(moma_data)
summary(moma_data)

# Convert ExhibitionBeginDate to Date format
moma_data$ExhibitionBeginDate <- as.Date(moma_data$ExhibitionBeginDate, format = "%m/%d/%Y")

# Step 2: Data Cleaning with Unique ExhibitionID and Count
filtered_data <- moma_data %>%
  mutate(Year = as.numeric(format(ExhibitionBeginDate, "%Y"))) %>%
  filter(Year >= 1940 & Year <= 1970) %>%
  group_by(ExhibitionID) %>%
  mutate(Count = n()) %>%  
  ungroup() %>%
  distinct(ExhibitionID, .keep_all = TRUE)  

write.csv(filtered_data, "Cleaned_MoMA_Data_With_Unique_ExhibitionID.csv", row.names = FALSE)

stopifnot(length(unique(filtered_data$ExhibitionID)) == nrow(filtered_data))

# Step 3: Filter Top Exhibitions by Count
filtered_data <- filtered_data %>%
  arrange(desc(Count)) %>%
  head(80)

write.csv(filtered_data, "Top_100_MoMA_Data.csv", row.names = FALSE)

cat("Number of Exhibitions (Nodes):", nrow(filtered_data), "\n")

# Step 4: Create Edges for Bipartite Network
edges_exhibition_participant <- filtered_data %>%
  select(ExhibitionID, DisplayName) %>%
  distinct()

initial_graph <- graph_from_data_frame(edges_exhibition_participant, directed = FALSE)

V(initial_graph)$type <- ifelse(V(initial_graph)$name %in% filtered_data$ExhibitionID, TRUE, FALSE)

initial_layout <- layout_with_fr(initial_graph)
initial_communities <- cluster_leiden(initial_graph, resolution_parameter = 1.0)

plot(
  initial_communities, initial_graph,
  layout = initial_layout,
  vertex.size = ifelse(V(initial_graph)$type, 7, 5),
  vertex.color = ifelse(V(initial_graph)$type, "lightblue", "pink"),
  vertex.label = V(initial_graph)$name,  # Add node labels
  vertex.label.cex = 0.6,  # Adjust label size
  vertex.label.color = "black",  # Label color
  main = "Initial MoMA Network: Exhibition-Participant Edges Only (Leiden)"
)

pdf("Initial_MoMA_Network_Leiden_Labeled.pdf", width = 10, height = 10)
plot(
  initial_communities, initial_graph,
  layout = initial_layout,
  vertex.size = ifelse(V(initial_graph)$type, 7, 5),
  vertex.color = ifelse(V(initial_graph)$type, "lightblue", "pink"),
  vertex.label = V(initial_graph)$name,  # Add node labels
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  main = "Initial MoMA Network: Exhibition-Participant Edges Only (Leiden)"
)
dev.off()


cat("Initial graph with Leiden communities saved as 'Initial_MoMA_Network_Leiden.pdf'.\n")

# Step 5: Enhance the Network with Shared Nationality
edges_nationality <- filtered_data %>%
  filter(!is.na(Nationality) & Nationality != "") %>%
  group_by(Nationality) %>%
  filter(n() > 1) %>%
  summarise(
    pairs = list(combn(DisplayName, 2, simplify = FALSE))
  ) %>%
  unnest(pairs) %>%
  mutate(
    From = map_chr(pairs, 1),
    To = map_chr(pairs, 2)
  ) %>%
  select(From, To)

edges_exhibition_participant <- edges_exhibition_participant %>%
  mutate(ExhibitionID = as.character(ExhibitionID))

edges_nationality <- edges_nationality %>%
  mutate(From = as.character(From), To = as.character(To))

edges_combined <- bind_rows(
  edges_exhibition_participant %>% rename(From = ExhibitionID, To = DisplayName),
  edges_nationality
)

enhanced_graph <- graph_from_data_frame(d = edges_combined, directed = FALSE)

V(enhanced_graph)$type <- ifelse(V(enhanced_graph)$name %in% filtered_data$ExhibitionID, TRUE, FALSE)

enhanced_communities <- cluster_leiden(enhanced_graph, resolution_parameter = 1.0)

layout_enhanced <- layout_with_fr(enhanced_graph)
plot(
  enhanced_communities, enhanced_graph,
  layout = layout_enhanced,
  vertex.size = ifelse(V(enhanced_graph)$type, 7, 5),
  vertex.color = ifelse(V(enhanced_graph)$type, "lightblue", "pink"),
  vertex.label = V(enhanced_graph)$name,  # Add node labels
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  main = "Enhanced MoMA Network Visualization (Leiden)"
)

pdf("Enhanced_MoMA_Network_Leiden_Labeled.pdf", width = 10, height = 10)
plot(
  enhanced_communities, enhanced_graph,
  layout = layout_enhanced,
  vertex.size = ifelse(V(enhanced_graph)$type, 7, 5),
  vertex.color = ifelse(V(enhanced_graph)$type, "lightblue", "pink"),
  vertex.label = V(enhanced_graph)$name,  # Add node labels
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  main = "Enhanced MoMA Network Visualization (Leiden)"
)
dev.off()


cat("Enhanced graph with Leiden communities saved as 'Enhanced_MoMA_Network_Leiden.pdf'.\n")

# Step 6: Enhance the Network with Shared Gender
edges_gender <- filtered_data %>%
  filter(!is.na(Gender) & Gender != "") %>%
  group_by(Gender) %>%
  filter(n() > 1) %>%
  summarise(
    pairs = list(combn(DisplayName, 2, simplify = FALSE))
  ) %>%
  unnest(pairs) %>%
  mutate(
    From = map_chr(pairs, 1),
    To = map_chr(pairs, 2)
  ) %>%
  select(From, To)

edges_gender <- edges_gender %>%
  mutate(From = as.character(From), To = as.character(To))

edges_combined_gender <- bind_rows(
  edges_combined,
  edges_gender
)

gender_enhanced_graph <- graph_from_data_frame(d = edges_combined_gender, directed = FALSE)

V(gender_enhanced_graph)$type <- ifelse(V(gender_enhanced_graph)$name %in% filtered_data$ExhibitionID, TRUE, FALSE)

gender_enhanced_communities <- cluster_leiden(gender_enhanced_graph, resolution_parameter = 1.0)

layout_gender_enhanced <- layout_with_fr(gender_enhanced_graph)
plot(
  gender_enhanced_communities, gender_enhanced_graph,
  layout = layout_gender_enhanced,
  vertex.size = ifelse(V(gender_enhanced_graph)$type, 7, 5),
  vertex.color = ifelse(V(gender_enhanced_graph)$type, "lightblue", "pink"),
  vertex.label = V(gender_enhanced_graph)$name,  # Add node labels
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  main = "Enhanced MoMA Network with Gender (Leiden)"
)

pdf("Gender_Enhanced_MoMA_Network_Leiden_Labeled.pdf", width = 10, height = 10)
plot(
  gender_enhanced_communities, gender_enhanced_graph,
  layout = layout_gender_enhanced,
  vertex.size = ifelse(V(gender_enhanced_graph)$type, 7, 5),
  vertex.color = ifelse(V(gender_enhanced_graph)$type, "lightblue", "pink"),
  vertex.label = V(gender_enhanced_graph)$name,  # Add node labels
  vertex.label.cex = 0.6,
  vertex.label.color = "black",
  main = "Enhanced MoMA Network with Gender (Leiden)"
)
dev.off()


cat("Gender-enhanced graph with Leiden communities saved as 'Gender_Enhanced_MoMA_Network_Leiden.pdf'.\n")


# Step 7: Network Summary and Metrics for Gender-Enhanced Graph

# Node-Level Metrics
cat("\n### Node-Level Metrics ###\n")
V(gender_enhanced_graph)$degree <- degree(gender_enhanced_graph)
V(gender_enhanced_graph)$closeness <- closeness(gender_enhanced_graph, normalized = TRUE)
V(gender_enhanced_graph)$betweenness <- betweenness(gender_enhanced_graph, normalized = TRUE)

# Save Node-Level Metrics for All Nodes
all_node_metrics <- data.frame(
  name = V(gender_enhanced_graph)$name,
  type = V(gender_enhanced_graph)$type,
  degree = V(gender_enhanced_graph)$degree,
  closeness = V(gender_enhanced_graph)$closeness,
  betweenness = V(gender_enhanced_graph)$betweenness
)

# Save Node Metrics to CSV
write.csv(all_node_metrics, "All_Node_Metrics_Gender_Enhanced_MoMA_Network.csv", row.names = FALSE)
cat("Node metrics saved to 'All_Node_Metrics_Gender_Enhanced_MoMA_Network.csv'.\n")

# Optional: Display Metrics for Verification
cat("Summary of All Nodes' Metrics:\n")
print(all_node_metrics)

# Network-Level Metrics
cat("\n### Network-Level Metrics ###\n")
network_diameter <- diameter(gender_enhanced_graph)
global_clustering <- transitivity(gender_enhanced_graph, type = "global")
modularity_score <- modularity(gender_enhanced_graph, membership(gender_enhanced_communities))
density <- edge_density(gender_enhanced_graph)
num_cliques <- length(max_cliques(gender_enhanced_graph))

# Save Network Metrics
network_metrics <- data.frame(
  Metric = c("Network Diameter", "Global Clustering Coefficient", "Modularity", "Density", "Number of Cliques"),
  Value = c(network_diameter, global_clustering, modularity_score, density, num_cliques)
)
write.csv(network_metrics, "Network_Metrics_Gender_Enhanced_MoMA_Network.csv", row.names = FALSE)

# Display Metrics in Console
cat("Network Diameter:", network_diameter, "\n")
cat("Global Clustering Coefficient:", global_clustering, "\n")
cat("Modularity Score:", modularity_score, "\n")
cat("Density:", density, "\n")
cat("Number of Cliques:", num_cliques, "\n")
cat("Network metrics saved to 'Network_Metrics_Gender_Enhanced_MoMA_Network.csv'.\n")

# Summary Output for Interpretation
print(network_metrics)

# Step 8: Gender and Nationality Analysis (Adjusted for Participant Nodes)

# Filter only participant nodes for analysis
participant_nodes <- filtered_data %>%
  filter(!is.na(DisplayName))  # Exclude nodes that don't represent participants

# Analyze Gender Distribution
gender_distribution <- participant_nodes %>%
  filter(!is.na(Gender) & Gender != "") %>%
  group_by(Gender) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))
cat("\n### Gender Distribution ###\n")
print(gender_distribution)

# Save Gender Distribution to CSV
write.csv(gender_distribution, "Gender_Distribution.csv", row.names = FALSE)

# Compare Node Metrics by Gender (Only for Participant Nodes)
gender_metrics <- all_node_metrics %>%
  inner_join(participant_nodes %>% select(DisplayName, Gender), by = c("name" = "DisplayName")) %>%
  filter(!is.na(Gender) & Gender != "") %>%
  group_by(Gender) %>%
  summarise(
    AvgDegree = mean(degree, na.rm = TRUE),
    AvgBetweenness = mean(betweenness, na.rm = TRUE),
    AvgCloseness = mean(closeness, na.rm = TRUE)
  )
cat("\n### Node Metrics by Gender ###\n")
print(gender_metrics)

# Save Node Metrics by Gender to CSV
write.csv(gender_metrics, "Node_Metrics_by_Gender.csv", row.names = FALSE)

# Analyze Nationality Distribution (Only for Participant Nodes)
nationality_distribution <- participant_nodes %>%
  filter(!is.na(Nationality) & Nationality != "") %>%
  group_by(Nationality) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))
cat("\n### Nationality Distribution ###\n")
print(nationality_distribution)

# Save Nationality Distribution to CSV
write.csv(nationality_distribution, "Nationality_Distribution.csv", row.names = FALSE)

# Compare Node Metrics by Nationality (Only for Participant Nodes)
nationality_metrics <- all_node_metrics %>%
  inner_join(participant_nodes %>% select(DisplayName, Nationality), by = c("name" = "DisplayName")) %>%
  filter(!is.na(Nationality) & Nationality != "") %>%
  group_by(Nationality) %>%
  summarise(
    AvgDegree = mean(degree, na.rm = TRUE),
    AvgBetweenness = mean(betweenness, na.rm = TRUE),
    AvgCloseness = mean(closeness, na.rm = TRUE)
  ) %>%
  arrange(desc(AvgDegree))
cat("\n### Node Metrics by Nationality ###\n")
print(nationality_metrics)

# Save Node Metrics by Nationality to CSV
write.csv(nationality_metrics, "Node_Metrics_by_Nationality.csv", row.names = FALSE)