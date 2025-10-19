#-------------------------------------------------------------------
# Project: Intra-Family Variability in Non-Cognitive Skills
# Script:  RLMS 2016-2019 Individual Data Preparation
# Author:  Garen Avanesian
# Date:    3 December 2024
#-------------------------------------------------------------------


# This script takes idind of those participated in 28th wave and if they were in 25th, it also selects their family data

# Read the file with relatives
ind_relations <- 
  read_sav(file.path(inputData, "ind_relatives_id_i_idind_USER_RLMS-HSE_HH_1994_2022_rus.sav")) %>%
  mutate(idind = as.factor(idind),
         id_i = as.factor(id_i)) 

# Extract the data on sibling
ind28 <-
  ind_relations %>%
  filter(id_w == 28) %>%
  select(id_w, idind, id_i, 
         # r01a1, # spouse
         # r02a1, r02a2, # parents (blood)
         # r04a1, r04a2, r04a3, r04a5, r04a6, r04a7, r04a8, r04a9, # children of respondents (blood)
         r06a1, r06a2, r06a3, r06a5, r06a6, r06a7, r06a8) 

# View(ind_relations)

idind28 <- unique(ind28$idind)
length(idind28)

# Step 1: Reshape the data into long format
sibling_data_long28 <- 
  ind28 %>%
  select(id_i, idind, starts_with("r06a")) %>% 
  pivot_longer(cols = starts_with("r06a"), 
               values_to = "sibling_id", 
               values_drop_na = TRUE) %>%
  mutate(id_i = factor(id_i),
         sibling_id = factor(sibling_id)) %>%
  rename(individual_id = id_i)



# View(sibling_data_long)

# Step 2: Create edges for graph (pairs of individual and sibling IDs)
edges28 <- 
  sibling_data_long28 %>%
  filter(!is.na(sibling_id)) %>%
  select(individual_id, idind, sibling_id)

# Step 3: Create a graph and identify connected components (families)
g28 <- 
  graph_from_data_frame(edges28 %>% select(individual_id, sibling_id), directed = FALSE)

family_groups28 <- components(g28)$membership

# Step 4: Add family group ID back to the original data
family_ids28 <- 
  sibling_data_long28 %>%
  distinct(individual_id, idind) %>%
  mutate(family_id = family_groups28[as.character(individual_id)]) %>%
  select(individual_id, idind, family_id)

# View results
# View(family_ids)

fam_id28 <-
  family_ids28 %>%
  select(idind, family_id) %>%
  group_by(family_id) %>%
  mutate(n_inds = n()) %>%
  ungroup() %>%
  mutate(family_id = as.factor(family_id))

# View(fam_id28)

# this is the vector of those who provided family information in 28th wave]
sibling_vector28 <- unique(fam_id28$idind)


# Read the file with relatives
ind25 <- 
  ind_relations %>%
  filter(id_w == 25) %>%
  # exclude those who participated in the 28th wave and have already been mapped
  filter(!idind %in% idind28) %>%
  select(id_w, idind, id_i, 
         r06a1, r06a2, r06a3, r06a5, r06a6, r06a7, r06a8) 


### Now create family ID for respondents who participated ONLY in 25th wave
sibling_data_long25 <-
  ind25 %>%
  select(id_i, idind, starts_with("r06a")) %>% 
  pivot_longer(cols = starts_with("r06a"), 
               values_to = "sibling_id", 
               values_drop_na = TRUE) %>%
  mutate(id_i = factor(id_i),
         sibling_id = factor(sibling_id)) %>%
  rename(individual_id = id_i)


# Step 2: Create edges for graph (pairs of individual and sibling IDs)
edges25 <- 
  sibling_data_long25 %>%
  filter(!is.na(sibling_id)) %>%
  select(individual_id, idind, sibling_id)

# Step 3: Create a graph and identify connected components (families)
g25 <- 
  graph_from_data_frame(edges25 %>% select(individual_id, sibling_id), directed = FALSE)

family_groups25 <- components(g25)$membership

# Step 4: Add family group ID back to the original data
family_ids25 <- 
  sibling_data_long25 %>%
  distinct(individual_id, idind) %>%
  mutate(family_id = family_groups25[as.character(individual_id)]) %>%
  select(individual_id, idind, family_id)

# View results
# View(family_ids)
# View(ind_fam_with_family_id)

fam_id25 <-
  family_ids25 %>%
  select(idind, family_id) %>%
  group_by(family_id) %>%
  mutate(n_inds = n()) %>%
  ungroup() %>%
  mutate(family_id = as.factor(family_id)) %>%
  mutate(family_id = paste0("25_", family_id))

# View(fam_id25)

# this is the vector of those who provided family information in 25th wave]
sibling_vector25 <- unique(fam_id25$idind)

length(sibling_vector25)

# now we need to merge both sibling vectors into one 
fam_id <-
  fam_id28 %>%
  full_join(fam_id25) 

# View(fam_id)
# check if length matches
# length(sibling_vector25) + length(sibling_vector28)
