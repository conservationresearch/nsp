
# Make an example data file so that people using our package can practice with this.
# Laura Keating, Nov 15 2022

# Take our original inputs
inputs<-read.csv(file="C:/Users/LauraK/OneDrive - The Calgary Zoological Society/New Species Prioritization/Manuscript/inputs_manuscript_v4.csv")

# Choose a few species to anonymize and include, include 'NA'
# I decided to include all the invertebrates 

species <- unique(inputs$species)

species_A <- species[2]# snail
species_B <- species[8] # skipper
species_C <- species[11] # hairstreak
species_D <- species[13] # ringlet
species_E <- species[17] # checkerspot
species_F <- species[20] # yucca

rows_to_include_generic <- which(inputs$species == "N/A")
rows_to_include_species_A <- which(inputs$species == species_A)
rows_to_include_species_B <- which(inputs$species == species_B)
rows_to_include_species_C <- which(inputs$species == species_C)
rows_to_include_species_D <- which(inputs$species == species_D)
rows_to_include_species_E <- which(inputs$species == species_E)
rows_to_include_species_F <- which(inputs$species == species_F)

rows_to_include <- c(rows_to_include_generic,
                     rows_to_include_species_A,
                     rows_to_include_species_B,
                     rows_to_include_species_C,
                     rows_to_include_species_D,
                     rows_to_include_species_E,
                     rows_to_include_species_F)

dat_example <- inputs[rows_to_include,]

# Rename species to be generic
dat_example$species[which(dat_example$species == species_A)] <- "Species A"
dat_example$species[which(dat_example$species == species_B)] <- "Species B"
dat_example$species[which(dat_example$species == species_C)] <- "Species C"
dat_example$species[which(dat_example$species == species_D)] <- "Species D"
dat_example$species[which(dat_example$species == species_E)] <- "Species E"
dat_example$species[which(dat_example$species == species_F)] <- "Species F"

dat_example$name[which(dat_example$species == "Species A")] <- sub(paste0(species_A,"_"),"Species A_", dat_example$name[which(dat_example$species == "Species A")])
dat_example$name[which(dat_example$species == "Species B")] <- sub(paste0(species_B,"_"),"Species B_", dat_example$name[which(dat_example$species == "Species B")])
dat_example$name[which(dat_example$species == "Species C")] <- sub(paste0(species_C,"_"),"Species C_", dat_example$name[which(dat_example$species == "Species C")])
dat_example$name[which(dat_example$species == "Species D")] <- sub(paste0(species_D,"_"),"Species D_", dat_example$name[which(dat_example$species == "Species D")])
dat_example$name[which(dat_example$species == "Species E")] <- sub(paste0(species_E,"_"),"Species E_", dat_example$name[which(dat_example$species == "Species E")])
dat_example$name[which(dat_example$species == "Species F")] <- sub(paste0(species_F,"_"),"Species F_", dat_example$name[which(dat_example$species == "Species F")])

# call it the same thing as before for simplicity
ExampleDataset <- dat_example

# Push it into the package
usethis::use_data(ExampleDataset, overwrite = TRUE)

