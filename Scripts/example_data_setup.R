
# Make an example data file so that people using our package can practice with this.
# Laura Keating, Nov 15 2022

# Take our original inputs
inputs<-read.csv(file="C:/Users/LauraK/OneDrive - The Calgary Zoological Society/New Species Prioritization/Manuscript/inputs_manuscript_v4.csv")


# Choose a few species to anonymize and include, include 'NA'
species <- unique(inputs$species)

species_1 <- species[3]# ferret
species_2 <- species[5] # lichen
species_3 <- species[8] # skipper
species_4 <- species[14] # frog
# species[13], species[16], species[19]

rows_to_include_generic <- which(inputs$species == "N/A")
rows_to_include_species_1 <- which(inputs$species == species_1)
rows_to_include_species_2 <- which(inputs$species == species_2)
rows_to_include_species_3 <- which(inputs$species == species_3)
rows_to_include_species_4 <- which(inputs$species == species_4)

rows_to_include <- c(rows_to_include_generic,
                     rows_to_include_species_1,
                     rows_to_include_species_2,
                     rows_to_include_species_3,
                     rows_to_include_species_4)

dat_example <- inputs[rows_to_include,]

# Rename species to be generic
dat_example$species[which(dat_example$species == species_1)] <- "Species 1"
dat_example$species[which(dat_example$species == species_2)] <- "Species 2"
dat_example$species[which(dat_example$species == species_3)] <- "Species 3"
dat_example$species[which(dat_example$species == species_4)] <- "Species 4"

dat_example$name[which(dat_example$species == "Species 1")] <- sub(paste0(species_1,"_"),"Species 1_", dat_example$name[which(dat_example$species == "Species 1")])
dat_example$name[which(dat_example$species == "Species 2")] <- sub(paste0(species_2,"_"),"Species 2_", dat_example$name[which(dat_example$species == "Species 2")])
dat_example$name[which(dat_example$species == "Species 3")] <- sub(paste0(species_3,"_"),"Species 3_", dat_example$name[which(dat_example$species == "Species 3")])
dat_example$name[which(dat_example$species == "Species 4")] <- sub(paste0(species_4,"_"),"Species 4_", dat_example$name[which(dat_example$species == "Species 4")])

# call it the same thing as before for simplicity
ExampleDataset <- dat_example

# Push it into the package
usethis::use_data(ExampleDataset, overwrite = TRUE)

