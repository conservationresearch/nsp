
# Make an example data file so that people using our package can practice with this.
# Laura Keating, Nov 15 2022

# Take our original inputs
inputs<-read.csv(file="C:/Users/LauraK/OneDrive - The Calgary Zoological Society/New Species Prioritization/Manuscript/inputs_manuscript_v4.csv")


# Choose a few species to anonymize and include, include 'NA'
species <- unique(inputs$species)

rows_to_include_generic <- which(inputs$species == "N/A")
rows_to_include_species_1 <- which(inputs$species == species[1])
rows_to_include_species_2 <- which(inputs$species == species[2])
rows_to_include_species_3 <- which(inputs$species == species[3])
rows_to_include_species_4 <- which(inputs$species == species[4])

rows_to_include <- c(rows_to_include_generic,
                     rows_to_include_species_1,
                     rows_to_include_species_2,
                     rows_to_include_species_3,
                     rows_to_include_species_4)

dat_example <- inputs[rows_to_include,]

# Rename species to be generic
dat_example$species[which(dat_example$species == species[1])] <- "Species 1"
dat_example$species[which(dat_example$species == species[2])] <- "Species 2"
dat_example$species[which(dat_example$species == species[3])] <- "Species 3"
dat_example$species[which(dat_example$species == species[4])] <- "Species 4"

ExampleDataset <- dat_example

# Push it into the package
usethis::use_data(ExampleDataset, overwrite = TRUE)
