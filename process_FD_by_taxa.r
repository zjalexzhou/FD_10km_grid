# Check if package is already installed
if (!requireNamespace("FD", quietly = TRUE)) {install.packages("FD")} # If not installed, install it
if (!requireNamespace("dplyr", quietly = TRUE)) {install.packages("dplyr")}
if (!requireNamespace("sf", quietly = TRUE)) {install.packages("sf")}
if (!requireNamespace("readr", quietly = TRUE)) {install.packages("readr")}

library(FD)
library(dplyr)
library(sf)
library(readr)


myFD <- function(path_site_species_mat, path_species_trait_mat, grid_num){

    print(prov)
    print(taxon)
    
    # Load the site_species data set
    site_species <- read_csv(path_site_species_mat)
    site_species <- site_species[grid_num,]
    
    # Filter out columns with row sums equal to 0
    # aka, species does not occur in any community (zero total abundance across all communities)
    site_species <- site_species[, colSums(site_species) != 0] %>% as.data.frame()
    
    # Check the number of remaining columns
    num_remaining_columns <- ncol(site_species)
    
    # If the number of remaining columns is 0, stop the R code
    if(num_remaining_columns == 0) {
      stop("No remaining columns after filtering.")
    }
    
    # Load the species_traits data set
    species_traits <- read_csv(path_species_trait_mat, locale = locale(encoding = "UTF-8"))   

    # Add underscores in scientific_name to ensure consistentcy
    species_traits$scientific_name <- gsub(" ", "_", species_traits$scientific_name)
   
    # Filter species_traits based on site-species
    species_traits <- species_traits %>% filter(scientific_name %in% colnames(site_species)) %>% as.data.frame()
    print(dim(species_traits))
    
    # Set the scientific name column as row names
    row.names(species_traits) <- species_traits$scientific_name
   
    # Order row names alphabetically
    species_traits <- species_traits[order(rownames(species_traits)), ]
    
    # Filter site_species based on the refined species_traits
    site_species <- site_species[colnames(site_species) %in% species_traits$scientific_name]
    print(dim(site_species))
    
    # Order columns alphabetically, as required by the `FD` package
    site_species <- site_species[, order(names(site_species))]
    
    # Add site name as rownames
    rownames(site_species) <- paste0("grid", grid_num)
    
    # Filter out rows with column sums equal to 0
    # aka, community has zero-sum abundances (no species)
    Sp_Richness <- rowSums(site_species)
    
    if(rowSums(site_species) == 0){
        next
    }
    site_species <- site_species[rowSums(site_species) != 0, ]
    # print(site_species)
    
    if(taxon == 'amphibian'){
        # For Amphibian
        species_traits <- species_traits %>% select(-scientific_name) %>%
            mutate_at(vars(`Young_site`, `SpawnLoc`, `Adult_site`, `mate_month`, `activity time`), as.factor)
    }
    
    if(taxon == 'bird'){
        # For Bird (adjusted traits columns)
        species_traits <- species_traits %>% select(-scientific_name, -...1) %>% 
            mutate_at(vars(nest_shape, flock_status, nest_site, migration_status), as.factor)
    }
    
    if(taxon == 'mammal'){
        # For Mammal
        species_traits <- species_traits %>% select(-scientific_name) %>%
            mutate_at(vars(body_mass, body_length, Maturity_d.x, Generation_length_d.x, Litters_per_year_n), as.numeric) %>%
            mutate_at(vars(Litter_size_n, Diet, Activity_cycle, Habitat_.breadth, Distribution.type,), as.factor)
    }
    
    if(taxon == 'reptile'){
        # For Reptile
        species_traits <- species_traits %>% select(-scientific_name) %>%
            mutate_at(vars(Trophic_level, Diel_activity, Habitat_breadth_IUCN), as.factor)
    }
    
    species_traits <- species_traits %>% select_if(~!all(is.na(.)))
    # print(species_traits)
    
    FD_list <- list(trait = species_traits, abun = as.matrix(site_species))
    
    site_species$gridID <- grid_num
    site_species$prov <- prov
    site_species$taxon <- taxon
    
    
#    FD_stats <- dbFD(FD_list$trait,
#                        FD_list$abun,
#                        w.abun = FALSE,
#                        corr = "lingoes",
#                        m = 10)
    
    FD_stats <- dbFD(FD_list$trait,
                        FD_list$abun,
                        w.abun = FALSE,
                        calc.FRic = FALSE,
                        corr = "lingoes")
    print(FD_stats)
    

    # Extract Rao'Q index from the FD
    site_species$SR <- FD_stats$nbsp
    site_species$RaoQ <- FD_stats$RaoQ
#    site_species$FRic <- FD_stats$FRic
    site_species$FEve <- FD_stats$FEve
#    site_species$FDiv <- FD_stats$FDiv
    # Extract Functional Dispersion from the FD
    site_species$FDis <- FD_stats$FDis
    

#     site_species <- site_species %>% select(`gridID`, `prov`, `taxon`, `SR`, `RaoQ`, `FRic`, `FEve`, `FDiv`, `FDis`)
    site_species_results <- site_species %>% select(`gridID`, `prov`, `taxon`, `SR`, `RaoQ`, `FEve`, `FDis`)
    folder <- paste0(YOUR_RESULT_FOLDER_PATH, taxon, "/", prov)
    # Check if the folder exists
    if (!dir.exists(folder)) {
      # If the folder doesn't exist, create it
      dir.create(folder)
      cat("Folder created:", folder, "\n")
    }
    write.csv(site_species_results, paste0(YOUR_RESULT_FOLDER_PATH, taxon, "/", prov, '/', grid_num, '.csv'))
}

# Start timing
start_time <- Sys.time()

# Call your function
# myFD()
# ('Anhui', 'mammal',132)

YOUR_RESULT_FOLDER_PATH = "YOUR_RESULT_FOLDER_PATH"

path_site_species_mat = paste0("./grid_species/", commandArgs(TRUE)[1])
path_species_trait_mat = paste0("./species_traits/", commandArgs(TRUE)[2])

myFD(commandArgs(TRUE)[1], commandArgs(TRUE)[2], as.integer(commandArgs(TRUE)[3]))

# End timing
end_time <- Sys.time()

# Calculate the elapsed time
elapsed_time <- end_time - start_time
print(elapsed_time)

