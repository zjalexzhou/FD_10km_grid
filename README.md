# FD_10km_grid
R code combos based on the FD package to calculate 10-km functional diversity from species traits

## process_FD_by_taxa.r

**Function: myFD(path_site_species_mat, path_species_trait_mat, grid_num)**

@ path_site_species_mat:

@ path_species_trait_mat:

@ grid_num: the "study site," *aka*, the serial number of the 10-km grid to be processed.

-> Example (if in linux system using command line): Rscript process_FD_by_taxa.r 'Anhui' 'mammal' 132

-> This script with input parameters will load your site-species matrix (for Anhui Province, CN), species_trait matrix (for mammal taxon), and compute your functional diversity metrics for grid 132101.