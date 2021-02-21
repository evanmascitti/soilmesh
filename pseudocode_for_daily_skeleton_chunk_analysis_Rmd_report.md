# Processing 

First, process the meshes and save them for later. This means I won't have to run long scripts every time I want to do a quick analysis. 
This should be considered part of data collection, not part of the data analysis. 

- list the file paths of all `.ply` files in a user-supplied directory 
- read all the files (use `parse_mesh_filename()`) in as a list and put them in a tibble with `tibble::enframe()`
- re-orient the meshes 
- process the meshes to remove the cylinder and make fine adjustments 
- optionally downsample to a target number of faces 
- create vector of new file names
- re-write the meshes into the derived data directory 

Update: as of 2021-02-21 the `batch_process_mesh()` function does everything above besides the downsampling.
___

# For the dily Rmd report/analysis 

## Processing 

- read the processed meshes 
- pair the tibble with the mix metadata for this experiment 
- compute the metrics 
- join with other data about water content and drydown times 


## Reporting 

- the water content and lamp times
- mesh metrics:
    - volume differences (below, above, total)
    - normalized new mesh surface area (i.e. relief index or roughness index)
    - DNE
- snapshot images of each mesh, labeled by cylinder, mix, and water content 

___


# Create a separate report which updates using all the current data collected on this set of mixtures 



