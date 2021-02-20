- read all the files in a directory whose names match a regular expression (likely *.ply but also containing the date as an argument?)
- downsmple these to a given number of faces 
- perform processing operations 
- create vector of new file names....
  - either set the names of the existing objects to this vector, or construct a new named list containing all the arguments for a mesh writing function
- write processed meshes to new files 
- read the processed mesh files into R session
  - enframe them into a tibble 
  - split the file names into usable data, namely the date and cylinder number 
  - join this tibble with other information contained in external csv files:
    - metadata about that particular test (the mix ID and contents)
    - the water content and lamp times
- iteratively perform 5 analyses and tack these onto the tibble as new columns:
  - volume differences (below, above, total)
  - normalized new mesh surface area (i.e. relief index or roughness index)
  - DNE
- snapshot images of each mesh, labeled by cylinder, mix, and water content 