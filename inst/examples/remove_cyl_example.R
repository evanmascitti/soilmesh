untrimmed <- rgl::translate3d(untrimmed_mesh1, x=-160, y=0, z=0)
trimmed <- remove_cyl(untrimmed_mesh1)
rgl::shade3d(untrimmed)
rgl::shade3d(trimmed)
