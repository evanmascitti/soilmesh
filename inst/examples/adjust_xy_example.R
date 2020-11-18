adjusted <- adjust_xy(mesh = untrimmed_mesh1)
rgl::shade3d(untrimmed_mesh1, color="firebrick")
rgl::shade3d(adjusted, color= "darkblue")
add_origin_axes()
