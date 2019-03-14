library(spinifex)
library(gganimate)
# library(ggplot2)
# library(magrittr)

#HIDDEN
f_dat  <- tourr::rescale(flea[,1:6])
f_cat  <- factor(flea$species)
f_path <- save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)
f_mvar <- 5
f_proj <- data.frame(tourr::rescale(f_dat %*% f_bas))
f_ang  <- .35

# array
dim(f_dat)
#> [1] 74  6
f_mt <- spinifex::manual_tour(
  basis = f_bas,manip_var = f_mvar,angle = f_ang)
dim(f_mt) # Basis only
#> [1]  6  2 11

# long df
f_mtour_df <- spinifex::array2df(f_mt, data = f_dat)
dim(f_mt_df$basis_slides)
#> [1] 66  4
# added frame and lab_abbr
dim(f_mt_df$data_slides)
#> [1] 814  3
# added frame



f_mtour_df <- spinifex::array2df(f_mt, data = f_dat)
dim(f_mtour_df$basis_slides)
#> [1] 66  4
# added frame and lab_abbr
dim(f_mtour_df$data_slides)
#> [1] 814  3
# added frame

play_manual_tour(data = f_dat, basis = f_bas, manip_var = 5,
                 render_type = render_gganimate, cat_var = f_cat)


if (F){
  ggplot2::ggplot() +
    geom_segment(data = basis_slides,
                 aes(x = V1, y = V2, xend = 0, yend = 0, frame = slide)) +
    geom_point(data = data_slides,
               aes(x = V1, y = V2, frame = slide)) +
    gganimate::transition_states(states = slide,
                                 transition_length = 0,
                                 state_length = 1 / fps)
}



?render_gganimate
render_gganimate
