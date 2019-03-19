library(spinifex)
library(gganimate)
library(ggplot2)

f_dat  <- tourr::rescale(flea[,1:6])
f_cat  <- factor(flea$species)
f_path <- save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)
f_mvar <- 5
f_proj <- data.frame(tourr::rescale(f_dat %*% f_bas))

f_ang <- .08
f_mt <- manual_tour(basis = f_bas,manip_var = f_mvar,angle = f_ang)

play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,angle = f_ang)
undebug(manual_tour)
manual_tour
