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

#slides 9 and 10 here.
play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,angle = f_ang)
play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,angle = f_ang, render_type = render_gganimate)

### TS NOTES
if (F) {
  debug(manual_tour)
  manual_tour(basis = f_bas,manip_var = f_mvar,angle = f_ang)
  myPhi = (phi_path + phi_start)*180/pi
  # phi_path makes the right values, is it in the rotation?
  theta*180/pi # is small, seems right.
  undebug(manual_tour)
  manual_tour
}

f_mt <- manual_tour(basis = f_bas,manip_var = f_mvar,angle = f_ang)

# i=9
# d <- as.data.frame(f_mt[,,i])
# ggplot(d) +
#   geom_segment(aes(x=V1,y=V2,xend=0,yend=0)) +
#   geom_text(aes(x=V1,y=V2,label=rownames(d)))