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

#ISSUE HERE
#circa slides 9-11 here.
play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,angle = f_ang)
play_manual_tour(data = f_dat, basis = f_bas, manip_var = f_mvar,angle = f_ang, render_type = render_gganimate)

### TS NOTES:
# check Phi:
if (F) {
  debug(manual_tour)
  manual_tour(basis = f_bas,manip_var = f_mvar,angle = f_ang)
  # Run till phi_path then go to degrees with:
  (phi_path + phi_start) * 180/pi
  # phi_path makes the right values.
  theta*180/pi # seems right.
  undebug(manual_tour)
  #double checking rotate_manip_space,
  rotate_manip_space
  # matrix validated with paper. trig test for phi? check manip space is ortho?
}
# Phi and Theta seem fine.
# rotate_manip_space is trivial and matrix already validated.

# issue with orthornormalize?!
if (F) {
  f_bas
  m_sp <- create_manip_space(f_bas,5)
  t(m_sp)%*%m_sp # is close to diag(3). trig test?
}

# create a dummy path
x <- c(seq(.6,1,.1),seq(1,0,-.1),seq(0,.6,.1))
y <- rep(0, length(x))
path <- as.matrix(data.frame(x,y)) #[23,2]

ang <- 30 *pi/180
rot <- matrix(c(cos(ang), -sin(ang), sin(ang), cos(ang)),
              byrow = T, ncol=2,nrow=2)
rpath<-path %*% rot

plot(rpath[,1],rpath[,2]) # !!rotate clockwise not anti clockwise.
#pos angle rotated right, maybe it just rotates in and unexpected way?
# maybe check the each frame with view_manip_space?

?view_manip_space

f_mt <- manual_tour(basis = f_bas,manip_var = f_mvar,angle = f_ang)
  (phi_path + phi_start) * 180/pi #phi1 = 41.5d, theta=19.5
  theta*180/pi # seems right.

for (i in 1:dim(f_mt)[3]){
  print(
    view_manip_space(basis = f_mt[,,i], manip_var = f_mvar) +
      geom_text(aes(x=-.2,y=0,label=i))
  )
}

#### Reviewing values of phi and the direction of movement shown in
  # view_manip_space, we need to go the other way. within manual_tour,
  # the values of Phi should have a sign chnage.