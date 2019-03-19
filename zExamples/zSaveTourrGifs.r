library(tourr)
f_dat <- tourr::rescale(flea[,1:6])
f_cat <- factor(flea$species)
f_holes_path <- save_history(f_dat, guided_tour(holes()))
f_rand_path  <- save_history(f_dat, grand_tour())

#n_frames:
dim(f_holes_path)[3]
dim(f_rand_path)[3]

library(RColorBrewer)
pch <- as.integer(f_cat) + 15
pal <- brewer.pal(n = 3, name = "Dark2")
col <- pal[as.numeric(flea$species)]
#animate_xy(flea[,-7], col=col)

render(f_dat, planned_tour(f_holes_path),display_xy(pch=pch, col=col),
       "png", "./tourr_output/fleaholes_%02d.png",)

render(f_dat, planned_tour(f_rand_path),display_xy(pch=pch, col=col),
       "png", "./tourr_output/flearand_%02d.png", frames = 99)

print("repeat last slide as needed. I use 300 ms wait. defaults are 480x480px")
browseURL("https://gifmaker.me/")
