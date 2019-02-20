library(spinifex)
library(gganimate)
library(ggplot2)
library(magrittr)
library(knitr)
library(kableExtra)

## FUNCTION FOR STATIC OUTPPUT, for chap3 use to clean up code chunks.
array2static_sub <- function(.m_tour, .data, .m_var, .cat, .mag =2.2, .sub=3)
{
  slides       <- array2df(array = .m_tour, data = .data)
  basis_slides <- slides$basis_slides
  data_slides  <- slides$data_slides
  basis_slides <- dplyr::filter(basis_slides, slide %in% 1:.sub)
  data_slides  <- dplyr::filter(data_slides, slide %in% 1:.sub)
  n_slides     <- max(basis_slides$slide)

  # Initialize
  ## manip var asethetics
  p             <- nrow(basis_slides) / n_slides
  col_v         <- rep("grey80", p)
  col_v[.m_var] <- "blue"
  col_v         <- rep(col_v, n_slides)
  siz_v         <- rep(0.3, p)
  siz_v[.m_var] <- 1
  siz_v         <- rep(siz_v, n_slides)
  cat           <- rep(as.factor(.cat), n_slides)

  ## circle
  angle <- seq(0, 2 * pi, length = 180)
  circ  <- data.frame(c_x = cos(angle), c_y = sin(angle))
  circ[nrow(circ)+1, ] <- NA
  ## data asethetics
  data_slides <- data.frame(data_slides, cat = rep(.cat, n_slides))

  grid_b <- grid_t <-
    data.frame(slide = 1:n_slides,
               x = .mag*rep(1:.sub), y = .mag*rep(1, each = 3))
  grid_t$y <- grid_t$y + max(grid_t$y)
  # OUTER JOIN
  basis_grid <- merge(x = basis_slides, y = grid_t, by = "slide", all = TRUE)
  # CROSS JOIN
  circ_grid  <- merge(x = circ, y = grid_t, by = NULL)
  # OUTER JOIN
  data_grid  <- merge(x = data_slides, y = grid_b, by = "slide", all = TRUE)

  # BASIS
  gg1 <-
    ggplot(data = basis_grid) +
    # AXES LINE SEGMETNS
    geom_segment(aes(x = V1 + x, y = V2 + y, xend = x, yend = y),
                 color = col_v, size = siz_v) +
    # AXES TEXT LABELS
    geom_text(aes(x = V1 + x, y = V2 + y, label = lab_abbr),
              color = col_v, vjust = "outward", hjust = "outward") +
    # AXES FRAME NUM
    geom_text(aes(x = x - .7, y = y + 1.1,
                  label = paste0("frame: ",slide)), color = "grey50") +
    # AXES CIRCLE PATH
    suppressWarnings( # Suppress for "Removed 1 rows containing missing values."
      geom_path(data = circ_grid, color = "grey80",
                mapping = aes(x = x+c_x, y = y+c_y))
    )

  # PROJECTION
  gg2 <- gg1 +
    # PROJ DATA POINTS
    geom_point(data = data_grid, size = .7,
               mapping = aes(x = V1 + x, y = V2 + y, color = cat),
               shape = as.integer(cat) + 15) +
    # PROJ DATA FRAME NUM
    geom_text(data = data_grid, color = "grey50",
              mapping = aes(x = x - .7, y = y + 1.1,
                            label = paste0("frame: ",slide))) +
    theme_void() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed() +
    theme(legend.position="none")

  gg2
}

f_dat  <- tourr::rescale(flea[,1:6])
f_cat  <- factor(flea$species)
f_path <- save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)
f_mvar <- 5
f_proj <- data.frame(tourr::rescale(f_dat %*% f_bas))

# view_basis(f_bas, labels = colnames(f_dat)) +
#   geom_point(data = f_proj,
#              mapping = aes(x = X1 - 1.75, y = X2 - .5, color = f_cat),
#              pch = as.integer(f_cat) + 15)

f_ang <- .35
f_mt <- manual_tour(basis = f_bas,manip_var = f_mvar,angle = f_ang)

array2static_sub(.m_tour = f_mt, .data = f_dat, .m_var = f_mvar, .cat = f_cat)
