#```{r, fig.width=10.4, fig.height=4}
f_pca <- stats::prcomp(f_dat, center = TRUE, scale. = TRUE)
f_pca_bas <- as.matrix(f_pca$rotation[, 1:2])
colnames(f_pca_bas) <- c("V1", "V2")
f_pca_proj <- tourr::rescale(f_pca$x) %>% as.data.frame()

### Basis and proj
gg_f_pca_proj <-
  view_basis(f_pca_bas, labels = colnames(f_dat)) +
  geom_point(data = f_pca_proj,
             mapping = aes(x = PC1 +.75, y = PC2 -.5, color = f_cat),
             pch = as.integer(f_cat) + 15) +
  ggtitle("flea - PC1:2") + xlab("PC1") + ylab("PC2")

### Cumsum variation
f_pca_var <- as.data.frame(
  rbind(c(0,0,0,0),
        cbind("PC" = 1:6,
              "Var" = f_pca$sdev,
              "Prop_Var" = f_pca$sdev/sum(f_pca$sdev),
              "cumsum_Prop_Var" = cumsum(f_pca$sdev)/sum(f_pca$sdev),
              "var_not_explained" = 1 -  cumsum(f_pca$sdev)/sum(f_pca$sdev)
        )
  )
)

ribbon <- f_pca_var[-1:-2, c(1,4)]
ribbon <- ribbon[rep(row.names(ribbon), 2), ]
ribbon[6:10, 1] <- ribbon[6:10, 1]+.999

gg_f_pca_var <-
  ggplot(f_pca_var, aes(x=PC,y=cumsum_Prop_Var,label=round(cumsum_Prop_Var, 2))) +
  geom_point(size=1) + geom_step() +
  ggtitle("Cumulative variance by PC") +
  geom_text(nudge_x=0, nudge_y=.05) +
  labs(x="Principal component", y="Cumulative variance") +
  theme_bw() +
  coord_fixed(ratio = .5) +
  geom_ribbon(data=ribbon, aes(x=PC, ymin=cumsum_Prop_Var, ymax=1),
              fill="red", alpha=.2) + theme(legend.position = 'none')

cowplot::plot_grid(gg_f_pca_proj, gg_f_pca_var, align = "h", rel_widths = c(2, 1.6))
#```

#- Best we can do in $d$-dim static projection
#- Variable transparency; map to variable space
#- How much variation is lost?
#
#  ---

#  # tSNE & non-linear projections

#  **RO #B) What benefits does UCS provide over popular alternatives?**

#*Future case study*

#  ```{r, results='hide', fig.width=6.1, fig.height=3.8}
f_tsne <- Rtsne(f_dat, dims = 2, perplexity=15, verbose=TRUE, max_iter = 500)
colnames(f_tsne$Y) <- paste0("tS",1:2)
f_tsne_proj <- tourr::rescale(f_tsne$Y) %>% as.data.frame()

ggplot() +
  geom_point(data = f_tsne_proj,
             mapping = aes(x = tS1, y = tS2, color = f_cat),
             pch = as.integer(f_cat) + 15) +
  theme_void()+ theme(legend.position = 'none') +
  ggtitle("flea - tSNE Components 1,2")
#```
#
#- Variables opacity; how different? in which direction? What's the relationship?
#- Can suffer from overfitting
#- Binary indicator of clustering/outliers
#
#---
#
## 2D UCS vs alternatives -- dynamic linear
#
#**RO #B) What benefits does UCS provide over popular alternatives?**
#
#*Future case study*
#
#
#.pull-left[
#```{r, out.width='70%', out.height='70%'}
knitr::include_graphics("./images/f_mt.gif")
#```
#
#As an [html widget](https://nspyrison.netlify.com/thesis/flea_manualtour_mvar5/)
#]
#
#.pull-right[
#<br>
#- Variable transparency; map to variable space
#- Subsets of variation viewed as projection changes
#]