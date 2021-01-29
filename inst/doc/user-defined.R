## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(grainchanger)
library(landscapetools)

num_cells <- function(x, lc_class, ...) {
  return(sum(x == lc_class))
}
d <- winmove(cat_ls, 4, "rectangle", num_cells, lc_class = 2)
show_landscape(d) 

## -----------------------------------------------------------------------------
library(ggplot2)
g_sf$num_cells <- winmove_agg(g_sf, cat_ls, 4, "rectangle", num_cells, lc_class = 2)

ggplot(g_sf, aes(fill = num_cells)) + 
  scale_fill_viridis_c() + 
  geom_sf() + 
  theme_bw()

## -----------------------------------------------------------------------------
num_classes <- function(x, ...) {
  length(unique(x))
}

g_sf$num_classes <- nomove_agg(g_sf, cat_ls, num_classes)

ggplot(g_sf, aes(fill = as.factor(num_classes))) +
  scale_fill_viridis_d("num_classes") + 
  geom_sf() + 
  theme_bw()

## -----------------------------------------------------------------------------
cv <- function(x) {
  sd(x) / mean(x)
}

poly_sf$cv <- nomove_agg(poly_sf, cont_ls, cv)

ggplot(poly_sf, aes(fill = cv)) +
  scale_fill_viridis_c() + 
  geom_sf() + 
  theme_bw()

