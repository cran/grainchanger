## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(grainchanger)
library(sf)
library(ggplot2)

coarse_dat <- cat_ls %>% 
  # get the bounding box
  st_bbox() %>% 
  # turn into an sfc object
  st_as_sfc() %>% 
  # negative buffer 
  st_buffer(-4) %>% 
  # make a square grid
  st_make_grid(cellsize = 19) %>% 
  # turn into sf object
  st_sf()

# we can plot this grid on top of the fine data
landscapetools::show_landscape(cat_ls) + 
  geom_sf(data = coarse_dat, alpha = 0.5)


coarse_dat$shdi_3 <- winmove_agg(coarse_dat = coarse_dat, 
                                 fine_dat = cat_ls,
                                 d = 3,
                                 type = "rectangle", 
                                 win_fun = shdi, 
                                 agg_fun = mean,
                                 is_grid = FALSE,
                                 lc_class = 1:4)

ggplot(coarse_dat, aes(fill = shdi_3)) + 
  geom_sf() + 
  theme_bw()

## ---- warning = TRUE----------------------------------------------------------
g_sf$shei_4 <- winmove_agg(coarse_dat = g_sf, 
                                 fine_dat = cat_ls,
                                 d = 4,
                                 type = "rectangle", 
                                 win_fun = shei, 
                                 agg_fun = mean,
                                 is_grid = FALSE,
                                 lc_class = 1:4)

## -----------------------------------------------------------------------------
library(sf)
library(ggplot2)

# coarse_dat <- st_read("your_file.shp")
coarse_dat <- st_read(system.file("shape/poly_sf.shp", package="grainchanger"))

coarse_dat$var_range <- nomove_agg(coarse_dat = coarse_dat,
                                   fine_dat = cont_ls,
                                   agg_fun = var_range,
                                   is_grid = FALSE)

ggplot(coarse_dat, aes(fill = var_range)) + 
  geom_sf() + 
  theme_bw()

## ----functions, echo = FALSE--------------------------------------------------
function_overview <- data.frame(
  `Function Name` = c("prop", "shdi", "shei", "range"),
  `Description` = c("Calculate the proportion of a given class", 
                    "Calculate the Shannon diversity", 
                    "Calculate the Shannon evenness", 
                    "Calculate the range of values"),
  `Additional arguments` = c("lc_class (numeric)", 
                             "lc_class (numeric)",
                             "lc_class (numeric)",
                             "")
)

knitr::kable(function_overview)

## ----torus--------------------------------------------------------------------
torus <- create_torus(cat_ls, 5)

landscapetools::show_landscape(torus)

