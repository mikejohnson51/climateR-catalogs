
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WIP: climateR-catalogs

<!-- badges: start -->
<!-- badges: end -->

The goal of climateR-catalogs is to build data catalogs usable with
terra, climateR, gdalio, opendap.catalog, ect.

The catalog(s) will be built using `targets` in this repo, and deployed
as JSON artifacts at
“<https://mikejohnson51.github.io/climateR-catalogs/catalog.json>”

``` r
targets::tar_visnetwork()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Here is a minimal example with the base information added:

``` r
pacman::p_load(jsonlite, dplyr, opendap.catalog, AOI, terra)

url = "https://mikejohnson51.github.io/climateR-catalogs/catalog.json"

cat = fromJSON(url, simplifyDataFrame = TRUE) %>% 
  filter(description == "NLCD Land_Cover L48 2019")

t(cat)
#>             [,1]                                                                                                 
#> source      NA                                                                                                   
#> product     "USGS NLCD Land Cover"                                                                               
#> URL         "/vsicurl/https://storage.googleapis.com/feddata-r/nlcd/2019_Land_Cover_L48.tif"                     
#> units       NA                                                                                                   
#> description "NLCD Land_Cover L48 2019"                                                                           
#> X1          "-2493045"                                                                                           
#> Xn          "2342655"                                                                                            
#> Y1          "-2493045"                                                                                           
#> Yn          "3310005"                                                                                            
#> resX        "30"                                                                                                 
#> resY        "30"                                                                                                 
#> ncols       "161190"                                                                                             
#> nrows       "104424"                                                                                             
#> proj        "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
```

``` r
(output   = dap(catalog = cat,  AOI = aoi_get("Fort Collins")))
#> class       : SpatRaster 
#> dimensions  : 667, 548, 1  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : -768615, -752175, 1975575, 1995585  (xmin, xmax, ymin, ymax)
#> coord. ref. : Albers Conical Equal Area 
#> source      : memory 
#> color table : 1 
#> categories  : NLCD Land Cover Class, Histogram, Red, Green, Blue, Opacity 
#> name        :        NLCD Land Cover Class 
#> min value   :                   Open Water 
#> max value   : Emergent Herbaceous Wetlands

plot(output)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This is still a little clunky but you can pass multirow “catalog”
data.frames staight to dap! For example say you want soil sand content,
land cover and elevation for the city of Fort Collins:

``` r
cat = fromJSON("docs/catalog.json", simplifyDataFrame = TRUE) %>% 
  filter(description == c("NLCD Land_Cover L48 2019") |
         product %in% c("mean sand 0-5cm", "30m CONUS DEM"))


output  = lapply(1:nrow(cat), function(x){   dap(catalog = cat[x,],  
                                         AOI = aoi_get("Fort Collins")) })

for(i in 1:3){
  plot(output[[i]], main = cat$product[i])  
}   
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->
