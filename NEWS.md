# ggtreeExtra 1.1.6

+ check whether the value of x is numeric to avoid warnings when x is factor. (2021-02-24, Wed)
+ remove axis of first `geom_tile` of vignettes, since the axis of this layer is meaningless. (2021-02-24, wed)

# ggtreeExtra 1.1.5

+ support `title` of panel. (2021-02-03, Wed)
  - <https://github.com/YuLab-SMU/ggtreeExtra/issues/7>
+ add citation info. (2021-02-04, Thu)
+ don't use `svg` dev. (2021-02-04, Thu)

# ggtreeExtra 1.1.4

+ add `position_points_jitterx` and `position_raincloudx` for **geom** of `ggridges`. (2021-01-20, Wed)
+ supports `geom_msa` of `ggmsa`. (2021-01-21, Thu)
+ specific `position` method for specific *geom* method automatically. (2021-01-27, Wed)

# ggtreeExtra 1.1.3

+ support multiple density plot from **geom** of *ggridges*. (2020-12-31, Thu)
  `geom_density_ridges`, `geom_density_ridges2`, `geom_density_ridges_gradient`,
  `geom_ridgeline`, `geom_ridgeline_gradient`.

# ggtreeExtra 1.1.2

+ support `subset` in mapping, but the `data` should also be provided. (2020-11-30, Mon)
+ add default position methods for common geometric functions. (2020-12-18, Fri)

# ggtreeExtra 1.0.0

+ Bioconductor 3.12 release (2020-10-28, Wed)

## the 0.99.0 or 0.99.x version mean I am submitting it to `Bioconductor`. (20200710, Fri)

+ 0.99.1 change `svg` of `dev` to `png`, set the dpi to 300. (20200714, Tue)
+ 0.99.2 `geom_axis_text` support the single column axis. (20200717, Fri)
+ 0.99.3 support `inward_circular` tree and `geom_axis_text` was build by `position_identityx`. (20200724, Fri)
+ 0.99.4 fix the constant of `barplot` bug. (2020-07-25, Sat)
+ 0.99.5 better support the layer when x axis is reverse. (2020-07-29, Wed)
+ 0.99.6 support adjusting the angle of geom_text. (2020-07-31, Fri)
+ 0.99.7 revise stylistic comment of R code and examples. (2020-08-04, Tue)
+ 0.99.8 change the formatting of code chunk. (2020-08-04, Tue)
### 0.99.9
  + modified the `color` aesthetics for `geom_boxplot` and `geom_violin`. (2020-08-14, Fri)
  + keep all column of data of tree when it merge with external data. (2020-08-14, Fri)
### 0.99.10 
  + add `geom_ringline` to create the grid line of external ring layers. (2020-08-21, Fri)
  + the `addbrink` and other argument control the line of margin has been removed. (2020-08-21, Fri)
  + add pseudo axis line in `geom_axis_text`. (2020-08-21, Fri) 
### 0.99.11
  + `geom_axis_text` and `geom_ringline` are removed. (2020-08-24, Mon)
  + user can use `axis.params=list(add.axis=TRUE)` and `grid.params=list(add.grid=TRUE)` to
    add the axis and grid lines of external layers, respectively. (2020-08-24, Mon)
### 0.99.12
  + add upper and lower grid lines of y. (2020-08-26, Wed)
### 0.99.13
  + update the method of `ggplot_add.layer_fruits`, when the `offset` between different `fruit_plot` 
    is different, will use each `offset` in each `fruit_plot`. (2020-08-31, Mon)
### 0.99.14
  + update `normxy` to support `dendrogram` layout. (2020-09-02, Wed)
### 0.99.15
  + add `add.another.axis` in `geom_fruit` to add another axis. (2020-09-04, Fri)
  + update `normxy` to fix the bug when negative values are present. (2020-09-04, Fri)
  + update `ggplot_add` method of `geom_fruit` to support the orientation 
    which x axis of the external is from bottom to top, when layout is `dendrogram`. (2020-09-04, Fri)
### 0.99.16
  + change `add.axis` in axis.params from TRUE or FALSE to `x` or `y` or `xy`. (2020-09-05, Sat)
  + remove `add.grid` in `grid.params` and default of `grid.params` is NULL. (2020-09-07, Mon)
  + update method of `axis tick` and remove `nbreaks`, the breaks will be calculate by `pretty`. (2020-09-07, Mon)
  + use `substitute` to allow list of `axis.params` or `grid.params` has empty argument, 
    eg `grid.params=list(color="black",)`. (2020-09-07, Mon)
### 0.99.17
  + change `add.axis` to `axis` in `axis.params`. (2020-09-08, Tue)
### 0.99.18
  + add `nbreak` in `axis.params` of `geom_fruit`, it will be sent to `n` of `pretty` to 
    generate the desired number of intervals of `axis`. (2020-09-08, Tue)
### 0.99.19
  + modified the namespace, remove `geom_vline`, add `geom_segment`. (2020-09-11, Fri)
  
# 0.0.1

+ add vignettes. (20200707, Tue)

# 0.0.0.9

+ support add the axis text of extra layer of `geom_fruit` using `geom_axis_text`. (20200630, Tue)
+ optimize normalization of extra layer data. (20200701, Wed) 
+ fixed bug: changed layer to layers. (20200703, Fri)

# 0.0.0.8

+ support data of NULL in `geom_fruit`, and user can add data by `%<+%` of `ggtree`. (20200628, Sun)

# 0.0.0.7 

+ add `geom_fruit_list` to support add the same position for multi layers. (20200622, Mon)

# 0.0.0.6

+ automatically detect the 'position'. (20200612)

# 0.0.0.5

+ change `pratio` parameter to `pwidth` and remove `tippoint` parameter.
+ change `geom_add` function to `geom_totree` function. (20200610)

# 0.0.0.4

+ support tip point for `geom_star` or `geom_point`
+ support `geom_boxplot` and `geom_violin` (20200606)

# 0.0.0.3

+ add marginal line, and when x is character, the new x normalized 
  should be started with zero. (20200601)

# 0.0.0.2

+ The distance between the panel and tree can be adjusted 
  using the "offset". The value of associate panel were 
  normalized in the range of x of tree. The width can be 
  adjusted using the "pratio". The "offset" and "pratio" 
  are the ratio related to tree. (20200529)

# 0.0.0.1

+ first version to `github` (20200528)
