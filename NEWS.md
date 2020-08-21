# 0.99.0 or 0.99.x

+ the 0.99.0 or 0.99.x version mean I am submitting it to `Bioconductor`. (20200710, Fri)
+ 0.99.1 change `svg` of `dev` to `png`, set the dpi to 300. (20200714, Tue)
+ 0.99.2 `geom_axis_text` support the single column axis. (20200717, Fri)
+ 0.99.3 support `inward_circular` tree and `geom_axis_text` was build by `position_identityx`. (20200724, Fri)
+ 0.99.4 fix the constant of `barplot` bug. (2020-07-25, Sat)
+ 0.99.5 better support the layer when x axis is reverse. (2020-07-29, Wed)
+ 0.99.6 support adjusting the angle of geom_text. (2020-07-31, Fri)
+ 0.99.7 revise stylistic comment of R code and examples. (2020-08-04, Tue)
+ 0.99.8 change the formatting of code chunk. (2020-08-04, Tue)
+ 0.99.9 modified the `color` aesthetics for `geom_boxplot` and `geom_violin`. (2020-08-14, Fri)
  + keep all column of data of tree when it merge with external data. (2020-08-14, Fri)
+ 0.99.10 
  + add `geom_ringline` to create the grid line of external ring layers. (2020-08-21, Fri)
  + the `addbrink` and other argument control the line of margin has been removed. (2020-08-21, Fri)
  + add pseudo axis line in `geom_axis_text`. (2020-08-21, Fri) 

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
