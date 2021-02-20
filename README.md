<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggtreeExtra: An R Package To Add Geom Layers On Circular Or Other Layout Tree Of “ggtree”.

‘ggtreeExtra’ extends the method for mapping and visualizing associated
data on phylogenetic tree using ‘ggtree’. These associated data can be
mapped to circular layout, fan layout, or other layout tree built by
‘ggtree’ with the grammar of ‘ggplot2’.

# :writing\_hand: Author

[Shuangbin Xu](https://github.com/xiangpin) and [GuangChuang
Yu](https://guangchuangyu.github.io)

School of Basic Medical Sciences, Southern Medical University.

If you use `ggtreeExtra` in published research. Please cite the paper:

**S Xu**, Z Dai, P Guo, X Fu, S Liu, L Zhou, W Tang, T Feng, M Chen, L
Zhan, T Wu, E Hu and **G Yu**<sup>\*</sup>. ggtreeExtra: Compact
visualization of richly annotated phylogenetic data. ***Research
Square*** doi:
[10.21203/rs.3.rs-155672/v2](https://doi.org/10.21203/rs.3.rs-155672/v2).
- [Source code to produce Supplementary
Material](https://github.com/YuLab-SMU/plotting-tree-with-data-using-ggtreeExtra)

<!--

2. __G Yu__, DK Smith, H Zhu, Y Guan, TTY Lam^\*^. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and
   other associated data. __*Methods in Ecology and Evolution*__. 2017, 8(1):28-36.
   doi: [10.1111/2041-210X.12628](https://doi.org/10.1111/2041-210X.12628).

1. __G Yu__. Using ggtree to visualize data on tree-like structures. __*Current Protocols in Bioinformatics*__, 2020, 69:e96. doi: [10.1002/cpbi.96](https://doi.org/10.1002/cpbi.96).
   - [Source code and data to reproduce figures in the article](https://github.com/GuangchuangYu/ggtree-current-protocols)
2. __G Yu__^\*^, TTY Lam, H Zhu, Y Guan^\*^. Two methods for mapping and visualizing associated data on phylogeny using ggtree. __*Molecular Biology and Evolution*__, 2018, 35(2):3041-3043. 
   doi: [10.1093/molbev/msy194](https://doi.org/10.1093/molbev/msy194).
   - [Source code to produce Supplementary Material](https://github.com/GuangchuangYu/plotting_tree_with_data)
3. __G Yu__, DK Smith, H Zhu, Y Guan, TTY Lam^\*^. ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and 
   other associated data. __*Methods in Ecology and Evolution*__. 2017, 8(1):28-36. 
   doi: [10.1111/2041-210X.12628](https://doi.org/10.1111/2041-210X.12628).

-->

# :arrow\_double\_down: Installation

The development version from `github`:

``` r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("xiangpin/ggtreeExtra")
```

The released version from
[`Bioconductor`](https://bioconductor.org/packages/ggtreeExtra):

``` r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")

## BiocManager::install("BiocUpgrade") ## you may need this
BiocManager::install("ggtreeExtra")
```

# :beginner: Usage

Please refer to the [online
vignette](https://bioconductor.org/packages/devel/bioc/vignettes/ggtreeExtra/inst/doc/ggtreeExtra.html).

<!-- <img src="inst/extdata/fig1.png" style="display: block; margin: auto;" /> -->

# :sparkling\_heart: Contributing

We welcome any contributions\! By participating in this project you
agree to abide by the terms outlined in the [Contributor Code of
Conduct](CONDUCT.md).
