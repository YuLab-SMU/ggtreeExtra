<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggadd: add layers which can be shifted horizontally or vertically.

The layers of ‘ggplot’ can be added using ‘+’, they will be overlaped if
‘x’ and ‘y’ are in the same range. However, sometimes we want to
visualize them separately. In cartesian coordinate system, this problem
can be solved by ‘facet’, But in polar coordinate system, The ‘facet’
can not do this. To solve the problem, this package was developed. The
application of this package is that it can add some layers on the tree
of “ggtree”.

# :writing\_hand: Author

[Shuangbin Xu](https://github.com/xiangpin) and [GuangChuang
Yu](https://guangchuangyu.github.io)

School of Basic Medical Sciences, Southern Medical University.

`ggadd` has not been published, if you use `ggadd` in published
research. Please cite the most appropriate paper(s) from this list:

1.  **G Yu**. Using ggtree to visualize data on tree-like structures.
    ***Current Protocols in Bioinformatics***, 2020, 69:e96. doi:
    [10.1002/cpbi.96](https://doi.org/10.1002/cpbi.96).
      - [Source code and data to reproduce figures in the
        article](https://github.com/GuangchuangYu/ggtree-current-protocols)
2.  **G Yu**<sup>\*</sup>, TTY Lam, H Zhu, Y Guan<sup>\*</sup>. Two
    methods for mapping and visualizing associated data on phylogeny
    using ggtree. ***Molecular Biology and Evolution***, 2018,
    35(2):3041-3043. doi:
    [10.1093/molbev/msy194](https://doi.org/10.1093/molbev/msy194).
      - [Source code to produce Supplementary
        Material](https://github.com/GuangchuangYu/plotting_tree_with_data)
3.  **G Yu**, DK Smith, H Zhu, Y Guan, TTY Lam<sup>\*</sup>. ggtree: an
    R package for visualization and annotation of phylogenetic trees
    with their covariates and other associated data. ***Methods in
    Ecology and Evolution***. 2017, 8(1):28-36. doi:
    [10.1111/2041-210X.12628](https://doi.org/10.1111/2041-210X.12628).

# :arrow\_double\_down: Installation

The development version from `github`:

``` r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("xiangpin/ggadd")
```

# :beginner: Usage

``` r
library(ggadd)
library(ggtree)
library(ggplot2)
library(ggnewscale)
library(treeio)
library(ggstar)

trfile <- system.file("extdata", "tree.nwk", package="ggadd")
tippoint1 <- system.file("extdata", "tree_tippoint_bar.csv", package="ggadd")
ring1 <- system.file("extdata", "first_ring_discrete.csv", package="ggadd")
ring2 <- system.file("extdata", "second_ring_continuous.csv", package="ggadd")
ringpoint <- system.file("extdata", "second_ring_point.csv", package="ggadd")

tr <- read.tree(trfile)
dat1 <- read.csv(tippoint1)
dat2 <- read.csv(ring1)
dat3 <- read.csv(ring2)

p <- ggtree(tr, layout="circular", size=0.1) + 
    geom_treescale(x=6, y=0, linesize=0.2, fontsize=1.2)

p1 <- p + geom_add(data=dat1,
                   geom=geom_star,
                   mapping=aes(y=ID, fill=Location, size=Length, starshape=Group),
                   tippoint=TRUE,
                   starstroke=0.2) +
          scale_size_continuous(range=c(1, 3),
                                guide=guide_legend(keywidth=0.5, keyheight=0.5, override.aes=list(starshape=15), order=2)) +
          scale_fill_manual(values=c("#D15FEE","#EE6A50","#FFC0CB","#8E8E38","#9ACD32","#006400","#8B4513"),
                            guide="none")+
          scale_starshape_manual(values=c(1, 15),
                                 guide=guide_legend(keywidth=0.5, keyheight=0.5, order=1))

p2 <- p1 +new_scale_fill()+ 
          geom_add(data=dat2,
                    geom=geom_tile,
                    mapping=aes(y=ID, x=Pos, fill=Type),
                    pratio=0.25,
                    position=position_identityx()) +
          scale_fill_manual(values=c("#339933", "#dfac03"),
                            guide=guide_legend(keywidth=0.5, keyheight=0.5, order=3))

p3 <- p2 + new_scale_fill()+
           geom_add(data=dat3,
                    geom=geom_tile,
                    mapping=aes(y=ID, x=Type2, alpha=Alpha, fill=Type2),
                    pratio=0.15,
                    position=position_identityx()) +
          scale_fill_manual(values=c("#b22222", "#005500", "#0000be", "#9f1f9f"),
                            guide=guide_legend(keywidth=0.5, keyheight=0.5, order=4)) +
          scale_alpha_continuous(range=c(0, 0.4),
                                guide=guide_legend(keywidth=0.5, keyheight=0.5, order=5))

p4 <- p3 + new_scale_fill()+
           geom_add(data=dat1,
                    geom=geom_bar,
                    mapping=aes(y=ID, x=Abundance, fill=Location),
                    pratio=0.4,
                    stat="identity",
                    orientation="y",
                    position=position_stackx()) +
           scale_fill_manual(values=c("#D15FEE","#EE6A50","#FFC0CB","#8E8E38","#9ACD32","#006400","#8B4513"),
                             guide=guide_legend(keywidth=0.5, keyheight=0.5, order=6)) +
           theme(legend.position=c(0.95, 0.5),
                 legend.title=element_text(size=7),
                 legend.text=element_text(size=6),
                 legend.spacing.y = unit(0.02, "cm"))
p4
```

<img src="inst/extdata/fig1.png" style="display: block; margin: auto;" />

# :sparkling\_heart: Contributing

We welcome any contributions\! By participating in this project you
agree to abide by the terms outlined in the [Contributor Code of
Conduct](CONDUCT.md).
