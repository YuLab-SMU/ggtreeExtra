context('compute_aes')

library(ggtree)
library(ggtreeExtra)
library(ggplot2)
set.seed(123)

tr <- rtree(10)
dat <- data.frame(id=rep(tr$tip.label, 4), 
                  sample=rep(c("A1","A2","B1","B2"),10), 
                  group=rep(c("A","A","B","B"),10))
dat$value <- abs(rnorm(n=40))*20

test_that('x in mapping of geom_fruit support function expression', {
    p <- ggtree(tr) + 
         geom_fruit(
           data=dat, 
           geom=geom_col, 
           mapping=aes(
             x=log10(value+1), 
             y=id, 
             fill=group
           ), 
           grid.params=list(linetype=2), 
           orientation="y", 
           axis.params=list(axis="x",text.size=2)
         )
    expect_true(ggplot2::is.ggplot(p))
})

