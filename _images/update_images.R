png("_images/factor_for_numeric.png", width=1000, height=2000, res=150)
layout(matrix(c(1:7, 9, 8, 8), nr=5, byrow=TRUE))
qviz(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)
dev.off()

png("_images/numeric_for_numeric.png", width=1000, height=1200, res=150)
layout(matrix(1:6, nr=3, byrow=TRUE))
qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
dev.off()

png("_images/numeric_for_factor_and_numeric.png", width=1000, height=2000, res=150)
layout(matrix(1:15, nc=3, byrow=TRUE))
qviz(Petal.Length ~ Species + Sepal.Width + Sepal.Length, data=iris)
dev.off()

png("_images/factor_for_factor_and_numeric.png", width=1000, height=4800, res=150)
layout(matrix(1:36, nc=3, byrow=TRUE))
library(mlbench)
data(BreastCancer)
qviz(Class ~ Cl.thickness + Cell.size + Bl.cromatin, data=BreastCancer)
dev.off()