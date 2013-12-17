source("../R/qviz.R")

png("_images/species_for_multiple_ivs.png", width=1000, height=1000, res=120)
layout(matrix(c(1:6, 7, 7, 8), nr=3, byrow=TRUE))
qviz(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)
dev.off()

png("_images//sepal_length_for_multiple_ivs.png", width=1000, height=800, res=100)
layout(matrix(1:4, nr=2, byrow=TRUE))
qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
dev.off()