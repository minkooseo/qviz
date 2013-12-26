png("_images/species_for_multiple_ivs.png", width=1000, height=2000, res=150)
layout(matrix(c(1:7, 9, 8, 8), nr=5, byrow=TRUE))
qviz(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)
dev.off()

png("_images/sepal_length_for_multiple_ivs.png", width=1000, height=1200, res=150)
layout(matrix(1:6, nr=3, byrow=TRUE))
qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
dev.off()