# qviz: R package for Quick Data Visualization


qviz is a R package supporting quick visualization of data. qviz can be used during the data exploration stage which is the essential part of any data analysis. Users just need to specify formula and data, and the tool draws charts that intuitively make sense. Types, colors and all the details of the charts are determined by the package automatically.

qviz provides with a single function: qviz(formula, data).

## Installation

    > install.packages("devtools")
    > library(devtools)
    > install_github('qviz','minkooseo')

## Example

Commands in the below renders several plots. To review them, use either 'par(ask=TRUE)' or appropriate layout commands. If you want output like the below, look at this  [script](https://github.com/minkooseo/qviz/blob/master/_images/update_images.R).


### Numeric Y with numeric Xs

    > data(iris)
    > qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)

![screenshot](https://github.com/minkooseo/qviz/raw/master/_images/numeric_for_numeric.png)

### Numeric Y with numeric and factor X

    > data(iris)
    > qviz(Petal.Length ~ Species + Sepal.Width + Sepal.Length, data=iris)

![screenshot3](https://github.com/minkooseo/qviz/raw/master/_images/numeric_for_factor_and_numeric.png)

### Factor Y with numeric Xs

    > data(iris)
    > qviz(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)

![screenshot2](https://github.com/minkooseo/qviz/raw/master/_images/factor_for_numeric.png)

### Factor Y with numeric and factor X
    
    > library(mlbench)
    > data(BreastCancer)
    > qviz(Class ~ Cl.thickness + Cell.size + Bl.cromatin, data=BreastCancer)

![screenshot4](https://github.com/minkooseo/qviz/raw/master/_images/factor_for_factor_and_numeric.png)
