# qviz: R package for Quick Data Visualization


qviz is a R package supporting quick visualization of data. qviz can be used during the data exploration stage which is the essential part of any data analysis. Users just need to specify formula and data, and the tool draws charts that intuitively make sense. Types, colors and all the details of the charts are determined by the package automatically.

qviz provides with a single function, qviz() whose parameters are formula and data.

## Example

### Numeric Y with numeric Xs

    data(iris)
    qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
    
![screenshot](https://github.com/minkooseo/qviz/raw/master/_images/sepal_length_for_multiple_ivs.png)

### Factor Y with numeric Xs

    data(iris)
    qviz(Species ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)

![screenshot2](https://github.com/minkooseo/qviz/raw/master/_images/species_for_multiple_ivs.png)
