compute_ellipse_points <- function(x, y, data) {
  require(ellipse)
  return(ellipse(var(data[, c(x, y)]),
                 centre=colMeans(data[, c(x, y)]),
                 level=0.95))
}

GetPanelMinMax <- function(x, y, data, want_ellipse) {
  MinMaxWithMargin <- function(points) {
    min_value <- min(points)
    max_value <- max(points)
    range <- max_value - min_value
    return (c(min_value - range * 0.05, max_value + range * 0.05))
  }
  if (want_ellipse) {
    ellipse_points <- compute_ellipse_points(x, y, data)
    return(list(xlim=MinMaxWithMargin(ellipse_points[, 1]),
                ylim=MinMaxWithMargin(ellipse_points[, 2])))
  }
  return(list(xlim=MinMaxWithMargin(data[, x]),
              ylim=MinMaxWithMargin(data[, y])))
}

draw <- function(x, y, data, want_regression=FALSE, want_ellipse=FALSE, col=1, pch=1, ...) {
  drawing <- list()
  drawing$xlab <- x
  drawing$ylab <- y
  panel_min_max <- GetPanelMinMax(x, y, data, want_ellipse)
  drawing$xlim <- panel_min_max$xlim
  drawing$ylim <- panel_min_max$ylim
  drawing$col <- col
  drawing$pch <- pch
  drawing$points <- data.frame(x=data[, x], y=data[, y])
#   plot(data[, x], data[, y], xlab=x, ylab=y, xlim=panel_min_max$xlim, ylim=panel_min_max$ylim)
  if (!is.factor(data[, x]) && !is.factor(data[, y])) {
    drawing$lm <- lm(data[, y] ~ data[, x])
  }
  if (!is.factor(data[, x]) && is.factor(data[, y])) {
    drawing$ellipse <- compute_ellipse_points(x, y, data)
  }
  return(drawing)
}

# draw_one_panel("Sepal.Length", "Petal.Width", iris)
# draw_one_panel("Sepal.Length", "Petal.Width", iris, want_regression=TRUE)
# draw_one_panel("Sepal.Length", "Petal.Width", iris, want_ellipse=TRUE)

show <- function(drawing, as_new_plot=TRUE, ...) {
  if (as_new_plot) {
    plot(drawing$points, type="n", xlim=drawing$xlim, ylim=drawing$ylim,
         xlab=drawing$xlab, ylab=drawing$ylab, col=drawing$col, pch=drawing$pch, ...)
  }
  points(drawing$points, col=drawing$col, pch=drawing$pch, ...)
  if (!is.null(drawing$lm)) {
    abline(drawing$lm, col=drawing$col, ...)
  }
  if (!is.null(drawing$ellipse)) {
    lines(drawing$ellipse, col=drawing$col, ...)
  }
}

get_label <- function(all_drawings) {
  return(list(
    xlab=paste(unique(sapply(all_drawings, function(d) { d$xlab })), collapse=", "),
    ylab=paste(unique(sapply(all_drawings, function(d) { d$ylab })), collapse=", ")))
}

show_multiple_drawings <- function(all_drawings) {
  if (NROW(all_drawings) > 0) {
    browser()
    all_xlim <- sapply(all_drawings, function(elem) { elem$xlim })
    all_ylim <- sapply(all_drawings, function(elem) { elem$ylim })
    labels <- get_label(all_drawings)
    plot(all_drawings[[1]]$points, type="n", 
         xlim=c(min(all_xlim), max(all_xlim)),
         ylim=c(min(all_ylim), max(all_ylim)),
         xlab=labels$xlab,
         ylab=labels$ylab)
    for (i in seq_along(all_drawings)) {
      show(all_drawings[[i]], as_new_plot=FALSE)
    }
    legend("topleft", sapply(all_drawings, function(d) { d$xlab }),
           col=seq_along(all_drawings) + 1, pch=seq_along(all_drawings) + 1) 
  }
}

qviz <- function(formula, data, force_regression=FALSE, ...) {
#   par(ask=TRUE)
  require(formula.tools)
  require(foreach)
  lvars <- lhs.vars(formula)
  rvars <- rhs.vars(formula)
  for (lv in lvars) {
    i = 1
    all_drawings <- list()
    for (rv in rvars) {
      if (!is.factor(data[, lv]) && !is.factor(data[, rv])) {
        # Draw scatter plot of lv ~ rv and regression.
        drawing <- draw(rv, lv, data, want_regression=!is.factor(data[, lv]), col=(i + 1), pch=(i + 1))
        show(drawing)
        all_drawings[[i]] <- drawing
      } else if (is.factor(data[, lv]) && !is.factor(data[, rv])) {
        # Draw boxplot, and data points over it.
        boxplot(as.formula(paste(rv, "~", lv)), data=data, ylab=rv)
        col <- rgb(.3, .3, 1, 0.4)
        points(jitter(as.numeric(data[, lv])), jitter(data[, rv]), col=col, bg=col, cex=0.5, pch=21)
      } else {
        # Rely on R's plot().
        plot(as.formula(paste(lv, "~", rv)), data=data)
      }
      i <- i + 1
    }
    show_multiple_drawings(all_drawings)
#     idx <- 1
#     if (NROW(rvars) >= 2) {
#       for (i in 1:(NROW(rvars) - 1)) {
#         for (j in (i + 1):NROW(rvars)) {
#           rv1 = rvars[j] # Keep x axis the same while changing y
#           rv2 = rvars[i]
#           if (is.factor(data[, lv]) && !is.factor(data[, rv1]) && !is.factor(data[, rv2])) {
#             all_drawings[[idx]] <- draw(rvars[j], rvars[i], data, want_ellipse=TRUE)
#             idx <- idx + 1
#           } else if (!is.factor(data[, lv]) && !is.factor(data[, rv1]) && !is.factor(data[, rv2])) {
#             all_drawings[[idx]] <- draw(rvars[j], rvars[i], data, want_regression=TRUE)
#             idx <- idx + 1
#           }
#         }
#       }
#     }
  }
}
qviz(Species ~ Sepal.Length, data=iris)
qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)



rvars <- c("Sepal.Length", "Petal.Length", "Petal.Width")
for(i in 1:(NROW(rvars) - 1)) {
  for (j in (i + 1):NROW(rvars)) {
    print(rvars[i])
    print(rvars[j])
    print("-------")
    show(draw(rvars[j], rvars[i], iris, want_ellipse=TRUE))
  }
}

qviz(Species  ~ Sepal.Width + Petal.Length + Sepal.Length, data=iris)

qviz(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris, force_regression=TRUE)


qviz(Sepal.Length  + Petal.Width~ Sepal.Width + Petal.Length , data=iris)



qviz(Sepal.Width + Petal.Length ~ Species , data=iris)


library(formula.tools)
data(iris)

plot(Species ~ ., data=iris)
with(iris, plot(Species + Sepal.Width ~ Sepal.Length))

hist(Spec)

with(iris, plot(Sepal.Width ~ Sepal.Length + Petal.Width))

with(iris, plot(Sepal.Width ~ Sepal.Length))


form <- Sepal.Width ~ Sepal.Length + Petal.Width
class(form)
model.matrix(Sepal.Width ~ Sepal.Length + Species, data=iris)



all.names(Sepal.Width ~ Sepal.Length * Petal.Width + Petal.Length)