compute_ellipse_points <- function(x, y, data) {
  if (NROW(data) == 1) {
    # In this case, ellipse will return error. Instead, have it render a single point.
    return(data[, c(x, y)])
  } else if (NROW(data) == 0) {
    return(data.frame())
  }
  return(ellipse(var(data[, c(x, y)]),
                 centre=colMeans(data[, c(x, y)]),
                 level=0.95))
}

GetPanelMinMax <- function(x, y, data, want_ellipse) {
  if (NROW(data) == 0) {
    return(list(xlim=c(NA, NA), ylim=c(NA, NA)))
  }
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

# TODO: Instead of relying on list, create a list of classes and have each class
# render corresponding chart.
draw <- function(x, y, data, want_regression=FALSE, want_pca=FALSE, want_ellipse=FALSE, col=1, pch=1, ...) {
  drawing <- list()
  drawing$xlab <- x
  drawing$ylab <- y
  panel_min_max <- GetPanelMinMax(x, y, data, want_ellipse)
  drawing$xlim <- panel_min_max$xlim
  drawing$ylim <- panel_min_max$ylim
  drawing$col <- col
  drawing$pch <- pch
  drawing$points <- data.frame(x=data[, x], y=data[, y])
  if (want_regression) {
    drawing$lm <- lm(data[, y] ~ data[, x])
  }
  if (want_ellipse) {
    drawing$ellipse <- compute_ellipse_points(x, y, data)
  }
  return(drawing)
}


draw_scatter_plot_with_class_ellipse <- function(x, y, lv, data) {
  i <- 1
  all_drawings <- list()
  for (lvl in levels(data[, lv])) {
    all_drawings[[i]] <- draw(x, y, data[data[, lv] == lvl, ], 
                              want_ellipse=TRUE, col=(i + 1), pch=(i + 1))
    i <- i + 1
  }
  return(all_drawings)
}


show <- function(drawing, as_new_plot=TRUE, ...) {
  if (as_new_plot) {
    plot(drawing$points, type="n", xlim=drawing$xlim, ylim=drawing$ylim,
         xlab=drawing$xlab, ylab=drawing$ylab, col=drawing$col, pch=drawing$pch, ...)
  }
  if (NROW(drawing$points) > 0) {
    points(drawing$points, col=drawing$col, pch=drawing$pch, ...)
  }
  if (!is.null(drawing$lm)) {
    abline(drawing$lm, col=drawing$col, ...)
  }
  if (!is.null(drawing$ellipse) && NROW(drawing$ellipse) > 0) {
    lines(drawing$ellipse, col=drawing$col, ...)
  }
}

get_label <- function(all_drawings) {
  return(list(
    xlab=paste(unique(sapply(all_drawings, function(d) { d$xlab })), collapse=", "),
    ylab=paste(unique(sapply(all_drawings, function(d) { d$ylab })), collapse=", ")))
}

show_multiple_drawings <- function(all_drawings, legend=NULL, ...) {
  if (NROW(all_drawings) > 0) {
    new_plot_drawn = FALSE
    for (i in seq_along(all_drawings)) {
      if (new_plot_drawn == FALSE && NROW(all_drawings[[i]]$points) != 0) {
        all_xlim <- sapply(all_drawings, function(elem) { elem$xlim })
        all_ylim <- sapply(all_drawings, function(elem) { elem$ylim })
        labels <- get_label(all_drawings)
        plot(all_drawings[[1]]$points, type="n", 
             xlim=c(min(all_xlim, na.rm=TRUE), max(all_xlim, na.rm=TRUE)),
             ylim=c(min(all_ylim, na.rm=TRUE), max(all_ylim, na.rm=TRUE)),
             xlab=labels$xlab,
             ylab=labels$ylab,
             ...)
        new_plot_drawn = TRUE        
      }
      show(all_drawings[[i]], as_new_plot=FALSE, ...)
    }
    legend("topleft", 
           legend=if(is.null(legend)) sapply(all_drawings, function(d) { d$xlab }) else legend,
           col=seq_along(all_drawings) + 1, pch=seq_along(all_drawings) + 1) 
  }
}

get_factor_vars <- function(vars, data) {
  return(vars[sapply(vars, function(v) { is.factor(data[, v]) })])
}

get_numeric_vars <- function(vars, data) {
  return(vars[sapply(vars, function(v) { is.numeric(data[, v]) })])
}

qviz_numeric_vars <- function(vars, data, ...) {
  numeric_vars <- get_numeric_vars(vars, data)
  if (NROW(numeric_vars) == 1) {
    boxplot(data[, numeric_vars], horizontal=TRUE, ylab=numeric_vars[1], ...)
  } else if (NROW(numeric_vars) > 0) {
    boxplot(data[, numeric_vars], horizontal=TRUE, ...)
  }
}

# Generate title string to use for plot.
# "y ~ x (condition)" if condition is not empty string.
# "y ~ x", otherwise.
main_title <- function(y, x, condition) {
  return(paste0(y, " ~ ", x, ifelse(condition == "", "", paste0("\n(", condition, ")"))))
}

qviz_single_rhs <- function(lv, rvars, data, condition="", ...) {
  i = 1
  all_drawings <- list()
  ### For single rvar.
  for (rv in rvars) {
    if (!is.factor(data[, lv]) && !is.factor(data[, rv])) {
      # Draw scatter plot of lv ~ rv and regression.
      drawing <- draw(rv, lv, data, want_regression=!is.factor(data[, lv]), col=(i + 1), pch=(i + 1))
      show(drawing, main=main_title(lv, rv, condition), ...)
      all_drawings[[i]] <- drawing
      i <- i + 1
    } else if (is.factor(data[, lv]) && !is.factor(data[, rv])) {
      # Draw boxplot, and data points over it.
      # TODO: Change axis as it looks confusing.
      boxplot(as.formula(paste(rv, "~", lv)), data=data, horizontal=TRUE, xlab=rv, ylab=lv, 
              main=main_title(lv, rv, condition), ...)
      for (lvl_idx in seq(nlevels(data[, lv]))) {
        sub_data <- subset(data, data[, lv] == levels(data[, lv])[lvl_idx])
        points(jitter(sub_data[, rv]), jitter(as.numeric(sub_data[, lv])),
               col=(lvl_idx + 1), pch=(lvl_idx + 1), cex=0.7)
      }
      
      # TODO: Improve the plot for factor ~ factor
    } else {
      # Rely on R's plot().
      plot(as.formula(paste(lv, "~", rv)), data=data, main=main_title(lv, rv, condition), ...)
    }
  }
  if (NROW(all_drawings) > 1) {
    show_multiple_drawings(all_drawings, main=main_title(lv, rv, condition), ...)
  }
}

qviz_classification <- function(lv, numeric_rvars, data, condition="") {
  for (rvar_i in 1:(NROW(numeric_rvars) - 1)) {
    for (rvar_j in (rvar_i + 1):NROW(numeric_rvars)) {
      rv_x = numeric_rvars[rvar_j] # Keep y axis the same while changing x
      rv_y = numeric_rvars[rvar_i]
      show_multiple_drawings(draw_scatter_plot_with_class_ellipse(rv_x, rv_y, lv, data), 
                             legend=paste(levels(data[, lv])),
                             main=main_title(lv, paste0(rv_x, "+", rv_y), condition))
    }
  }
}

qviz_pca_classification <- function(lv, numeric_rvars, data) {
  pc <- princomp(data[, numeric_rvars])
  pca_data <- cbind(as.data.frame(predict(pc, newdata=data)[, 1:2]), 
                    data[, lv])
  names(pca_data) <- c("PC1", "PC2", lv)
  prop_of_variance <- (summary(pc)$sdev ^ 2) / sum(summary(pc)$sdev ^ 2)
  subtitle <-sprintf(paste0("PC1(Proportion of Variance: %.2f) and ",
                            "PC2(Proportion of Variance: %.2f)"), prop_of_variance[1], prop_of_variance[2])
  show_multiple_drawings(draw_scatter_plot_with_class_ellipse("PC1", "PC2", lv, pca_data),
                         legend=paste(levels(data[, lv])),
                         main=paste(lv, "~ PC1 + PC2\n",
                                    "PCA of", paste(numeric_rvars, collapse=", ")),
                         sub=subtitle)  
}

qviz_multiple_rhs <- function(lv, rvars, data) {
  numeric_rvars <- get_numeric_vars(rvars, data)
  # If this is classification problem.
  if (is.factor(data[, lv]) && NROW(numeric_rvars) >= 2) {
    qviz_classification(lv, numeric_rvars, data)
    if (NROW(numeric_rvars) >= 3) {
      qviz_pca_classification(lv, numeric_rvars, data)
    }
  }
  factor_rvars <- get_factor_vars(rvars, data)
  # For each level of factor, plot single variable and classification ellipse.
  if (NROW(numeric_rvars) > 0 && NROW(factor_rvars) > 0) {
    if (NROW(factor_rvars) > 0) {
      for (frv in factor_rvars) {
        for (lvl in levels(data[, frv])) {
          data_for_lvl <- data[data[, frv] == lvl, ]
          condition <- paste(frv, "==", lvl)
          qviz_single_rhs(lv, numeric_rvars, data_for_lvl, condition)
          if (is.factor(data[, lv]) && NROW(numeric_rvars) > 1) {
            qviz_classification(lv, numeric_rvars, data_for_lvl, condition)
          }
        }
      }
    }
  }
}

clean_data_types <- function(data, vars) {
  for (v in vars) {
    if (is.ordered(data[, v])) {
      data[, v] <- as.numeric(data[, v])
    } else if (is.logical(data[, v]) || is.character(data[, v])) {
      data[, v] <- as.factor(data[, v])
    }
  }
  return(data)
}

qviz <- function(formula, data, ...) {
  lvars <- lhs.vars(formula)
  rvars <- rhs.vars(formula)
  data <- clean_data_types(data, c(lvars, rvars))
  qviz_numeric_vars(lvars, data, main="Numeric Ys")
  qviz_numeric_vars(rvars, data, main="Numeric Xs")
  for (lv in lvars) {
    qviz_single_rhs(lv, rvars, data)
    qviz_multiple_rhs(lv, rvars, data)
  }
}
