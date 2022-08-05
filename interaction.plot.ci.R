#' @Title
#' Gráfico de interação bidirecional (dois fatores)
#' 
#' @description 
#' Cria um gráfico de interação bidirecional, comumente usado para
#' Análise de Variância (ANOVA) de dois fatores.
#' 
#' @param data data.frame com o conjunto de dados estudado.
#' @param x string com o nome da coluna para o fator 1.
#' @param y string com o nome da coluna da variável dependente.
#' @param color string com o nome da coluna para o fator 2.
#'
#' @author Rubens Oliveira da Cunha Júnior (cunhajunior.rubens@gmail.com).
#' 
#' @examples
#' data(CO2)
#' interaction.plot.ci(
#'  data = CO2,
#'  x = 'Treatment',
#'  y = 'uptake',
#'  color = 'Type'
#' )
interaction.plot.ci <- function(data, x, y, color) {

  # mean and confidence interval 
  df <- with(data, aggregate(data[[y]], list(data[[color]], data[[x]]), mean))
  df$se <- with(data, aggregate(
    data[[y]], list(data[[color]], data[[x]]), function(x) {
      qt(0.95, df = length(x)-1)*sd(x) / sqrt(length(x))
      }))[,3]
  colnames(df) <- c(color, x, 'mean', 'ci')
  df <- df[order(df[[color]]), ]
  
  n.fac <- length(unique(data[[color]]))
  n.x <- length(unique(data[[x]]))
  
  df.bp.mean <- matrix(df$mean, nrow = n.fac, byrow = T)
  df.bp.ci <- matrix(df$ci, nrow = n.fac, byrow = T)
  colnames(df.bp.mean) <- unique(df[[x]])
  rownames(df.bp.mean) <- unique(df[[color]])
  
  # plot
  bp <- barplot(df.bp.mean,
                beside = T,
                density = 0,
                border = NA,
                xlab = x,
                ylab = y, 
                ylim = c(0.8 * (min(df$mean - df$ci)),
                         1.2 * (max(df$mean + df$ci))))

  x.coord <- apply(bp, 2, mean)
  color.names <- palette.colors(n = n.fac, palette = "Set 1")
  
  points(x = bp, y = df.bp.mean, pch = 16, col = color.names)
  box()
  axis(side = 1, at = x.coord, labels = rep('', n.x), las = 2)
  
  # ci arrows
  arrows(x0 = as.numeric(paste(bp, sep = "")),
         x1= as.numeric(paste(bp, sep = "")),
         y0 = df.bp.mean - df.bp.ci,
         y1 = df.bp.mean + df.bp.ci,
         col = matrix(rep(color.names, each = n.x), nrow = n.fac, byrow = T),
         code = 3, angle = 90, length = 0.025)
  
  # lines
  for (i in 1:nrow(bp)) {
    lines(x = bp[i,],
          y = df.bp.mean[i,],
          col = color.names[i],
          lty = i)
  }
  
  # legend
  legend("top", legend = rownames(df.bp.mean), col = color.names,
         lty = c(1:n.fac), xpd = T, horiz = T, title = color, lwd = 1,
         pch = rep(16, n.fac), bty = "n", inset = -0.3)
}