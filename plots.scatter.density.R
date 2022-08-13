#' @Title
#' Gráfico de dispersão com densidade das distribuições marginais
#' 
#' @description 
#' Cria um gráfico de dispersão entre duas variáveis e mostra o gráfico
#' da função densidade das dsitribuições marginais.
#'
#' @param x vector.
#' @param y vcrtor.
#'
#' @author Rubens Oliveira da Cunha Júnior (cunhajunior.rubens@gmail.com).
#' 
#' @examples
#' x <- rexp(500)
#' y <- rnorm(500)
#' plot.scatter.density(x, y)
plot.scatter.density <- function(x, y) {
  
  # set layout
  layout(matrix(c(1,2,3,4), ncol=2, byrow=TRUE),
         widths=c(3,1),
         heights=c(1,2))
  
  # add plots
  par(mar = c(0,4,4,0) + 0.1)
  x.pdf <- density(x)
  plot(x = x.pdf$x,
       y = x.pdf$y,
       type = 'l',
       col = 'red',
       ann = F,
       axes = F)
  box()
  
  plot.new()
  
  par(mar = c(5,4,0,0) + 0.1)
  xy.plot <- plot(x, y)
  
  par(mar = c(5,0,0,2) + 0.1)
  y.pdf <- density(y)
  plot(x = y.pdf$y,
       y = y.pdf$x,
       type = 'l',
       col = 'red',
       ann = F,
       axes = F)
  box()
}