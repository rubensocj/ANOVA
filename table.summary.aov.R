#' @Title
#' Tabela com os resultados da Análise de Variância (ANOVA)
#' 
#' @description 
#' Cria uma tabela com os resultados da ANOVA.
#' 
#' @details 
#' Converte o resultado da função summary.aov em um objeto da classe data.frame.
#' 
#' @param model objeto da classe aov obtido da ANOVA.
#'
#' @return data.frame com o resultado da ANOVA.
#'
#' @author Rubens Oliveira da Cunha Júnior (cunhajunior.rubens@gmail.com).
#' 
#' @examples
#' data(iris)
#' mod <- aov(Sepal.Length ~ Species, data = iris)
#' tb <- table.summary.aov(mod)
table.summary.aov <- function(model) {
  suppressWarnings(if(is.na(match(class(model), c("aov")))) {
    stop("Invalid argument type: 'model' must be of class: 'aov'")
  })
  
  # results of ANOVA
  res <- rbind.data.frame(summary.aov(model))
  colnames(res) <- c('Df','Sum Sq','Mean Sq','F value','Pr(>F)')
  
  # source of variation
  src <- c(attributes(model$terms)$term.labels, "Residuals")
  tb <- cbind("Source of variation" = src, res)
  
  # signif. codes
  sig.cod <- c('***','**','*','.',' ')
  sig <- rep('', nrow(tb))
  sig[res$`Pr(>F)` >= 0 & res$`Pr(>F)` <= 0.001] <- sig.cod[1]
  sig[res$`Pr(>F)` > 0.001 & res$`Pr(>F)` <= 0.01] <- sig.cod[2]
  sig[res$`Pr(>F)` > 0.01 & res$`Pr(>F)` <= 0.05] <- sig.cod[3]
  sig[res$`Pr(>F)` > 0.05 & res$`Pr(>F)` <= 0.1] <- sig.cod[4]
  sig[res$`Pr(>F)` > 0.1 & res$`Pr(>F)` <= 1] <- sig.cod[5]
  
  # format
  tb[ , 3:5] <- round(tb[ , 3:5], 2)
  tb$`F value` <- as.character(tb$`F value`)
  tb$`F value`[is.na(tb$`F value`)] <- ''
  tb$`Pr(>F)` <- format(tb$`Pr(>F)`, scientific = T, digits = 3)
  tb$`Pr(>F)` <- as.character(tb$`Pr(>F)`)
  tb$`Pr(>F)` <- gsub('NA','',tb$`Pr(>F)`)
  tb$`Pr(>F)` <- paste0(tb$`Pr(>F)`, ' ', sig)
  
  return(tb)
}
