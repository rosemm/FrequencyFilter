#' @export
frequency_filter <- function(data, log=FALSE, base = exp(1), min.types=5, drop.zeros=log, debug = FALSE){
  if(drop.zeros){
    # only keep observations with freq of at least 1 for both chi and adu
    data <- filter(data, chi.count > 0 & adu.count > 0)
  }

  if(debug) message(unique(data$id))

  class(data) <- "data.frame"

  row.names(data) <- data$word

  if(log){ # to log transform the counts before calculating the correlation
    data$chi.count <- log(data$chi.count, base = base)
    data$adu.count <- log(data$adu.count, base = base)
  }
  if(nrow(data) > min.types){

    r <- cor(chi.count ~ adu.count, data=data, method = "pearson")

    # calculate regression coefs
    model <- lm(chi.count ~ adu.count, data=data)

    b0.est <- summary(model)$coef[1,1]
    b1.est <- summary(model)$coef[2,1]
    b0.se <- summary(model)$coef[1,2]
    b1.se <- summary(model)$coef[2,2]
    df.res <- summary(model)$df[2]
    Rsq <- summary(model)$r.squared

    resids <- resid(model)
    zresids <- scale(resids)

    cis <- predict.lm(model, interval = "confidence", level = 0.95)

    lm <- data.frame(word=names(resids), #
                     b0.est=b0.est,
                     b1.est=b1.est,
                     b0.se=b0.se,
                     b1.se=b1.se,
                     df.res=df.res,
                     r=r,
                     Rsq=Rsq,
                     resid=resids,
                     zresid = zresids)
    lm <- cbind(lm, cis)
  } else {
    lm <- data.frame(word=NA,
                     b0.est=NA,
                     b1.est=NA,
                     b0.se=NA,
                     b1.se=NA,
                     df.res=NA,
                     r=NA,
                     Rsq=NA,
                     resid=NA,
                     zresid=NA,
                     fit=NA,
                     lwr=NA,
                     upr=NA)
  }
  return(lm)
}
