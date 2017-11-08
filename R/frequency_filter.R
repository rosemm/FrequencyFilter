#' @export
frequency_filter <- function(data_input, log=TRUE, base = exp(1), min.types=200, drop.zeros=log, outlier_cutoff=1, debug = FALSE){
  if(debug) message(unique(data_input$id))
  
  # For each word, get sum of chi counts and adu counts.
  # Note that in most cases, this will already be accomplished, 
  # but words that have more than one gloss used at different points in the transcription
  # may have separate counts. This ensures that cases like that are properly collapsed.
  data <- data_input %>% 
    dplyr::select(word, chi.count, adu.count) %>% 
    group_by(word) %>%
    summarise(chi.count = sum(chi.count), 
              adu.count = sum(adu.count))
  
  if(drop.zeros){
    # only keep observations with freq of at least 1 for both chi and adu
    data <- data %>% 
      dplyr::filter(chi.count > 0 & adu.count > 0) 
  }

  class(data) <- "data.frame"

  row.names(data) <- data$word

  # to log transform the counts before calculating the correlation
  if(log){ 
    if(drop.zeros){
      data$chi.count <- log(data$chi.count, base = base)
      data$adu.count <- log(data$adu.count, base = base)
    } else {
      # if zeros are NOT to be dropped, then use log(x + 1) instead of log(x)
      data$chi.count <- log1p(data$chi.count)
      data$adu.count <- log1p(data$adu.count)
    }
  }
  
  if(nrow(data) > min.types){

    r <- cor(data$chi.count, data$adu.count, method = "pearson")

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
    preds <- predict(model)

    cis <- predict.lm(model, interval = "confidence", level = 0.95)

    lm <- data.frame(word=names(resids), #
                     b0.est=b0.est,
                     b1.est=b1.est,
                     b0.se=b0.se,
                     b1.se=b1.se,
                     df.res=df.res,
                     r=r,
                     Rsq=Rsq,
                     pred=preds,
                     resid=resids,
                     zresid = zresids, stringsAsFactors = FALSE)
    lm <- cbind(lm, cis)
  } else {
    lm <- data.frame(word=NA_character_,
                     b0.est=NA,
                     b1.est=NA,
                     b0.se=NA,
                     b1.se=NA,
                     df.res=NA,
                     r=NA,
                     Rsq=NA,
                     pred=NA,
                     resid=NA,
                     zresid=NA,
                     fit=NA,
                     lwr=NA,
                     upr=NA, stringsAsFactors = FALSE)
  }
  data <- dplyr::left_join(data_input, lm, by="word")
  
  # define outliers
  data$zoutlier <- with(data, 
                        ifelse(zresid < - outlier_cutoff, "low",
                               ifelse(zresid > outlier_cutoff, "high",
                                      ifelse(zresid >= -outlier_cutoff & zresid <= outlier_cutoff, "no",
                                             NA))))
  return(data)
}
