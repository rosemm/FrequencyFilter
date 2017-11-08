#' Residualizes child counts by regressing on adult counts for the same words
#'
#' Residualizes child word counts by regressing on counts for the main adult interlocotur for the same words in the same transcripts.
#' The input data should already be word counts by speaker for each transcript or timepoint. 
#' By default, counts are log transformed before regressing.
#'
#' @param data_input the dataframe of counts in child and adult speech
#' @param chi.count the name of the column with child counts
#' @param adu.count the name of the column with adult counts
#' @param min.types the minimum number of types necessary to run regression at a given timepoint or transcript. If set to 0, it will attempt to fit a regression for every timepoint/transcript regardless of how many words are available. Note that it is inadvisable to use residuals from regressions fit at very sparse timepoints.
#' @param log log transform counts before regressing? Default is TRUE.
#' @param base the base to use for log transformation
#' @param drop.zeros should zero-count words (words that never occur in adult speech, or never in child speech) be excluded? Default is TRUE, which restricts analysis to counts of words that are observed in both child and adult speech.
#' @param outlier_cutoff how should outliers (exceptionally high or low residuals) be defined? Default is 1 SD. 
#' @param debug turn on to display extra messages while the function runs, useful for identifying problem transcripts. Default is FALSE. 
#' 
#' @return The original input data, with new variables added on providing results from the regressions including residual values for each word. 
#'
#' @examples
#' frequency_filter(data_pos)
#' 
#' @references Ibbotson, Hartman, and Nilsson Björkenstam. (in prep). "Frequency Filter: An Open Access Tool for Analysing Language Development".
#' 
#' @export
frequency_filter <- function(data_input, chi.count = "chi.count", adu.count = "adu.count", min.types=200, log=TRUE, base = exp(1), drop.zeros=TRUE, outlier_cutoff=1, debug = FALSE){
  if(debug) message(unique(data_input$id))
  
  # for use in dplyr functions below (see vignette("programming") in the dplyr package)
  chi.count <- enquo(chi.count)
  adu.count <- enquo(adu.count)
  
  # For each word, get sum of chi counts and adu counts.
  # Note that in most cases, this will already be accomplished, 
  # but words that have more than one gloss used at different points in the transcription
  # may have separate counts. This ensures that cases like that are properly collapsed.
  data <- data_input %>% 
    dplyr::select(word, chi.count = !!chi.count, adu.count=!!adu.count) %>% 
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
