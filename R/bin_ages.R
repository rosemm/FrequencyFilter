
bin_ages <- function(data, breaks, use.mor=FALSE){
  
  data$age.bin <- cut(data$age.mos, breaks=breaks, labels=FALSE) # break age into bins
  age.bins <- data %>%
    group_by(age.bin) %>%
    summarize(age=round(mean(age.mos, na.rm=TRUE), 0))
  data <- left_join(data, age.bins, by="age.bin")
  
  data$age.bin <- NULL # drop that column
  
  # pool all of the transcript counts within each age bin for each child
  if(!use.mor){
    data_grouped <- data %>%
      group_by(child, age, word)
  }
  if(use.mor){
    data_grouped <- data %>%
      group_by(child, age, orth_word, POS, word)
  }
  
  data <- data_grouped %>%
    summarize(chi.count=sum(chi.count, na.rm = TRUE),
              adu.count=sum(adu.count, na.rm = TRUE),
              N.chi.utts=sum(N.chi.utts, na.rm = TRUE),
              N.adu.utts=sum(N.adu.utts, na.rm = TRUE)) %>%
    unite(col="id", child, age, remove=FALSE)
  
  return(data)
} # define a function that pools transcripts for a given child by approximate age
