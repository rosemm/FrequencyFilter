#' @export
bin_ages <- function(data, breaks){
# define a function that pools transcripts for a given child by approximate age
  
  data$age.bin <- cut(data$age.mos, breaks=breaks, labels=FALSE) # break age into bins
  
  age.bins <- data %>%
    group_by(age.bin) %>%
    # age is the average age for this child's files within that window
    dplyr::summarize(age=round(mean(age.mos, na.rm=TRUE), 0))
  
  data <- left_join(data, age.bins, by="age.bin")

  data$age.bin <- NULL # drop that column

  # pool all of the transcript counts within each age bin for each child
  # sum counts for each word in chi and adu speech
  pooled_word_counts <- data %>%
    group_by(child, age, word, gloss, POS) %>% # note that if mor was not use in count_words(), POS will just be NA
    summarize(chi.count=sum(chi.count, na.rm = TRUE),
              adu.count=sum(adu.count, na.rm = TRUE)) 
  # get totals for chi and adu utterances from all files in age bin
  pooled_utt_counts <- data %>%
    dplyr::select(child, age, file, N.chi.utts, N.adu.utts) %>% 
    unique() %>% 
    group_by(child, age) %>% 
    summarize(N.chi.utts=sum(N.chi.utts, na.rm = TRUE),
              N.adu.utts=sum(N.adu.utts, na.rm = TRUE)) 
  
  data %>% 
    dplyr::select(child, age, word, gloss, POS) %>% 
    unique() %>% 
    left_join(pooled_word_counts, by = c("child", "age", "word", "gloss", "POS")) %>% 
    left_join(pooled_utt_counts, by = c("child", "age"))
} 
