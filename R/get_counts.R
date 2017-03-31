
get_counts <- function(this.transcript, word.list = NULL, use.mor=FALSE, debug=FALSE, POS_regex=NULL, meta.data = NULL){
  
  stopifnot(length(unique(this.transcript$file)) == 1)
  if(debug) message("processing... ",unique(this.transcript$file))
  
  # get child utterances
  if(!use.mor) chi.utts <- dplyr::filter(this.transcript, role=="Target_Child")$utterance
  if(use.mor)  chi.utts <- dplyr::filter(this.transcript, role=="Target_Child")$mor
  
  if(length(chi.utts) == 0){ # if there were no hits for "Target_Child" in role, then look in name
    this.transcript$role <- ifelse(this.transcript$name=="Target_Child", "Target_Child", this.transcript$role) # copy over name information into role for Target_Child
    
    if(!use.mor) chi.utts <- dplyr::filter(this.transcript, role=="Target_Child")$utterance
    if(use.mor)  chi.utts <- dplyr::filter(this.transcript, role=="Target_Child")$mor
    
    if(length(chi.utts) == 0) message(paste0("WARNING: No utterances by Target_Child found in ", unique(this.transcript$file), ". Check the headers in your .cha files to make sure they're formatted correctly."))
  } # end if statement
  if(length(chi.utts) > 0){
    # collapse the child utterances into one long string of words
    chi.stream <- strsplit(paste(chi.utts, collapse=" "), split=" ")[[1]]
    chi.stream <- chi.stream[grep(x=chi.stream, pattern="[[:alpha:]]+")] # only keep words that have at least one letter (mostly just drops blanks)
  } else chi.stream <- NULL
  
  # get utterances from whichever adult (non-Target_Child) speaks most in this transcript
  temp <- this.transcript %>%
    count(role) # the number of utterances per speaker in this transcript
  
  # if there are speakers in the file other than Target_Child, assign the adult to count
  if(nrow(dplyr::filter(temp, role != "Target_Child")) > 0){
    adu.role <- dplyr::filter(temp, role != "Target_Child")$role[which.max(dplyr::filter(temp, role != "Target_Child")$n)] # which adult spoke the most?
    
    if(!use.mor) adu.utts <- dplyr::filter(this.transcript, role==adu.role)$utterance
    if(use.mor){
      if(all(is.na(dplyr::filter(this.transcript, role==adu.role)$mor))){
        # if there are no entries for mor for adu.role
        adu.utts <- NULL
      } else {
        adu.utts <- dplyr::filter(this.transcript, role==adu.role)$mor
      }
    }
  } else adu.utts <- NULL
  
  if(length(adu.utts) > 0){
    # collapse the child utterances into one long string of words
    adu.stream <- strsplit(paste(adu.utts, collapse=" "), split=" ")[[1]]
    adu.stream <- adu.stream[grep(x=adu.stream, pattern="[[:alpha:]]+")] # only keep words that have at least one letter (mostly just drops blanks)
  } else adu.stream <- NULL
  
  # how many utterances for chi and adu?
  N.chi.utts <- ifelse(length(chi.utts) > 0, temp[temp$role=="Target_Child", ]$n, 0)
  N.adu.utts <- ifelse(length(adu.utts) > 0, temp[temp$role==adu.role, ]$n, 0)
  
  # check for words from word list in child and adult speech
  # first generate word.list from chi.stream and adu.stream if there is no word.list provided
  if(is.null(word.list) & length(chi.utts) > 0 & length(adu.utts) > 0){
    words <- unique(c(chi.stream, adu.stream)) # all words from both child's and adult's production
    word.list <- data.frame(word = words)
    if(use.mor){
      word.list <- word.list %>%
        # pull out pos tags and root forms (orth_words)
        tidyr::separate(col = word,
                 into = c("POS", "orth_word"),
                 sep = "[|]",
                 remove = FALSE,
                 extra = "merge") %>% 
        # remove prefixes from in front of pos tags
        tidyr::separate(col = POS,
                 into = c("prefix", "POS"),
                 sep = "#",
                 remove = TRUE,
                 extra = "merge",
                 fill = "left") %>% 
        # stick prefixes back on orth_words
        tidyr::unite(col = "orth_word", prefix, orth_word, 
              sep = "", 
              remove = TRUE) %>% 
        dplyr::mutate(orth_word = gsub(x=orth_word, pattern = "^NA(.*)", replacement = "\\1"),
                      orth_word = gsub(x=orth_word, pattern = "[+][[:alpha:]]+[|]", replacement = ""))
      
      if(!is.null(POS_regex)){
        # if targetting certain POS (or categories of POS), use POS_regex to select sets of POS tags
        
        word.list$POS_cat <- word.list$POS
        for(i in 1:length(POS_regex)){
          # replace POS tags from POS_regex with the names from POS_regex (allows grouping of similar POSs, like all nouns, all verbs, etc.)
          word.list$POS_cat <- gsub(x=word.list$POS_cat, pattern = POS_regex[[i]], replacement = names(POS_regex[i]))
        }
        
        # reconstruct word from orth_word using POS_cat instead of POS, to allow for grouping of similar POSs
        word.list <- word.list %>%
          dplyr::select(word, POS_cat, orth_word) %>% 
          # pull out pos tags and root forms (orth_words)
          tidyr::separate(col = word,
                          into = c("POS", "word_temp"),
                          sep = "[|]",
                          remove = FALSE,
                          extra = "merge") %>% 
          # separate prefixes from pos tags
          tidyr::separate(col = POS,
                          into = c("prefix", "POS"),
                          sep = "#",
                          remove = TRUE,
                          extra = "merge",
                          fill = "left") %>% 
          # recompile prefixes with POS_cat
          tidyr::unite(col = "POS_temp", prefix, POS_cat, 
                       sep = "#", 
                       remove = FALSE) %>%
          dplyr::select(-prefix) %>% 
          # recompile words with POS_temp
          tidyr::unite(col = "pos_word", POS_temp, word_temp, 
                       sep = "|", 
                       remove = TRUE) %>%
          # remove empty (NA) prefixes
          dplyr::mutate(pos_word = gsub(x=pos_word, pattern = "^NA#(.*)", replacement = "\\1")) %>% 
          # only keep POSs in POS_regex
          dplyr::filter(POS_cat %in% names(POS_regex)) %>% 
          dplyr::select(word, pos_word, orth_word, POS, POS_cat) # put columns back in original order
      }
    } 
  }
  
  # for swedish language, need to replace "vår", "våran", "vårt", "våra" with other characters, because dplyr chokes on it
  if(unique(this.transcript$language) == "swedish"){
    chi.stream <- gsub(x=chi.stream, pattern="^vår([ta]*n*)", replacement = "voor\\1")
    adu.stream <- gsub(x=adu.stream, pattern="^vår([ta]*n*)", replacement = "voor\\1")
    word.list$word <- gsub(x=word.list$word, pattern="^vår([ta]*n*)", replacement = "voor\\1")
  }
  
  if( length(chi.utts)*length(adu.utts) > 0 | !use.mor ){
    
    if(!is.null(POS_regex)){
      # if focusing on particular POSs, use POS categories from POS_regex for chi.stream and adu.stream
      chi.stream.temp <- data.frame(word = chi.stream) %>% 
        left_join(dplyr::select(word.list, word, pos_word), by="word") %>% 
        na.omit() # drops all words not in word.list
      chi.stream <- chi.stream.temp$pos_word # use pos_word instead of word
      
      adu.stream.temp <- data.frame(word = adu.stream) %>% 
        left_join(dplyr::select(word.list, word, pos_word), by="word") %>% 
        na.omit() # drops all words not in word.list
      adu.stream <- adu.stream.temp$pos_word # use pos_word instead of word
      
      # replace word column in word.list with pos_word since both are no longer needed
      word.list$word <- word.list$pos_word
      word.list$pos_word <- NULL
      word.list$POS <- word.list$POS_cat
      word.list$POS_cat <- NULL
      word.list <- unique(word.list)
    } else {
      # if is.null(POS_regex), just use all words as-is in word.list$word
      chi.stream <- chi.stream[chi.stream %in% word.list$word]
      adu.stream <- adu.stream[adu.stream %in% word.list$word]
    }
    
    chi.count <- data.frame(word=chi.stream) %>% 
      dplyr::group_by(word) %>% 
      dplyr::summarize(chi.count = n())
    adu.count <- data.frame(word=adu.stream) %>% 
      dplyr::group_by(word) %>% 
      dplyr::summarize(adu.count = n())
    this.count <- word.list %>% 
      dplyr::left_join(chi.count, by = "word") %>%
      dplyr::left_join(adu.count, by = "word") %>% 
      dplyr::ungroup()
  } # end if statement checking if there are both chi and adu utts (or use.mor is FALSE)
  
  if(length(chi.utts)*length(adu.utts) == 0 & use.mor){
    this.count <- data.frame(word=NA, POS=NA, orth_word=NA, chi.count=0, adu.count=0)
  } 
  
  this.count$file <- unique(this.transcript$file)
  this.count$N.chi.utts <- N.chi.utts
  this.count$N.adu.utts <- N.adu.utts
  if(length(adu.utts) > 0) {
    this.count$adu.role <- adu.role
  } else {
    this.count$adu.role <- NA
  }
  
  return(this.count)
}
