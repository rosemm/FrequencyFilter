#' @export
get_counts <- function(this.transcript, word.list = NULL, use.mor = TRUE, debug=FALSE, POS_regex=NULL){
  stopifnot(length(unique(this.transcript$file)) == 1)
  if(debug) message("processing... ", unique(this.transcript$file))
  
  if(use.mor & length(na.omit(this.transcript$mor)) == 0){
    message("No mor information available for ", unique(this.transcript$file), ". Setting use.mor = FALSE")
    use.mor <- FALSE
  }
  
  # get child utterances
  chi.utts.orth <- na.omit(dplyr::filter(this.transcript, role=="Target_Child")$utterance)
  chi.utts.mor <- na.omit(dplyr::filter(this.transcript, role=="Target_Child")$mor)
  if(use.mor) {
    chi.utts <- chi.utts.mor
  } else {
    chi.utts <- chi.utts.orth
  }

  if(length(chi.utts) == 0){ # if there were no hits for "Target_Child" in role, then look in name
    this.transcript$role <- ifelse(this.transcript$name=="Target_Child", "Target_Child", this.transcript$role) # copy over name information into role for Target_Child

    chi.utts.orth <- na.omit(dplyr::filter(this.transcript, role=="Target_Child")$utterance)
    chi.utts.mor <- na.omit(dplyr::filter(this.transcript, role=="Target_Child")$mor)
    if(use.mor) {
      chi.utts <- chi.utts.mor
    } else {
      chi.utts <- chi.utts.orth
    }
    
    if(length(chi.utts) == 0) message(paste0("WARNING: No utterances by Target_Child found in ", unique(this.transcript$file), ". Check the headers in your .cha files to make sure they're formatted correctly."))
  } # end if statement

  if(length(chi.utts) > 0){
    # collapse the child utterances into one long string of words
    chi.stream <- strsplit(paste(na.omit(chi.utts), collapse=" "), split=" ")[[1]]
    chi.stream <- chi.stream[grep(x=chi.stream, pattern="[[:alpha:]]+")] # only keep words that have at least one letter (drops blanks and punct notation)
  } else chi.stream <- NULL

  # get utterances from whichever adult (non-Target_Child) speaks most in this transcript
  temp <- this.transcript %>%
    count(role) # the number of utterances per speaker in this transcript

  # if there are speakers in the file other than Target_Child, select the adult to count
  if(nrow(dplyr::filter(temp, role != "Target_Child")) > 0){
    adu.role <- dplyr::filter(temp, role != "Target_Child")$role[which.max(dplyr::filter(temp, role != "Target_Child")$n)] # which adult spoke the most?

    if(!use.mor) adu.utts <- na.omit(dplyr::filter(this.transcript, role==adu.role)$utterance)
    if(use.mor)  adu.utts <- na.omit(dplyr::filter(this.transcript, role==adu.role)$mor)
      
  } else adu.utts <- NULL
  
  # how many utterances for chi and adu?
  N.chi.utts <- ifelse(length(chi.utts) > 0, temp[temp$role=="Target_Child", ]$n, 0)
  N.adu.utts <- ifelse(length(adu.utts) > 0, temp[temp$role==adu.role, ]$n, 0)

  if(N.adu.utts > 0){
    # collapse the adult utterances into one long string of words
    adu.stream <- strsplit(paste(na.omit(adu.utts), collapse=" "), split=" ")[[1]]
    adu.stream <- adu.stream[grep(x=adu.stream, pattern="[[:alpha:]]+")] # only keep words that have at least one letter (just drops blanks and punct notation)
  } else adu.stream <- NULL
  
  # If BOTH chi utts and adu utts are present, get word counts.
  # this limits analysis to files that contain an interaction (omits monologues or usual CHAT files that transcribe only child speech, for example)
  if(N.chi.utts*N.adu.utts > 0) {
    # check for words from word list in child and adult speech
    # first generate word.list from chi.stream and adu.stream if there is no word.list provided
    if(is.null(word.list)){
      words <- unique(c(chi.stream, adu.stream)) # all words from both child's and adult's production
      word.list <- data.frame(word = words, stringsAsFactors = FALSE) %>%
        as.tbl()  # for processing speed
      
      if(use.mor){ # Note that if !use.mor, then chi.stream and adu.stream are just the words as transcribed (orthographic). No need to extract info.

        word.list$POS <- sapply(X=word.list$word, FUN = parse_mor_word, extract = "POS")
        word.list$root_word <- sapply(X=word.list$word, FUN = parse_mor_word, extract = "word")
        
        if(!is.null(POS_regex)){
          # if targetting certain POS (or categories of POS), use POS_regex to select sets of POS tags
          word.list$POS_cat <- word.list$POS
          for(i in 1:length(POS_regex)){
            # replace POS tags from POS_regex with the names from POS_regex (allows grouping of similar POSs, like all nouns, all verbs, etc.)
            word.list$POS_cat <- gsub(x=word.list$POS_cat, pattern = POS_regex[[i]], replacement = names(POS_regex[i]))
          }
          
          # reconstruct word from root_word using POS_cat instead of POS, to allow for grouping of similar POSs
          word.list <- word.list %>%
            # paste POS_cat onto root_word to generate POS_word
            tidyr::unite(POS_word, POS_cat, root_word, sep = "|", remove = FALSE) %>% 
            dplyr::select(word, POS_word, root_word, POS, POS_cat) # put columns back in original order
        }
      } # end if statement use.mor
      
      if(!use.mor){
        # add in the missing columns, so it will line up the same was as files that do have mor
        word.list <- word.list %>% 
          mutate(root_word = word, POS = NA_character_) %>% 
          dplyr::select(word, root_word, POS)
      }
    } # end if statement is.null(word.list)
    
    # for swedish language, need to replace "vår", "våran", "vårt", "våra" with other characters, because dplyr::left_join chokes on it 
    if(tolower(unique(this.transcript$language)) == "swedish"){
      chi.stream <- gsub(x=chi.stream, pattern="(.*)å(.*)", replacement = "\\1oo\\2")
      adu.stream <- gsub(x=adu.stream, pattern="(.*)å(.*)", replacement = "\\1oo\\2")
      word.list$word <- gsub(x=word.list$word, pattern="(.*)å(.*)", replacement = "\\1oo\\2")
    }
    
    if(!is.null(POS_regex)){
      # if focusing on particular POSs, use POS categories from POS_regex for chi.stream and adu.stream
      chi.stream.temp <- data.frame(word = chi.stream) %>%
        left_join(dplyr::select(word.list, word, POS_word), by="word") %>%
        na.omit() # drops all words not in word.list
      chi.stream <- chi.stream.temp$POS_word # use POS_word instead of word
      
      adu.stream.temp <- data.frame(word = adu.stream) %>%
        left_join(dplyr::select(word.list, word, POS_word), by="word") %>%
        na.omit() # drops all words not in word.list
      adu.stream <- adu.stream.temp$POS_word # use POS_word instead of word
      
      # replace word column in word.list with POS_word since both are no longer needed
      word.list$word <- word.list$POS_word
      word.list$POS_word <- NULL
      word.list$POS <- word.list$POS_cat
      word.list$POS_cat <- NULL
      word.list <- unique(word.list)
    } else {
      # if is.null(POS_regex), just use all words as-is in word.list$word
      chi.stream <- chi.stream[chi.stream %in% word.list$word]
      adu.stream <- adu.stream[adu.stream %in% word.list$word]
    }
    
    # Count occurrences of each word
    chi.count <- data.frame(word=chi.stream, stringsAsFactors = FALSE) %>% 
      dplyr::group_by(word) %>%
      dplyr::summarize(chi.count = n())
    adu.count <- data.frame(word=adu.stream, stringsAsFactors = FALSE) %>% 
      dplyr::group_by(word) %>%
      dplyr::summarize(adu.count = n())
    this.count <- word.list %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(chi.count, by = "word") %>%
      dplyr::left_join(adu.count, by = "word") %>%
      # pull out the gloss (translation) for words that have it included
      tidyr::separate(word, into = c("word", "gloss"), sep = "=", remove = TRUE, fill = "right") %>%
      # words not observed in a given stream (chi or adu) get a count of 0 (instead of NA)
      dplyr::mutate(chi.count = ifelse(is.na(chi.count), 0, chi.count),
                    adu.count = ifelse(is.na(adu.count), 0, adu.count)) %>% 
      dplyr::select(word, root_word, POS, gloss, chi.count, adu.count)

  } # end if statement checking that there are both chi and adu utts available (N.chi.utts*N.adu.utts > 0)
  
  # If there are no utterances at all for chi or adu (or both), leave counts blank
  if( N.chi.utts*N.adu.utts == 0 ){
    this.count <- data.frame(word=NA, root_word=NA, POS=NA, gloss = NA, chi.count=0, adu.count=0)
  }

  this.count$file <- unique(this.transcript$file)
  this.count$N.chi.utts <- N.chi.utts
  this.count$N.adu.utts <- N.adu.utts
  if(N.adu.utts > 0) {
    this.count$adu.role <- adu.role
  } else {
    this.count$adu.role <- NA
  }
  # make sure character columns are classed as character, even if they're all empty (NA)
  this.count <- this.count %>% 
    dplyr::mutate_at(vars(word, root_word, POS, gloss, file, adu.role), as.character)

  return(this.count)
}
