#' Count words in child and adult speech
#'
#' @export
count_words <- function(this.transcript, use.mor = TRUE, POS_regex=NULL, mor = "mor_word", orth = "orth_word", debug=FALSE){
  stopifnot(length(unique(this.transcript$file)) == 1)
  if(debug) message("processing... ", unique(this.transcript$file))
  
  # select columns to be used for mor_word and orth_word
  # (default is to use those names to begin with, in which case no change in column names occurs)
  colnames(this.transcript) <- gsub(x=colnames(this.transcript), pattern = mor,  replacement = "mor_word")
  colnames(this.transcript) <- gsub(x=colnames(this.transcript), pattern = orth, replacement = "orth_word")
  
  if(use.mor & length(na.omit(this.transcript$mor_word)) == 0){
    message("No mor information available for ", unique(this.transcript$file), ". Setting use.mor = FALSE")
    use.mor <- FALSE
  }
  
  # get number of utterances for each speaker in file to identify target child and adu interlocutor
  n.utts.by.speaker <- this.transcript %>% 
    ungroup() %>% 
    dplyr::select(speaker, name, role, utt.num) %>% 
    unique() %>% # drop multiple rows for a given utterance (more than one word in the utterance)
    count(speaker, name, role) 
  
  # identify target child
  if(nrow(dplyr::filter(n.utts.by.speaker, role == "Target_Child")) == 0){
    if(nrow(dplyr::filter(n.utts.by.speaker, name == "Target_Child")) > 0){
      # if there is no speaker with role "Target_Child" check name
      # if there is a speaker with name "Target_Child", overwrite the role for that speaker with "Target_Child"
      this.transcript <- this.transcript %>% 
        mutate(role = ifelse(name == "Target_Child", "Target_Child", role))
    } else if(nrow(dplyr::filter(n.utts.by.speaker, speaker == "CHI")) > 0){
      # if no speaker has "Target_Child" as name or role, look for CHI in speaker
      # if there is a speaker "CHI", overwrite the role for that speaker with "Target_Child"
      this.transcript <- this.transcript %>% 
        mutate(role = ifelse(speaker == "CHI", "Target_Child", role))
    }
  }
  chi.utts <- this.transcript %>% 
    dplyr::filter(role == "Target_Child")
  
  # identify adult interlocutor
  n.utts.by.speaker <- this.transcript %>% # recalculating this in case role got updated for Target_Child
    ungroup() %>% 
    dplyr::select(speaker, name, role, utt.num) %>% 
    unique() %>% # drop multiple rows for a given utterance (more than one word in the utterance)
    count(speaker, name, role)
  candidate.adu <- n.utts.by.speaker %>% 
    dplyr::filter(role != "Target_Child")
  
  if(nrow(candidate.adu) > 0){
    # which adult spoke the most?
    adu.role <- candidate.adu$role[which.max(candidate.adu$n)] 
    adu.utts <- this.transcript %>% 
      dplyr::filter(role == adu.role)
  } else {
    adu.role <- NULL
    adu.utts <- data.frame() # an empty data.frame
  }
  
  # how many utterances for chi and adu?
  N.chi.utts <- ifelse(nrow(chi.utts) > 0, n.utts.by.speaker[n.utts.by.speaker$role=="Target_Child", ]$n, 0)
  N.adu.utts <- ifelse(nrow(adu.utts) > 0, n.utts.by.speaker[n.utts.by.speaker$role==adu.role, ]$n, 0)
  
  if(N.chi.utts == 0) message(paste0("No utterances by Target_Child found in ", unique(this.transcript$file), ". Check the headers in your .cha files to make sure they're formatted correctly."))
  if(N.adu.utts == 0) message(paste0("No utterances by adult interlocutor found in ", unique(this.transcript$file), ". Check the headers in your .cha files to make sure they're formatted correctly."))
  
  # If BOTH chi utts and adu utts are present, get word counts.
  # this limits analysis to files that contain an interaction (omits monologues or unusual CHAT files that transcribe only child speech, for example)
  if(N.chi.utts*N.adu.utts > 0) {
    
    # select just the utterances by the Target Child and selected interlocutor
    this.words <- this.transcript %>% 
      dplyr::filter(role %in% c("Target_Child", adu.role)) %>% 
      ungroup() %>% 
      dplyr::mutate(role = ifelse(role == "Target_Child", "chi.count", "adu.count"))
    
    if(use.mor){
      # pull out POS information, optionally collapsing POS categories accoring to POS_regex
      
      unique.words <- this.words %>% 
        ungroup() %>% 
        dplyr::select(mor_word) %>% 
        unique()
      # parse mor_word into POS, word base, and gloss (when it's provided)
      unique.words$POS <- sapply(X=unique.words$mor_word, FUN = parse_mor_word, extract = "POS")
      unique.words$base_word <- sapply(X=unique.words$mor_word, FUN = parse_mor_word, extract = "word")
      unique.words$gloss <- sapply(X=unique.words$mor_word, FUN = parse_mor_word, extract = "gloss")
      
      if(!is.null(POS_regex)){
        # if targetting certain POS (or categories of POS), use POS_regex to select sets of POS tags
        for(i in 1:length(POS_regex)){
          # replace POS tags from POS_regex with the names from POS_regex (allows grouping of similar POSs, like all nouns, all verbs, etc.)
          unique.words$POS <- gsub(x=unique.words$POS, pattern = POS_regex[[i]], replacement = names(POS_regex[i]))
        }
      } # end if statement !is.null(POS_regex)
      
      # join unique.words back to this.words to attach POS and base word
      this.words <- dplyr::left_join(this.words, unique.words, by = "mor_word")
    } else {
      # if !use.mor
      this.words$POS <- NA_character_
      this.words$base_word <- NA_character_
      this.words$gloss <- NA_character_
    }
    
    # If there was an alignment error when breaking the utterance into individual words (i.e. mor and speaker tiers did not align easily)
    # then drop POS, as it may be inaccurate
    this.words$POS <- ifelse(this.words$alignment.error, NA_character_, this.words$POS)
    
    # Count occurrences of each word
    this.count <- this.words %>% 
      ungroup() %>% 
      count(role, orth_word, gloss, POS) %>% # Note: for use.mor=FALSE, POS will just be NA for every word
      tidyr::spread(key = role, value = n, fill = 0) %>% 
      dplyr::mutate(use.mor = use.mor) %>% 
      dplyr::select(word=orth_word, gloss, POS, chi.count, adu.count, use.mor)
  
  } # end if statement checking that there are both chi and adu utts available (N.chi.utts*N.adu.utts > 0)
  
  # If there are no utterances at all for chi or adu (or both), leave counts blank
  if( N.chi.utts*N.adu.utts == 0 ){
    this.count <- data.frame(word=NA_character_, gloss=NA_character_, POS=NA_character_, chi.count=0, adu.count=0, use.mor, stringsAsFactors = FALSE) %>% as.tbl()
  }  

  this.count$N.chi.utts <- N.chi.utts
  this.count$N.adu.utts <- N.adu.utts
  
  return(this.count)
}