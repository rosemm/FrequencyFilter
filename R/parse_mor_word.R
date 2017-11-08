#' Extract information from %mor tier words in CHAT transcripts
#'
#' @export
parse_mor_word <- function(mor_word, extract = c("POS", "word", "gloss")){
  
  # pull prefixes off
  prefix_split <- strsplit(mor_word, split = "#", fixed = TRUE)[[1]]
  word_root <- prefix_split[(length(prefix_split))] # the last item in the prefix split
  
  # pull out the gloss (translation) for words that have it included
  gloss_split <- strsplit(word_root, split = "=", fixed = TRUE)[[1]]
  if(length(gloss_split) == 2){
    word_root <- gloss_split[1]
    gloss <- gloss_split[2]
  } else {
    gloss <- NA_character_
  }
  
  # separate POS from word root
  pos_split <- strsplit(word_root, split = "|", fixed = TRUE)[[1]]
  if(length(pos_split) == 2){
    # exactly one | marking POS
    POS <- pos_split[1]
    word_root <- pos_split[2]
  } else if(length(pos_split) == 1) {
    # no | marking POS
    POS <- NA_character_
  } else if (length(pos_split) > 2){
    if(grepl(x=mor_word, pattern = "+", fixed = TRUE)){
      # more than one | marking POS happens for compound nouns like n|+n|bunny+n|rabbit
      POS <- pos_split[1] 
      compound_split <- strsplit(word_root, split = "+", fixed = TRUE)[[1]]
      POS <- strsplit(compound_split[1], split = "|", fixed = TRUE)[[1]] # the POS of the word overall
      compound_roots <- gsub(x=compound_split[2:length(compound_split)], pattern = "[^|]*[|](.*)", replacement = "\\1")
      word_root <- paste(compound_roots, collapse = "")
    } else if(grepl(x=mor_word, pattern = "~", fixed = TRUE)){
      # more than one | marking POS happens for contractions like v|let~pro:obj|us (let's)
      contraction_split <- strsplit(word_root, split = "~", fixed = TRUE)[[1]]
      contraction_roots <- gsub(x=contraction_split, pattern = "[^|]*[|](.*)", replacement = "\\1")
      word_root <- paste(contraction_roots, collapse = "_")
      contraction_POS <- gsub(x=contraction_split, pattern = "(.*)[|].*", replacement = "\\1")
      POS <- paste("CONT", paste(contraction_POS, collapse = "_"), sep = "=")
    } else {
      # If no POS has been determined in the above steps
      POS <- NA_character_
    }
  }
  
  # paste prefixes back onto word root
  if(length(prefix_split) > 1){
    prefixes <- paste(prefix_split[1:(length(prefix_split)-1)], collapse = "")
    word <- paste(c(prefixes, word_root), collapse = "")
  } else {
    word <- word_root
  }
  
  # that each item is one element long (not a list)
  if(length(POS) != 1){
    POS <- NA_character_
  }
  if(length(gloss) != 1){
    gloss <- NA_character_
  }
  if(length(word) != 1){
    word <- mor_word # the unparsed mor_word
  }
  
  if(extract == "POS")   return(POS)
  if(extract == "word")  return(word)
  if(extract == "gloss") return(gloss)
}
