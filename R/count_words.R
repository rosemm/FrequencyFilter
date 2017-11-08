#' Count words in child and adult speech
#'
#' Counts the number of occurrences of each word in the target child's speech and that of the child's primary interlocutor
#' Optionally distinguish counts of eah word by part of speech by setting use.mor=TRUE (e.g. count "kiss" as a verb and "kiss" as a noun separately).
#' It makes used of the part of speech tagging available in the MOR tier of CHAT transcribed files, but if desired one can
#' further collapse those part of speech categories using the POS_regex argument, which will replace MOR part of speech
#' with the user-specified labels. This is useful if, for example, one wants to count occurrences of words separately by
#' broad part of speech category but not as fine-grained as MOR tags (e.g. count "kiss" as a noun separately from "kiss" as a verb, but collapse categories for child forms, family words and wordplay so that "beep" isn't counted separately for each.)
#'
#' @param this.transcript a dataframe with all words from the utterances from one transcript (or one timepoint, if transcripts from roughly the same age are being pooled together)
#' @param use.mor whether or not to count occurrences separately by part of speech
#' @param POS_regex if use.mor=TRUE, optionally collapse part of speech categories using regular expressions here
#' @param mor_word the column that contains the MOR tier entries
#' @param orth_word the column that contains the speaker tier entries
#' @param debug turn on to display extra messages while the function runs, useful for identifying problem transcripts. Default is FALSE.
#'
#' @return A dataframe with word (from the speaker tier), gloss and POS (extracted from the mor tier), and counts from target child and target child's primary adult interlocutor.
#' To count the words for several transcripts, count_words can be placed in side a for loop or do expression.
#'
#' @examples
#' \dontrun{
#' words.mor <- transcripts.cleaned %>%
#' group_by(language, corpus, child, age.mos, file, cha, speaker, name, role, utt.num) %>%
#'   do({
#'     utts_to_words(., debug = FALSE)
#'   }) %>%
#'   ungroup()
#'
#' word.counts.mor <- words.mor %>%
#' group_by(language, corpus, child, age.mos, file, cha) %>%
#'   do({
#'     count_words(., use.mor = TRUE, POS_regex=NULL, debug = FALSE)
#'   })
#' }
#'
#' @seealso \code{\link[FrequencyFilter]{utts_to_words}}
#'
#' @export
count_words <- function(this.transcript, use.mor = TRUE, POS_regex=NULL, mor_word = "mor_word", orth_word = "orth_word", debug=FALSE){
  stopifnot(length(unique(this.transcript$file)) == 1)
  if(debug) message("processing... ", unique(this.transcript$file))

  # for use in dplyr functions below (see vignette("programming") in the dplyr package)
  mor_word <- enquo(mor_word)
  orth_word <- enquo(orth_word)

  # select columns to be used for mor_word and orth_word
  # (default is to use those names to begin with, in which case no change in column names occurs)
  this.transcript <- this.transcript %>%
    rename(mor_word = !!mor_word, orth_word = !!orth_word)

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
