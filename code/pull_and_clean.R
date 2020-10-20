library(tidyverse)
library(rvest)

# This file will loop through the debates enumerated in debate_key.csv. 
# It will read them from the Comission on Presidential Debates website
# and perform a series of operations to clean the data.

# The 2020 debates are not yet available on their website, instead
# I pull these from rev.com.

# A key with information on each debate
debate_key <- read_csv("../input/debate_key.csv")

# A key to clean every name that appears in a debate transcript
name_cleaning_key <- read_csv("../input/name_cleaning_key.csv")

# This was somewhat painful to construct, but in some debates they include
# other lines of meta information in the transcript. I remove these here.
other_meta_lines <- read_csv("../input/other_meta_lines.csv")

# I will remove certain crowd reactions
crowd_reactions <- read_csv("../input/crowd_reactions.csv") %>% 
  .$crowd_reactions

crowd_reactions_str <- crowd_reactions %>% 
  str_replace_all("\\(", "\\\\(") %>% 
  str_replace_all("\\)", "\\\\)") %>% 
  str_replace_all("\\[", "\\\\[") %>% 
  str_replace_all("\\]", "\\\\]") %>% 
  str_replace_all("\\.", "\\\\.") %>% 
  paste(collapse="|")

# I will build thie table in a big cleaning loop
final_debate_tibble <- NULL

for(i in 1:nrow(debate_key)){
  
  page <- str_interp("https://www.debates.org/voter-education/debate-transcripts/${debate_key$transcript_url[i]}")
  page_html <- read_html(page)
  
  n_meta_lines <- debate_key$n_meta_lines[i]
  
  curr_name_cleaning_key <- filter(name_cleaning_key, election == debate_key$election[i])
  
  # Extract the text
  if(debate_key$date[i] == "2008-10-02"){  # Uses a different html encoding for some reason
    extracted_text <- page_html %>% 
      html_node("body") %>% 
      xml2::xml_find_all("//div[contains(@class, 'bodytext')]") %>% 
      # This now is every paragraph of the main text
      html_children() %>% 
      # Remove the first N lines with meta information
      .[7:length(.)] %>% 
      html_text()
  }
  else{
    extracted_text <- page_html %>% 
      html_node("body") %>% 
      xml2::xml_find_all("//div[contains(@id, 'content-sm')]") %>% 
      # This now is every paragraph of the main text
      html_children() %>% 
      # Remove the first N lines with meta information
      .[(n_meta_lines+1):length(.)] %>% 
      html_text()
  }
  
  # Remove lines that are just (Applause) and (Laughter)
  extracted_text <- extracted_text[!extracted_text %in% crowd_reactions]
  
  # Remove any other meta lines
  curr_meta_lines <- other_meta_lines %>% 
    filter(date == debate_key$date[i]) %>% 
    .$line %>% 
    c(., "")  # Add a removal of blank lines
    
  extracted_text <- extracted_text[!extracted_text %in% curr_meta_lines]
  
  # Fix a weird number thing that happens in 1984
  if(debate_key$election[i] == 1984){
    extracted_text <- extracted_text %>% 
      str_remove_all("\\\\1\\/2\\\\") %>% 
      str_remove_all("\\\\1\\/4\\\\")
  }
  
  # Fix the one place the speaker is missing on a line in 1992
  if(debate_key$date[i] == "1996-10-06"){
    broken_line <- which(substr(extracted_text, 1, 1) == ":")
    extracted_text[broken_line] <- paste0(
      "CLINTON",
      extracted_text[broken_line])
  }
  
  # Fix the UTF errors in 2000
  extracted_text <- extracted_text %>% 
    str_replace_all("â€™", "'") %>% 
    str_replace_all("â€“", "-") %>% 
    str_replace_all("â€\u009d", "'") %>% 
    str_replace_all("â€œ", "'")
    
  # Fix the 2004 Cheney response typos
  if(debate_key$date[i] == "2004-10-05"){
    extracted_text <- extracted_text %>% 
      str_replace("IFILL: Mr. Vice President\\? CHENEY:", "CHENEY:") %>% 
      str_replace("(LAUGHTER) CHENEY: ", "CHENEY:")
  }
  
  # In 2008 they put two transcriptions, we pick the first
  if(debate_key$date[i] == "2008-09-26"){
    last_line <- which(extracted_text == "END")[1] - 1
    extracted_text <- extracted_text[1:last_line]
  }
  
  # Fix a couple weird 2012 things
  if(debate_key$date[i] == "2012-10-03"){
    extracted_text <- extracted_text %>% 
      str_replace("(CROSSTALK) LEHRER:", "LEHRER:") %>% 
      str_replace(" LEHRER:", "LEHRER:")
    # Remove the weird character from the very end
    extracted_text <- extracted_text[1:length(extracted_text)-1]
  }
  
  if(debate_key$date[i] == "2012-10-16"){
    extracted_text <- extracted_text %>% 
      str_replace("Go ahead. OBAMA:", "OBAMA:") %>% 
      str_replace(" ROMNEY:", "ROMNEY:")
  }
  
  # Regexs to match
  name_regexes <- c(
    "^[A-Z\\s\\.\\“\\”\\’\\–\\/\\[\\]\\*,]*:", # all caps followed by colon
    "^[A-Z\\s\\.\\“\\”\\’\\–\\/\\[\\]\\*,]*;", # all caps followed by semi-colon
    "^Mr.[A-Z\\s\\.\\“\\”\\’\\–\\/\\[\\]\\*,]*:", # Sometimes its Mr. instead of MR.
    "FRANK McGEE, MODERATOR:", # McGee
    "MR. McGEE:", # McGee
    "MR. SM1TH:", # SM1TH (typo)
    "MS. McAFEE:", # McAFFE
    "	CHENEY:"  # space before CHENEY
  )
  
  speaker_str <- paste(name_regexes, collapse = "|")
  speaker_remove_str <- paste(paste0(name_regexes,"\\s"), collapse = "|")
  
  # Split speaker name from text
  speaker_name <- extracted_text %>% 
    str_extract(speaker_str) %>% 
    str_remove(":")
  
  speaker_text <- extracted_text %>% 
    str_remove(speaker_remove_str) %>% 
    # Remove all the applause and laughter in brackets
    str_remove("\\[[A-za-z]*\\]") %>% 
    str_remove_all(crowd_reactions_str)

  curr_debate_tibble <- 
    tibble(
      speaker_raw = speaker_name,
      text = speaker_text
    )
  
  # If we are missing a speaker, it is because the response is split into
  # multiple paragraphs. Fill downward over missing values here.
  curr_debate_tibble <- curr_debate_tibble %>%
    fill(speaker_raw, .direction="down")

  # Clean the speaker names using the key
  curr_debate_tibble <- curr_debate_tibble %>% 
    left_join(curr_name_cleaning_key, by=c("speaker_raw"="raw_name")) %>% 
    # Mutate in the general information about the debate
    mutate(election = debate_key$election[i],
           date = debate_key$date[i],
           gop_candidate =  debate_key$gop_candidate[i],
           dem_candidate = debate_key$dem_candidate[i],
           other_candidate = debate_key$other_candidate[i],
           vp_debate = debate_key$vp_debate[i]) %>% 
    select(
      election, date, gop_candidate, dem_candidate, other_candidate, vp_debate,
      speaker_clean=clean_name, speaker_raw, text) %>% 
    mutate(text_num = 1:n())
  
  # Add to the big tibble
  if(is.null(final_debate_tibble)){
    final_debate_tibble <- curr_debate_tibble
  }
  else{
    final_debate_tibble <- bind_rows(final_debate_tibble, curr_debate_tibble)
  }
}


# Until the presidential debate posts the transcripts for 2020, I use
# transcripts downloaded from rev.com. This cleaning is pretty manual
# right now unfortunately.

## 2020 PRESIDENTIAL DEBATE 1 ##
part1 <- read_table("../input/2020_debate1_part1.txt", col_names=FALSE)
part2 <- read_table("../input/2020_debate1_part2.txt", col_names=FALSE)

curr_debate_tibble <- tibble(
  text = c(part1$X1, part2$X1)
)

curr_debate_tibble <- curr_debate_tibble %>% 
  mutate(speaker_raw = case_when(
    text == "Chris Wallace:" ~ "WALLACE",
    text == "President Donald J. Trump:" ~ "TRUMP",
    text == "Vice President Joe Biden:" ~ "BIDEN")) %>% 
  fill(speaker_raw, .direction="down") %>% 
  filter(!text %in% c("Chris Wallace:", "President Donald J. Trump:", "Vice President Joe Biden:"),
         !is.na(text)) %>% 
  mutate(election=2020, date=as.Date("2020-09-29"),
         gop_candidate="TRUMP", dem_candidate="BIDEN", other_candidate=NA_character_,
         vp_debate=0, speaker_clean=speaker_raw) %>% 
  mutate(text = str_remove_all(text, "\\[crosstalk\\]")) %>% 
  mutate(text = str_remove_all(text, "\\[crosstalk\\s[0-9]*:[0-9]*:[0-9]*\\]")) %>% 
  mutate(text = str_remove_all(text, "\\[crosstalk\\s[0-9]*:[0-9]*:[0-9]*:[0-9]*\\]")) %>% 
  select(
    election, date, gop_candidate, dem_candidate, other_candidate, vp_debate,
    speaker_clean, speaker_raw, text) %>% 
  mutate(text_num = 1:n())

final_debate_tibble <- bind_rows(final_debate_tibble, curr_debate_tibble)
  
## 2020 VP DEBATE ##
part1 <- read_table("../input/2020_vp_part1.txt", col_names=FALSE)
part2 <- read_table("../input/2020_vp_part2.txt", col_names=FALSE)

curr_debate_tibble <- tibble(
  text = c(part1$X1, part2$X1)
)

curr_debate_tibble <- curr_debate_tibble %>% 
  mutate(speaker_raw = case_when(
    text == "Susan Page:" ~ "PAGE",
    text == "Mike Pence:" ~ "PENCE",
    text == "Kamala Harris:" ~ "HARRIS")) %>% 
  fill(speaker_raw, .direction="down") %>% 
  filter(!text %in% c("Susan Page:", "Mike Pence:", "Kamala Harris:"),
         !is.na(text)) %>% 
  mutate(election=2020, date=as.Date("2020-10-07"),
         gop_candidate="PENCE", dem_candidate="HARRIS", other_candidate=NA_character_,
         vp_debate=1, speaker_clean=speaker_raw) %>% 
  mutate(text = str_remove(text, "\\[crosstalk\\]")) %>% 
  mutate(text = str_remove(text, "\\[crosstalk\\s[0-9]*:[0-9]*:[0-9]*\\]")) %>% 
  mutate(text = str_remove(text, "\\[crosstalk\\s[0-9]*:[0-9]*:[0-9]*:[0-9]*\\]")) %>% 
  select(
    election, date, gop_candidate, dem_candidate, other_candidate, vp_debate,
    speaker_clean, speaker_raw, text) %>% 
  mutate(text_num = 1:n())

final_debate_tibble <- bind_rows(final_debate_tibble, curr_debate_tibble)

write_csv(final_debate_tibble, "../output/all_debate_text.csv")


