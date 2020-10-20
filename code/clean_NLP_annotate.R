library(tidyverse)
library(cleanNLP)

dat_in <- read_csv("../output/all_debate_text.csv")

dat_main <- dat_in %>% 
  filter(speaker_clean == gop_candidate | 
           speaker_clean == dem_candidate | 
           speaker_clean == other_candidate) %>% 
  mutate(party = case_when(
    speaker_clean == gop_candidate ~ "Republican Candidate",
    speaker_clean == dem_candidate ~ "Democratic Candidate",
    speaker_clean == other_candidate ~ "Other Candidate"
  )) %>% 
  mutate(speaker_clean = case_when(
    speaker_clean == "B_CLINTON" ~ "B. Clinton",
    speaker_clean == "H_CLINTON" ~ "H. Clinton",
    speaker_clean == "GHW_BUSH" ~ "G.H.W. Bush",
    speaker_clean == "GW_BUSH" ~ "G.W. Bush",
    speaker_clean == "MCCAIN" ~ "McCain",
    TRUE ~ str_to_title(speaker_clean)
  ))

# Use the udpipe init
cnlp_init_udpipe()

dat_by_debate <- dat_main %>% 
  group_by(date, speaker_clean, party) %>% 
  summarise(text = paste(text, collapse=" ")) %>% 
  ungroup

# This annotation step takes a long time to run
anno <- cnlp_annotate(dat_by_debate)

write_rds(anno, "../output/annotated_all_debates.Rds")