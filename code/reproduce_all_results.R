library(tidyverse)
library(magrittr)
library(lubridate)
library(tidytext)
library(ggrepel)
library(ggwordcloud)
library(grid)
library(gridExtra)
library(sentimentr)
library(cleanNLP)
library(knitr)
library(kableExtra)

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

word_sentiment <- dat_main %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn"))

make_over_time_plot <- function(dat, x_col, y_col, label_col, color_col, y_label,
                                y_min, y_max,
                                x_nudge_col=NULL, y_nudge_col=NULL){
  
  dem_dat <- filter(dat, party=="Democratic Candidate")
  rep_dat <- filter(dat, party=="Republican Candidate")
  
  data_text <- rbind(
    as.data.frame(do.call(cbind, approx(dem_dat[[x_col]], dem_dat[[y_col]], n = 1000))),
    as.data.frame(do.call(cbind, approx(rep_dat[[x_col]], rep_dat[[y_col]], n = 1000))))
  
  data_text[[label_col]] <- ""
  data_text[[color_col]] <- NA_character_
  colnames(data_text) <- c(x_col, y_col, label_col, color_col)
  
  
  data_text$x_nudge <- 0
  data_text$y_nudge <- 0
  
  if(!is.null(x_nudge_col)){
    dat <- dat %>% 
      rename("x_nudge"=x_nudge_col)
  }
  else{
    dat$x_nudge <- 0
  }
  
  if(!is.null(y_nudge_col)){
    dat <- dat %>% 
      rename("y_nudge"=y_nudge_col)
  }
  else{
    dat$y_nudge <- 0
  }
  
  data_text <- rbind(dat, data_text)
  
  ggplot(dat, aes_string(x=x_col, y=y_col, col=color_col, label=label_col)) +
    geom_line(size=1.25) +
    geom_point(size=3) +
    geom_text_repel(data=data_text,
                    segment.size  = 0.2,
                    box.padding = 0.75,
                    show.legend = F,
                    nudge_x = data_text$x_nudge,
                    nudge_y = data_text$y_nudge,
                    force=1) +
    xlim(1958,2022) +
    ylim(y_min,y_max) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    scale_color_manual(values=c("#0015BC", "#DE0100")) +
    labs(x="Election Year", y=y_label)
}

dat_p1 <- word_sentiment %>% 
  # Here, drop the VP debates and other party candidates
  filter(vp_debate == 0, party != "Other Candidate") %>% 
  group_by(election, party, speaker_clean) %>% 
  summarise(avg_word_sent = mean(value)) %>% 
  ungroup %>% 
  # Some nudge columns that will be useful when we plot
  mutate(x_nudge = case_when(
    speaker_clean == "G.H.W. Bush" & election == 1992 ~ 2,
    speaker_clean == "H. Clinton" & election == 2016 ~ 5,
    TRUE ~ 0
  )) %>% 
  mutate(y_nudge = case_when(
    speaker_clean == "B. Clinton" & election == 1992 ~ -0.1,
    TRUE ~ 0
  ))


make_over_time_plot(dat_p1, x_col="election", y_col="avg_word_sent",
                    label_col="speaker_clean", color_col="party", y_label="Average Sentiment",
                    y_min=-0.6, y_max=0.7,
                    x_nudge_col="x_nudge", y_nudge_col="y_nudge") + labs(title="Average Sentiment of U.S. Presidential Debaters")

ggsave("../output/avg_sentiment_by_election.png",
       width=12, height=6)

set.seed(9)

dat_p2 <- word_sentiment %>% 
  group_by(date, party, speaker_clean) %>% 
  summarise(avg_word_sent = mean(value)) %>% 
  ungroup() %>% 
  mutate(party = factor(party, levels=c("Democratic Candidate",
                                        "Republican Candidate",
                                        "Other Candidate"))) %>% 
  group_by(speaker_clean) %>% 
  mutate(rand_int=sample.int(n())) %>% 
  ungroup %>% 
  mutate(text_label = if_else(
    date >= as.Date("2016-01-01") | rand_int == 1 | avg_word_sent < -0.3 | avg_word_sent > 0.6,
    paste(speaker_clean, date), ""))

ggplot(dat_p2, aes(x=1, y=avg_word_sent, col=party, label=text_label)) +
  geom_vline(xintercept = 1, color="gray") +
  geom_point(size=2, alpha=0.35) +
  geom_text_repel(
    size = 3,
    segment.size = 0.2,
    point.padding = 0.5,
    min.segment.length = 0,
    show.legend = F
  ) +
  theme_minimal() +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  ) + 
  scale_color_manual(values=c("#0015BC", "#DE0100", "#8A8A8A")) +
  labs(y="Average Sentiment", title="Average Sentiment by U.S. General Election Debate Performance")

ggsave("../output/avg_sentiment_by_debate.png",
       width=8, height=8)

trump_word_usage <- word_sentiment%>% 
  filter(speaker_clean == "Trump") %>% 
  group_by(word, value) %>% 
  summarise(n=n()) %>% 
  ungroup %>% 
  mutate(share_of_words = round((n/sum(n)) * 100, 2)) 

kennedy_word_usage<- word_sentiment %>% 
  filter(speaker_clean == "Kennedy") %>% 
  group_by(word, value) %>% 
  summarise(n=n()) %>% 
  ungroup %>% 
  mutate(share_of_words = round((n/sum(n)) * 100, 2)) 

bind_rows(
  trump_word_usage %>% arrange(value, desc(share_of_words)) %>% head(10),
  trump_word_usage %>% arrange(value, share_of_words) %>% tail(10)) %>% 
  select(Word=word, `Sentiment Value`=value, `Share of Neg & Pos Words (%)`= share_of_words)

bind_rows(
  kennedy_word_usage %>% arrange(value, desc(share_of_words)) %>% head(10),
  kennedy_word_usage %>% arrange(value, share_of_words) %>% tail(10)) %>% 
  select(Word=word, `Sentiment Value`=value, `Share of Neg & Pos Words (%)`= share_of_words)


set.seed(9)

dat_p3 <- dat_p2 %>% 
  mutate(text_label = if_else(
    speaker_clean %in% c("Trump", "Biden"),paste(speaker_clean, date), ""))

ggplot(dat_p3, aes(x=1, y=avg_word_sent, col=party, label=text_label)) +
  geom_vline(xintercept = 1, color="gray") +
  geom_point(size=2, alpha=0.35) +
  geom_text_repel(
    size = 3,
    segment.size = 0.2,
    point.padding = 0.5,
    min.segment.length = 0,
    show.legend = F
    ) +
  theme_minimal() +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank()
  ) + 
  scale_color_manual(values=c("#0015BC", "#DE0100", "#8A8A8A")) +
  labs(y="Average Sentiment", title="Average Sentiment by U.S. General Election Debate Performance – Biden/Trump")

ggsave("../output/avg_sentiment_by_debate_biden_trump.png",
       width=8, height=8)

sentence_sent <- dat_main %>% 
  filter(vp_debate == 0, party != "Other Candidate") %>% 
  group_by(election, speaker_clean, party) %>% 
  summarise(text = paste(text, collapse=" ")) %>% 
  ungroup %>% 
  get_sentences() %$%
  sentiment_by(text, list(election, speaker_clean, party)) %>% 
  as_tibble() %>% 
  select(-word_count, -sd)

make_over_time_plot(sentence_sent, x_col="election", y_col="ave_sentiment",
                    label_col="speaker_clean", color_col="party", y_label="Average Sentiment",
                    y_min=-0.02, y_max=0.14) + labs(title="Average Sentiment of U.S. Presidential Debaters (sentimentr)")

ggsave("../output/avg_sentiment_by_election_sentimentr.png",
       width=12, height=6)

sentence_emotion <- dat_main %>% 
  filter(vp_debate == 0, party != "Other Candidate") %>% 
  group_by(election, speaker_clean, party) %>% 
  summarise(text = paste(text, collapse=" ")) %>% 
  ungroup %>% 
  get_sentences() %$%
  emotion_by(text, list(election, speaker_clean, party)) %>% 
  as_tibble() %>% 
  select(-word_count, -emotion_count, -sd)

disgust <- 
  make_over_time_plot(select(filter(sentence_emotion, emotion_type=="disgust"), -emotion_type),
                    x_col="election", y_col="ave_emotion",
                    label_col="speaker_clean", color_col="party", y_label="",
                    y_min=0, y_max=0.014)

ggsave("../output/avg_disgust_by_election.png", disgust, 
       width=12, height=6)

anger <- 
  make_over_time_plot(select(filter(sentence_emotion, emotion_type=="anger"), -emotion_type),
                    x_col="election", y_col="ave_emotion",
                    label_col="speaker_clean", color_col="party", y_label="",
                    y_min=0.005, y_max=0.02)

ggsave("../output/avg_anger_by_election.png", anger, 
       width=12, height=6)

fear <- 
  make_over_time_plot(select(filter(sentence_emotion, emotion_type=="fear"), -emotion_type),
                    x_col="election", y_col="ave_emotion",
                    label_col="speaker_clean", color_col="party", y_label="",
                    y_min=0.005, y_max=0.03)

ggsave("../output/avg_fear_by_election.png", fear, 
       width=12, height=6)

trust <- 
  make_over_time_plot(select(filter(sentence_emotion, emotion_type=="trust"), -emotion_type),
                    x_col="election", y_col="ave_emotion",
                    label_col="speaker_clean", color_col="party", y_label="",
                    y_min=0.015, y_max=0.06)

ggsave("../output/avg_trst_by_election.png", trust, 
       width=12, height=6)


p <- grid.arrange(
  disgust + labs(title="Disgust") +
    theme(legend.position = "none", plot.title = element_text(size=16, hjust=0.5)),
  anger + 
    labs(title = "Anger") + 
    theme(legend.position = "none", plot.title = element_text(size=16, hjust=0.5)),
  fear + 
    labs(title = "Fear") +
    theme(legend.position = "none", plot.title = element_text(size=16, hjust=0.5)),
  trust + 
    labs(title = "Trust") +
    theme(legend.position = "none", plot.title = element_text(size=16, hjust=0.5)),
  
  top = textGrob("Average Emotional Scores of U.S. Presidential Debaters",
                 gp=gpar(fontsize=20)),
  
  nrow = 2)

ggsave("../output/avg_emotion_by_election.png", p, 
       width=16, height=12)


set.seed(17)

bigrams <- dat_main %>% 
  filter(vp_debate == 0, party != "Other Candidate") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!is.na(word1), !is.na(word2)) %>% 
  filter(word1 != "uh", word2 != "uh")

bigram_frequencies <- bigrams_filtered %>%
  group_by(election, speaker_clean, word1, word2) %>% 
  summarise(n = n()) %>% 
  ungroup %>%
  arrange(desc(n)) %>% 
  group_by(election, speaker_clean) %>% 
  slice(1:20) %>% 
  ungroup

bigram_frequencies_w_party <- dat_main %>% 
  distinct(election, speaker_clean, party) %>% 
  right_join(bigram_frequencies, by=c("election","speaker_clean")) %>% 
  mutate(n_gram = paste(word1, word2))

dems <- filter(bigram_frequencies_w_party, party == "Democratic Candidate")
repubs <- filter(bigram_frequencies_w_party, party == "Republican Candidate")

years <- unique(bigram_frequencies_w_party$election)

for(i in 1:length(years)){
  
  year <- years[i]
  
  curr_dem <- filter(dems, election == year)
  # Sometimes the speaker says his opponent's name way more than
  # all the others (e.g. Governor Romney). This leads the cloud
  # to have a weird scaling. We can still show that the word
  # was said far more but not destroy the cloud by scaling
  # it down a bit.
  curr_dem <- curr_dem %>% arrange(desc(n))
  if(curr_dem$n[1] > 1.25 * curr_dem$n[2]){
    curr_dem$n[1] <- curr_dem$n[2] * 1.25
  }
  curr_dem_name <- curr_dem$speaker_clean[1]
  
  curr_repub <- filter(repubs, election == year)
  curr_repub <- curr_repub %>% arrange(desc(n))
  if(curr_repub$n[1] > 1.25 * curr_repub$n[2]){
    curr_repub$n[1] <- curr_repub$n[2] * 1.25
  }
  curr_repub_name <- curr_repub$speaker_clean[1]
  
  p_dem <- ggplot(curr_dem, aes(label=n_gram, size=n)) +
    geom_text_wordcloud(color="#0015BC") +
    scale_size_area(max_size = 6) +
    theme_minimal() +
    theme(plot.title = element_text(size=14,
                                    hjust=0.5,
                                    color = "#3F3F3F")) +
    labs(title=curr_dem_name)
  
  p_repub <- ggplot(curr_repub, aes(label=n_gram, size=n)) +
    geom_text_wordcloud(color="#DE0100") +
    scale_size_area(max_size = 6) +
    theme_minimal() +
    theme(plot.title = element_text(size=14,
                                    hjust=0.5,
                                    color = "#3F3F3F")) +
    labs(title=curr_repub_name)
  
  p_both <- grid.arrange(p_dem,
                         p_repub,
                         nrow = 1,
                         top = textGrob(str_interp("${year} Election Debates"),
                                        gp=gpar(fontsize=18, col="#3F3F3F")))
  
  ggsave(str_interp("../output/wordclouds_${year}_election.png"), p_both, 
         width=10, height=4)
}

# I pre-annotated the using cleanNLP. This process takes a long time.
# See the script clean_NLP_annotate.R.

anno <- read_rds("../output/annotated_all_debates.Rds")

# Do PCA using all (non-proper) Nouns, Adjectives, and Verbs
pca <- anno$token %>%
  filter(upos %in% c("NOUN", "ADJ", "VERB")) %>%
  cnlp_utils_tfidf(min_df = 0.1, max_df = 0.9, tf_weight = "lognorm") %>%
  cnlp_utils_pca()

pca <- bind_cols(anno$document, pca) %>% 
  mutate(party=factor(party, levels=c("Democratic Candidate",
                                      "Republican Candidate",
                                      "Other Candidate")))

ggplot(pca, aes(PC1, PC2, label=paste(paste(speaker_clean, date)), color=party)) +
  geom_point(alpha = 0.35, size = 4) +
  geom_text_repel(
    size = 3,
    segment.size = 0.2,
    min.segment.length = 0,
    show.legend = F
  ) +
  scale_color_manual(values=c("#0015BC", "#DE0100", "#8A8A8A")) +
  theme_void() +
  theme(legend.title = element_blank()) +
  labs(title="Principal Components Analysis (Nouns, Adjectives, and Verbs)")

ggsave("../output/pca_noun_adj_verbs.png",
       width=12, height=8)

# Do PCA using only Adjectives and Verbs
pca2 <- anno$token %>%
  filter(upos %in% c("ADJ", "VERB")) %>%
  cnlp_utils_tfidf(min_df = 0.1, max_df = 0.9, tf_weight = "lognorm") %>%
  cnlp_utils_pca()

pca2 <- bind_cols(anno$document, pca2) %>% 
  mutate(party=factor(party, levels=c("Democratic Candidate",
                                      "Republican Candidate",
                                      "Other Candidate")))

ggplot(pca2, aes(PC1, PC2, label=paste(paste(speaker_clean, date)), color=party)) +
  geom_point(alpha = 0.35, size = 4) +
  geom_text_repel(
    size = 3,
    segment.size = 0.2,
    min.segment.length = 0,
    show.legend = F
  ) +
  scale_color_manual(values=c("#0015BC", "#DE0100", "#8A8A8A")) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(title="Principal Components Analysis (Adjectives and Verbs)")

ggsave("../output/pca_adj_verbs.png",
       width=12, height=8)
