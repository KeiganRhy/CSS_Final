library(readtext)
data1 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Democrat/Browner.txt")
data2 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Democrat/Jackson.txt")
data3 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Democrat/Mccarthy.txt")
data4 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Democrat/Regan.txt")

combined_democrat <- rbind(data1, data2, data3, data4)
#combine the democratic admins

data5 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Republican/Johnson.txt")
data6 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Republican/Leavitt.txt")
data7 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Republican/Wheeler.txt")
data8 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Republican/Pruitt.txt")
data9 <- readtext("~/EPA_FINAL/EPA Admin. Data/EPA_Text/Republican/Whitman.txt")

combined_republican <- rbind(data5,data6,data7,data8,data9)
#combine the republican admins

library(quanteda) 
library(topicmodels) 
library(stm) 
library(tidyverse)
library(tidytext) 
library(reshape2) 

combined_data_frame_democrat <- as.data.frame(combined_democrat)
str(combined_data_frame_democrat)
#this is just me checking to make sure that the structure is sucessfully a data frame
combined_data_frame_republican <- as.data.frame(combined_republican)
str(combined_republican)

democrat_corpus <- corpus(combined_data_frame_democrat)
view(democrat_corpus)
republican_corpus <- corpus(combined_data_frame_republican)
view(republican_corpus)
#creating the corpuses for each party

democrat_toks_clean <- democrat_corpus %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("en"), "shall", "upon")) 

republican_toks_clean <- republican_corpus %>%
  tokens(remove_punct = TRUE, 
         remove_symbols = TRUE, 
         remove_numbers = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("en"), "shall", "upon")) 
#pre-processing removing the stopwords, punctuation, and symbols for both parties

democrat_dfm <- dfm(democrat_toks_clean)
republican_dfm <- dfm(republican_toks_clean)
#creating the dfms 

democrat_dfm.t<-dfm_trim(democrat_dfm, 
                          min_docfreq = 0.075, 
                          max_docfreq = 0.95, 
                          docfreq_type = "prop")

republican_dfm.t<-dfm_trim(republican_dfm, 
                          min_docfreq = 0.075, 
                          max_docfreq = 0.95, 
                          docfreq_type = "prop")
#trimming each of the DFM
sparsity(democrat_dfm)
#0.66
sparsity(democrat_dfm.t)
#0.67
sparsity(republican_dfm)
#0.72
sparsity(republican_dfm.t)
#0.73

#TOPIC MODELS 
democrat_dtm <- convert(democrat_dfm.t, to = "topicmodels")
republican_dtm <- convert(republican_dfm.t, to = "topicmodels")
#DFM to DTM

set.seed(2024) # similar to the example from class

democrat_lda_t10<- LDA(democrat_dtm, 
                        k = 5, # number of topics 
                        method = "VEM" 
)

set.seed(2024) 

republican_lda_t10<- LDA(republican_dtm, 
                        k = 5, # number of topics 
                        method = "VEM" 
)
#making the LDAs for each party!!

options(scipen=999) #turn off the scientific notation
democrat_theta_t10 <- posterior(democrat_lda_t10)$topics %>%
  as.data.frame() %>% # theta matrix to a data frame
  rename_with(~ paste0("topic_", .), .cols = seq_along(.)) %>% # adding a common prefix "topic_" to the columns 
  mutate(document = row_number())  
glimpse(democrat_theta_t10)

options(scipen=999) #turn off the scientific notation
republican_theta_t10 <- posterior(republican_lda_t10)$topics %>%
  as.data.frame() %>% # theta matrix to a data frame
  rename_with(~ paste0("topic_", .), .cols = seq_along(.)) %>% # adding a common prefix "topic_" to the columns 
  mutate(document = row_number())  
glimpse(republican_theta_t10)
#this is a better look at the theta matrix

#TOP TOPICS FOR EACH DOCUMENT 
democrat_top5_t10 <- democrat_theta_t10 %>%
  #from a wide format to a long format
  gather(key = "topic",
         value = "probability", 
         -document # reshape all columns except "document"
  ) %>%
  # top 5 for each doc
  group_by(document) %>%
  top_n(5, probability) %>%
  ungroup()
head(democrat_top5_t10)

republican_top5_t10 <- republican_theta_t10 %>%
  #from a wide format to a long format
  gather(key = "topic",
         value = "probability", 
         -document # reshape all columns except "document"
  ) %>%
  # top 5 for each doc
  group_by(document) %>%
  top_n(5, probability) %>%
  ungroup()
head(republican_top5_t10)

#HEATMAP
democrat_theta_t10 %>%
  gather(key = "topic", value = "probability", -document) %>%
  mutate(topic = factor(topic, levels = paste0("topic_", 1:10)),
         document = factor(document, levels = unique(document))) %>%
  ggplot(aes(x = topic, y = document, fill = probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "lightblue") +
  scale_y_discrete(breaks = function(x) x[seq(1, length(x), by = 20)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Topic", y = "Document", fill = "Probability")

republican_theta_t10 %>%
  gather(key = "topic", value = "probability", -document) %>%
  mutate(topic = factor(topic, levels = paste0("topic_", 1:10)),
         document = factor(document, levels = unique(document))) %>%
  ggplot(aes(x = topic, y = document, fill = probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_y_discrete(breaks = function(x) x[seq(1, length(x), by = 20)]) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Topic", y = "Document", fill = "Probability")

democrat_beta_t10 <- democrat_lda_t10 %>% 
  tidytext::tidy(matrix = "beta") 

head(democrat_beta_t10, 10)

republican_beta_t10 <- republican_lda_t10 %>% 
  tidytext::tidy(matrix = "beta") 

head(republican_beta_t10, 10)
#I am mostly doing this for the beta function 


democrat_top10_words_beta_t10 <-democrat_beta_t10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(democrat_top10_words_beta_t10)

republican_top10_words_beta_t10 <-republican_beta_t10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(republican_top10_words_beta_t10)
#top 10 words per each topic 

#BAR CHARTS
democrat_top10_words_beta_t10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_y_reordered() +
  labs(x = "Beta (Importance)", y = "Top 10 Words") +
  theme_minimal()

republican_top10_words_beta_t10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_y_reordered() +
  labs(x = "Beta (Importance)", y = "Top 10 Words") +
  theme_minimal()

#SENTIMENT ANALYSIS
data("data_dictionary_LSD2015")
# pre made dictionary model

democratsentiment_scores <- dfm_lookup(democrat_dfm.t,
                                         dictionary = data_dictionary_LSD2015)
head(democratsentiment_scores)


republicansentiment_scores <- dfm_lookup(republican_dfm.t,
                                       dictionary = data_dictionary_LSD2015)
head(republicansentiment_scores)
