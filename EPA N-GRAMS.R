library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)

#N GRAM EXAMPLE
democrat_toks_bigram <- democrat_corpus %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>% 
  tokens_ngrams(n = 2)

republican_toks_bigram <- republican_corpus %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>% 
  tokens_ngrams(n = 2)

head(democrat_toks_bigram[[1]], 30)
head(republican_toks_bigram[[1]], 30)
#these are largely unhelpful as they are formalities 

#SELECTING DIFFERENT WORDS 
enviro_democrat_bigram_select <- tokens_select(democrat_toks_bigram, pattern = phrase("*_environment"))
head(enviro_democrat_bigram_select, 10)
#"health_environment","protecting_environment","Stewardship_environment","priorities_environment"
enviro_republican_bigram_select <- tokens_select(republican_toks_bigram, pattern = phrase("*_environment"))
head(enviro_republican_bigram_select, 10)
#"health_environment", "protect_environment", "nation's environment", "america's environment"

energy_democrat_bigram_select <- tokens_select(democrat_toks_bigram, pattern = phrase("*_energy"))
head(energy_democrat_bigram_select, 10)
#"source energy", "cleaner_energy"
energy_republican_bigram_select <- tokens_select(republican_toks_bigram, pattern = phrase("*_energy"))
head(energy_republican_bigram_select, 10)
#"clean energy", "promote energy" 

climate_republican_bigram_select <- tokens_select(republican_toks_bigram, pattern = phrase("*_climate"))
head(climate_republican_bigram_select, 10)
#involves climate, us climate 

climate_democrate_bigram_select <- tokens_select(democrat_toks_bigram, pattern = phrase("*_climate"))
head(regulate_democrate_bigram_select, 10)
# address climate, change climate, confronting climate, urgency climate 





