
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
#packages install

democrat_keyness <- textstat_keyness(democrat_dfm.t) 
head(democrat_keyness, n = 20)
#democrat keyness 

republican_keyness <- textstat_keyness(republican_dfm.t)
head(republican_keyness, n= 20)
#republican keyness 

