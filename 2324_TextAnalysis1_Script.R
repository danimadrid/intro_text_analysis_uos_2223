######################################
#           Text Analysis 1          #
#   prepared by Dani Madrid-Morales  #
#      University of Sheffield       #
#          22 November 2023          #
######################################

#### Demo 0: Setting up ####

#install.packages("pacman") # This installs pacman locally
library(pacman) # This loads pacman to this R session
# This installs and loads a bunch of packages
p_load(quanteda, tidyverse, magrittr, readtext)

#### Demo 1: Introduction to quanteda ####

# Import text data from CSV file to R
df_abstracts <- readtext(file = "data/housing_scopus.csv", 
                         text_field = "Abstract")

# Remove rows that have missing values
df_abstracts %<>%
  drop_na(united_states) %>%
  drop_na(united_kingdom)

# Create a corpus with quanteda
hs_corpus <- corpus(df_abstracts)

# We can see what's inside our corpus using the `summary` command
summary(hs_corpus, 3)

# You could change the unit of analysis (defaults to "document") to sentences
hs_sent_corpus <- corpus_reshape(hs_corpus, to = 'sentences')
ndoc(hs_sent_corpus) # How many "documents" in the sentence-level corpus
ndoc(hs_corpus) # How many "documents" in the abstract-level corpus
summary(hs_sent_corpus, 3)

#### Demo 2: Pre-processing text data ####

# 1 - Tokenize corpus & remove punctuation
hs_tokens <- tokens(hs_corpus, 
                    remove_punct = TRUE,
                    remove_numbers = TRUE,
                    remove_symbols = TRUE, 
                    remove_url = TRUE, 
                    padding = TRUE) # For even more options, see ?tokens 
head(hs_tokens[[7]], 20) # Gives me 20 tokens from the seventh document in corpus

# 2- Lowercase the corpus
hs_lower_tokens <- tokens_tolower(hs_tokens)
head(hs_lower_tokens[[7]], 20)

# 3 - Remove stopwords
# Prints a list of available sources for stopwords
stopwords_getsources()

# Prints a list of languags for a given source
stopwords_getlanguages("marimo")
stopwords("en", "nltk")
stopwords("en", "smart")

# Exclude words from stopwords list "nltk - en"
hs_tokens_no_stopwords <- tokens_remove(hs_lower_tokens, 
                                        stopwords("en", "nltk"))
head(hs_lower_tokens[[7]], 20) # with stopwords
head(hs_tokens_no_stopwords[[7]], 20) # without stopwords

# Create a list of words to exclude
words_to_exclude <- c("taylor", "francis", "group")

# Exclude words from custom made list
hs_tokens_no_stopwords <- tokens_remove(hs_tokens_no_stopwords, words_to_exclude)

# 4 - Stemming your tokens object
hs_tokens_stemmed <- tokens_wordstem(hs_tokens_no_stopwords)
head(hs_tokens_stemmed[[7]], 20)

# DFM fromm a lemmatized tokens object
hs_dfm_stemmed <- dfm(hs_tokens_stemmed)
hs_dfm_stemmed
ndoc(hs_dfm_stemmed) # Gives us the number of documents
nfeat(hs_dfm_stemmed) # Gives us the number of (unique) features

# Most frequently occurring words
topfeatures(hs_dfm_stemmed, 20)

# We compute keyness using the variable united_states
# as the grouping variable
tstat_key <- textstat_keyness(hs_dfm_stemmed, 
                              docvars(hs_dfm_stemmed, "united_states") > 0)
textplot_keyness(tstat_key)

head(tstat_key, 10) # Words strongly associated with US-based scholars
tail(tstat_key, 10) # Words strongly associated with non US-based scholars

#### Demo 3: Dictionary analysis ####

# 1 - Create a corpus
# 2 - Tokenise the corpus & pre-process texts
# 3 - Identify a dictionary
dict_countries <- data_dictionary_newsmap_en
dict_countries

# 4 - Apply the dictionary to the dataset
countries_abstract <- tokens_lookup(hs_tokens, 
                                    dictionary = dict_countries, 
                                    levels = 3)

# 5 - Summarise the instances
dfm_countries <- dfm(countries_abstract)
dfm_countries

# The function topfeatures can summarise the data for us
topfeatures(dfm_countries, 10)

# We can easily compare groups
topfeatures(dfm_countries, 10, groups = united_states)

# We transform the DFM of country mentions to a data.frame format
df_countries <- convert(dfm_countries, to = "data.frame")

# We merge this information with our original dataset
df_abstracts %<>%
  left_join(df_countries, by = "doc_id")

# Bonus Points: We could plot a heatmap to visualise our findings
country_counts <- as.data.frame(topfeatures(dfm_countries, 242))
country_counts$id <- toupper(rownames(country_counts))
colnames(country_counts) <- c("frequency", "id")
rownames(country_counts) <- NULL
country_counts

# Next, we need to import geographic data to plot the map
world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region) # convert contry name to ISO code

# We can plot the frequency of countries wtih the geom_map function
ggplot(country_counts, aes(map_id = id)) + 
  geom_map(aes(fill = frequency), map = world_map) + 
  expand_limits(x = world_map$long, y = world_map$lat) + 
  scale_fill_viridis_c(option = "A", 
                       name = "Frequency",
                       direction = -1) + 
  theme_void() + 
  coord_fixed() + 
  ggtitle("Countries Mentioned in Housing Studies Abstracts (2000-2023)")