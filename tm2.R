## Some code for preprocessing and describing text data -- Example: 11 Trump Speeches

# Load libraries
library(tm)
library(grid)
library(wordcloud)
library(wordcloud2)
library(tidyverse)


# LOADING TEXTS
## FOR MAC:
texts <- file.path("~", "Desktop", "texts")
dir(texts) # inspect the texts

## FOR PC:
#texts <- file.path("C:", "texts")
#dir(texts)

# Now we can create our raw corpus, which we will preprocess in a moment
docs <- VCorpus(DirSource(texts))
summary(docs)

# read each document
writeLines(as.character(docs[1]))


#
# PREPROCESSING

# Start with punctuation
docs <- tm_map(docs, removePunctuation)
writeLines(as.character(docs[1])) # Check the corpus... did it work? 


# MAybe we need a bit more cleaning of unique characters
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("’", " ", docs[[j]])
  docs[[j]] <- gsub("—", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])  # an ascii character that does not translate
}
writeLines(as.character(docs[1])) # always inspect

# We can now update our recent update (minus punctuation) by redefining "docs" to remove numbers
docs <- tm_map(docs, removeNumbers)
writeLines(as.character(docs[1])) # inspect again

# For consistency, we may also want to remove captialization
docs <- tm_map(docs, tolower)
(docs <- tm_map(docs, PlainTextDocument))


# Next, remove superfluous words like articles or words with no substantive value for analysis
## For a list of the stopwords, run: stopwords("english")  
#  And you can use this to see how many there are (174): length(stopwords("english"))
docs <- tm_map(docs, 
               removeWords, 
               stopwords("english"))
docs <- tm_map(docs, PlainTextDocument) # redefine

# manually removing words too for your specific purposes (e.g., our "representative" or "honourable" example Tuesday)
docs <- tm_map(docs, removeWords, c("will")) # trump says "will" a ton (try leaving "will" in; most frequently used)

# There are some words that tm pulls apart that should stay together; manually define for each document, j:
for (j in seq(docs)) {
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("I m", "I'm", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument) # redefine docs

# We can also omit certain "stems" or common English word endings (e.g., ing, es)
docs_st <- tm_map(docs, stemDocument) # note that we are storing this in a new corpus to give ourselves some options for analysis later
docs_st <- tm_map(docs_st, PlainTextDocument) 

# Preprocessing leaves behind a lot of white space, or extra spaces between words or lines
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument) # final redefine for retaining the lataest preprocessing steps


#
# STAGING: "DocumentTermMatrix" from tm
# Create dtm (matrix of term frequencies per document); other packages also do this, e.g., qdap

# dtm: each document is a row, and each term is a column
dtm <- DocumentTermMatrix(docs)

# Or, transpose of a dtm == tdm
# --> tdm: each word is a row (frequency of usage), and each document is a column
tdm <- TermDocumentMatrix(docs)


#
# EXPLORE numerically
# with preprocessed and staged text, we can now explore trump's speeches

frequency <- sort(colSums(as.matrix(dtm)), 
                  decreasing=TRUE) # add number of times each term is used, and sorting based on frequency of usage
head(frequency) # most frequently used words

# what is the most common term used?
frequency[1]

# We can also tailor the search of terms by frequency used
findFreqTerms(dtm, lowfreq = 100) # narrows by words used more than 100 times (or whatever threshold you set), which are 14 words

# same thing, another way - verify that there are 14 words used over 100 times
wf <- data.frame(word = names(frequency), 
                 freq = frequency)
head(wf, 15)  # sure enough, the 15th word is used less than 100 times

# Export the corpus to a .csv if you don't want to repeat the steps above, and wish to use this in the future
#trump.speech.corpus <- as.matrix(dtm)
#write.csv(trump.speech.corpus, "trump.speech.corpus.csv")

# We can also explore relationships between word usage (pre-cursor to topic models) - e.g., what is the correlation of words being used together
findAssocs(dtm, 
           terms = "great", 
           corlimit = 0.90) # manually locate terms, and then specify the correlation threshold

findAssocs(dtm, 
           terms = c("great" , "america"), 
           corlimit = 0.90) # multiple words

findAssocs(dtm, 
           terms = "hell", 
           corlimit = c(0.75, 0.5)) # multiple correlation thresholds


#
# EXPLORE visually
## Visualize the frequencies as bar plots

# words used more than 100 times
words.100 <- ggplot(subset(wf, freq > 100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x = "Term")
words.100

# words used more than 75 times
words.75 <- ggplot(subset(wf, freq > 75), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=55, hjust=1)) +
  labs(x = "Term")
words.75

# words used more than 50 times
words.50 <- ggplot(subset(wf, freq > 50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Term")
words.50

# words used more than 35 times
words.35 <- ggplot(subset(wf, freq > 35), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x = "Term")
words.35

# view in a grid
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(words.100, vp = vplayout(1, 1))
print(words.75, vp = vplayout(1, 2))
print(words.50, vp = vplayout(2, 1))
print(words.35, vp = vplayout(2, 2))


## Wordclouds: larger size = greater usage
# back to minimum usage of 100 times - not too useful though, as wordclouds are better with many terms to see patterns
set.seed(2345) # specifies start/end, making configuration consistent for each plot
wordcloud(names(frequency), frequency, min.freq = 100)

# thus, let's visualize the 150 most frequently used terms
set.seed(2345)
wordcloud(names(frequency), frequency, max.words = 150)

# and of course, some color is good
# using brewer palette, we can first choose the color scheme we like best, and then specify it below in our word cloud
display.brewer.all(n = NULL, type = "all", select = NULL, exact.n = TRUE,
                   colorblindFriendly = FALSE)

# And to manually add a title and for a better-formatted WC...
layout(matrix(c(1, 2), nrow=2), heights=c(1, 6))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Trump Speeches Term Frequency Wordcloud")
# Now, generate and overlay the fully formatted wordcloud to result in a good visualization
wordcloud(names(frequency), frequency,
          min.freq = 1, # terms used at least once
          max.words = 300, # 300 most frequently used terms
          random.order = FALSE, # centers cloud by frequency, > = center
          rot.per = 0.30, # sets proportion of words oriented horizontally
          main = "Title",
          colors = brewer.pal(8, "Dark2")
          )  


# via wordcloud2 - animated
# options: 'circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', and 'star'
wordcloud2(wf,
           shape = "diamond")

#




## Some other useful text mining code -- Example: exploring css related papers submitted to the arXiv
library(aRxiv) # API interface
library(lubridate) # for time measures
library(tidyverse) # munging/stacking
library(skimr) # EDA

# SQL query of papers with "Computational Social Science" keyword(s)
css_papers <- arxiv_search(query = '"Computational Social Science"', 
                           limit = 100)

# Inspect the documents, e.g., title, abstract, authors, etc.
head(css_papers$title)
head(css_papers$abstract)
head(css_papers$authors)

# Define measures of time for EDA
css_papers <- css_papers %>% 
  mutate(submitted = ymd_hms(submitted),
         updated = ymd_hms(updated))

skim(css_papers)

# Make a corpus out of the abstracts; inspect the first paper's abstract
css_abstracts <- with(css_papers, 
                      VCorpus(VectorSource(abstract)))

css_abstracts[[1]] %>% # call first paper in the corpus
  as.character() %>% # character class
  strwrap() # makes it read like a paragraph

# Light preprocessing
css_abstracts <- css_abstracts %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords("english"))

css_abstracts[[1]] %>% # call first paper in the corpus
  as.character() %>% # character class
  strwrap() # makes it read like a paragraph

# Make a wordcloud
wordcloud(css_abstracts,
          max.words = 150,
          colors = brewer.pal(15, "BrBG"),
          random.color = TRUE)
