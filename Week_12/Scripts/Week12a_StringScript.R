# Lecture 12a 
# Last edited 04/19/21
install.packages('tidytext')
install.packages('wordcloud2')
install.packages('janeaustenr')
install.packages("stopwords")


library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)

# to make a string
words<-"This is a string"
words

# you can have several strings in a vector
words_vector<-c("Apples", "Bananas","Oranges")
words_vector

######Manipulation#####

# to paste words together, use paste
paste("High temp", "Low pH")

# add a dash in between the words
paste("High temp", "Low pH", sep = "-")

# to remove the space in between te words
paste0("High temp", "Low pH")

# Working with vectors
shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

# another example
two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

####Manipulation: individual character####
shapes # vector of shapes
str_length(shapes) # how many letters are in each word?
# if you have a sequence and want to extract certain area 
seq_data<-c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) # extract the 2nd to 4th AA
# you can add characters to certain parts of the string
# to add A to the third position
str_sub(seq_data, start = 3, end = 3) <- "A" # add an A in the 3rd position
seq_data
str_dup(seq_data, times = c(2, 3)) # times is the number of times to duplicate each string

####Whitesspace####
badtreatments<-c("High", " High", "High ", "Low", "Low")
badtreatments
# to remove white spaces
str_trim(badtreatments) # this removes both
# you can also just remove from one side or the other
str_trim(badtreatments, side = "left") # this removes left
# string pad will add white spaces
str_pad(badtreatments, 5, side = "right") # add a white space to the right side after the 5th character
# if you just want to make everything 5 characters
# the 5 tells you how many characters that you want to have in the string 
str_pad(badtreatments, 5, side = "right", pad = "1") # add a 1 to the right side after the 5th character

### Locale sensitive####
# make everything uppercase
x<-"I love R!"
str_to_upper(x)
# make it lowercase
str_to_lower(x)

####Pattern Matching####
data<-c("AAA", "TATA", "CTAG", "GCTT")
# find all the strings with an A
str_view(data, pattern = "A")
# in order to detect a specific pattern
str_detect(data, pattern = "A")
str_detect(data, pattern = "AT")
# locate a pattern
str_locate(data, pattern = "AT")

## regex: regular expressions#####
#metacharacters
vals<-c("a.b", "b.c","c.d")
# to replace all of the periods with a space 
#string, pattern, replace
str_replace(vals, "\\.", " ")

# initially, this only replaces the first one
vals<-c("a.b.c", "b.c.d","c.d.e")
#string, pattern, replace
str_replace(vals, "\\.", " ")
# to use string replace all to make a space before and after
#string, pattern, replace
str_replace_all(vals, "\\.", " ")

##Sequences####
# to escape the character, we need to put the two slashes before the better
val2<-c("test 123", "test 456", "test")
str_subset(val2, "\\d")

##Character class ####
# if I want to count all the instances of a vowel
str_count(val2, "[aeiou]")
# count any digit
str_count(val2, "[0-9]")

# example
strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")
# I want to extract all of these numbers and clean them 
# a phone number pattern is three - three - three 
# Make a regex that finds all the strings that contain a phone number. 
# We know there is a specific pattern (3 numbers, 3 numbers, 4 numbers and it can have either a "." or "-" to separate them). 
# Let's also say we know that the first number cannot be a 1
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
# Which strings contain phone numbers?
str_detect(strings, phone)
# subset only the strings with phone numbers
test<-str_subset(strings, phone)
test

# subset only the strings with phone numbers
test<-str_subset(strings, phone)
test


#[1] "550-153-7578"       "435.114.7586"       "home: 672-442-6739"

test%>%
  str_replace_all(pattern = "\\.", "-")%>%
  str_remove_all(pattern = "\\:", "")

# correct answer
test %>%
  str_replace_all(pattern = "\\.", replacement = "-") %>% # replace periods with -
  str_replace_all(pattern = "[a-zA-Z]|\\:", replacement = "") %>% # remove all the things we don't want
  str_trim() # trim the white space

## tidytext####
# explore it
head(austen_books())
# every row is an individual line 
# do NOT view, just use head
tail(austen_books())
# lets clean it up and add a column for line and chapter
original_books <- austen_books() %>% # get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # count the chapters (starts with the word chapter followed by a digit or roman numeral)
                                                 ignore_case = TRUE)))) %>% #ignore lower or uppercase
  ungroup() # ungroup it so we have a dataframe again
# don't try to view the entire thing... its >73000 lines...
head(original_books)
tail(original_books)
# we will want to clean this so that there is only one word per row so its tidy. 
# In tidytext each word is referred to as a token. The function to unnest the data so that its only one word per row is unnest_tokens()
tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) # add a column named word, with the input as the text column
head(tidy_books) # there are now >725,000 rows. Don't view the entire thing!
#see an example of all the stopwords
head(get_stopwords())
cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) # dataframe without the stopwords
head(cleaned_books)
#lets count the most common words
cleaned_books %>%
  count(word, sort = TRUE)
# how would we modify this code to only count the most popular words by book
cleaned_books%>%
  group_by(book, chapter)
# look at the different lexicons
sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% # only keep pos or negative words
  count(word, sentiment, sort = TRUE) # count them
head(sent_word_counts)

# lets plot it
sent_word_counts %>%
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it goes from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

# make a wordcloud
# this one is interactive
words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:100) #take the top 100
wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words

