#Load the libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Load the resume file as a txt file
resume <- readLines(file.choose())

resume_file <- Corpus(VectorSource(resume))

#Write a function to perform replace of special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

resume_file <- tm_map(resume_file, toSpace, "/")
resume_file <- tm_map(resume_file, toSpace, "@")
resume_file <- tm_map(resume_file, toSpace, "\\|")

# Convert the document to lower case
resume_file <- tm_map(resume_file, content_transformer(tolower)) 

# Remove numbers from the document 
resume_file <- tm_map(resume_file, removeNumbers)

# Remove StopWords from the document
resume_file <- tm_map(resume_file, removeWords, stopwords("english"))

# Remove WhiteSpaces
resume_file <- tm_map(resume_file, stripWhitespace)

# Build a Document Matrix from the file
dtm <- TermDocumentMatrix(resume_file)
m <- as.matrix(dtm) 
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Build the Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
