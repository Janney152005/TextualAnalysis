## =============================================================================
## Lab 3: Processing (Preprocessing) Text Data
## =============================================================================

# This lab walks through cleaning up raw text before analyzing it (e.g., removing
# punctuation and numbers, lowercasing, stripping stopwords, stemming) and,
# just as importantly, showing how each of those choices changes what you
# find. Every step is explained in plain language, jargon is defined the
# first time it shows up, and the lab runs end-to-end on a real dataset of
# song lyrics with Billboard chart data, so you can see real before/after
# results. By the way, you can use your own data instead (see Part 3)!
#
# How to use this file:
#   - Read top to bottom. Each "Part" builds on the one before it.
#   - Run code a few lines at a time (highlight + Cmd/Ctrl+Enter in RStudio),
#     not the whole file at once, so you can see what each step actually does.
#   - If something errors, don't panic and see Part 9, "When something breaks."
#
# Initially Created By Daniel Karell | Yale University | daniel.karell@yale.edu
# Adopted and Further Developed By Minsu Park | NYU Abu Dhabi | minsu.park@nyu.edu


## =============================================================================
## Part 0: A few words you'll see a lot
## =============================================================================

# CORPUS: a collection of documents treated as one object for text analysis.
#   Here, each "document" is one article, and the corpus is our whole set of
#   collected articles.
#
# TOKEN / TOKENIZATION: breaking text into individual pieces (usually words)
#   so a computer can count and compare them. "the cat sat" tokenizes into
#   three tokens: "the", "cat", "sat".
#
# DOCUMENT-TERM MATRIX (DTM): a table where each row is one document, each
#   column is one unique word ("term") that appears anywhere in the corpus,
#   and each cell is how many times that word appears in that document. This
#   is the central data structure behind most of what we do in this lab.
#
# TERM FREQUENCY: how many times a given word occurs, either within one
#   document or summed across the whole corpus.
#
# STOPWORDS: very common words ("the," "and," "of," ...) that show up in
#   almost every document and carry little topical meaning on their own.
#   They're often removed before analysis so they don't drown out more
#   informative words.
#
# STEMMING: cutting a word down to a shared "root" so related forms count as
#   the same word, e.g. "drink," "drinks," and "drinking" all become "drink."
#
# METADATA: information *about* each document (publication date, section,
#   word count, ...), as opposed to the text of the document itself.


## =============================================================================
## Part 1: What are we actually doing, and why does it matter?
## =============================================================================

# Raw text is messy: it has punctuation, numbers, inconsistent capitalization
# ("Computer" vs. "computer"), and different forms of the same word ("fly"
# vs. "flying"). Before we can meaningfully count or compare words across
# documents, we usually need to "preprocess" the text -- cleaning it up and
# making equivalent words actually match.
#
# The twist: preprocessing isn't just tidying up. Every choice you make
# (Do you remove stopwords? Which stopword list? Do you stem words?) can
# change which words look "important" in your data, and therefore can change
# your conclusions. This lab's main goal is to make that visible: we'll
# apply several different preprocessing pipelines to the same data and
# compare the results side by side.


## =============================================================================
## Part 2: Load the packages we need
## =============================================================================

# If you've never installed these before, run install.packages() once for
# each (uncomment the line below, run it, then you can comment it out again):
# install.packages(c("dplyr", "tm", "stm"))

library(dplyr)  # data-wrangling helpers like as_tibble()
library(tm)     # "text mining": corpus objects, cleaning functions, DTMs
library(stm)    # "structural topic model" package; we'll use its automatic
                # text preprocessor (textProcessor()) later in Part 8

# Clear your workspace so we start from a clean slate. Skip this line if you
# have other work open in your R session that you don't want to lose.
rm(list = ls())


## =============================================================================
## Part 3: Load your data
## =============================================================================

# We'll use a dataset of song lyrics with Billboard chart information
# ("20181013_lyrics_with_billboard_record.tsv"). If you're using a different 
# dataset (e.g., your own Lab 2 NYT collection), just point data_path at 
# that file instead, and adjust the sep argument and column names below to match.

data_path <- "C:/Users/muend/Desktop/Data Analysis/Textual Analysis/Dataset/20181013_lyrics_with_billboard_record.tsv"

if (!file.exists(data_path)) {
   stop("Can't find '", data_path, "'. Make sure the file is saved in the ",
        "toy_data folder, or change data_path above to point at wherever ",
        "your data lives.")
}

# This is a large file (~85MB, ~27,000 songs), so read.csv() may take a
# little while to load; that's normal, not a sign anything's broken.
data <- read.csv(data_path, sep = "\t", header = TRUE)
# or data <- read.tsv(data_path, header = TRUE)
# Let's take a look at the data.
print(as_tibble(data)) # as_tibble(data)
# How many observations (songs) are there in total?
print(paste("Initial row count:", nrow(data)))
# nrow(data)

# 3.1. Subsample the data, to keep the rest of this lab fast to run through.
# We take a *random* sample rather than just the first N rows, because this
# file is grouped by artist/album -- the first N rows would only capture a
# handful of artists, not a representative slice of the dataset.
set.seed(1)                # makes the "random" sample reproducible
sample_size <- 1000         # increase this later for a more thorough analysis
data <- data %>% slice_sample(n = sample_size)
print(paste("Sampled row count:", nrow(data)))

# nrow(data)                 # confirm we're now working with a smaller set

# The lyrics column is called "lyric." Let's create a "text" column with
# that same content, so the rest of this lab can refer to "text" regardless
# of which dataset you're using (e.g., Lab 2's NYT data uses different
# column names for its text fields).
data$text <- data$lyric

# 3.2. Let's start by cleaning up some text. Take a look at one song's text.
data$text[7]
# You'll probably notice the raw text looks like a Python list written out
# as a string -- e.g. ["line one", "line two", ...] -- rather than plain
# prose. That's an artifact of how this dataset was originally scraped. Our
# punctuation-removal step below will also incidentally strip out those
# brackets and quotation marks, which is a happy side effect here.


## =============================================================================
## Part 4: Basic text cleaning
## =============================================================================

example <- data$text[7]

# 4.1. Remove punctuation marks
example <- removePunctuation(example)
example

# 4.2. Remove numbers
example <- removeNumbers(example)
example

# 4.3. Make words easier to compare by making everything lower-case, so
# "Fashion" and "fashion" count as the same word.
example <- tolower(example)
example

# 4.4. Clean up extra white space left behind by the steps above
example <- stripWhitespace(example)
example

# 4.5. We can apply all four steps at once to an entire column of text (not
# just one article) -- each function works on a vector of strings just as
# well as on a single string.
text_clean <- removePunctuation(data$text)
text_clean <- removeNumbers(text_clean)
text_clean <- tolower(text_clean)
text_clean <- stripWhitespace(text_clean)
text_clean[7]   # compare to data$text[7] above -- same cleanup, whole column
text_clean[1:3]


## =============================================================================
## Part 5: Build a corpus, and see how preprocessing choices affect results
## =============================================================================

# There are many more preprocessing options than the four basic ones above,
# and some of them can meaningfully change our findings. To see that, we'll
# build a "corpus" ---a proper text-mining object--- and compare a few
# different cleaning pipelines against each other.

# 5.1. Create a corpus, one document per song. We already subsampled the
# data down to `sample_size` songs in Part 3, specifically so this and the
# later steps (DTMs, stemming, stm) run quickly for a live demonstration --
# if you want a more thorough analysis, raise sample_size in Part 3 and
# re-run from there (expect things to take noticeably longer).
corpus <- VCorpus(VectorSource(data$text))
corpus  # look at your corpus -- how many documents does it report?

# 5.2. Clean the text inside the corpus, the same four steps as Part 4, but
# using tm_map() to apply each function to every document in the corpus.
# content_transformer() is needed to wrap a plain R function (tolower) so
# tm_map knows how to apply it to documents rather than to a plain vector.
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)

# 5.3. Let's look at the most frequent words using a frequency table. We
# first convert our corpus to "PlainTextDocument" format (a step some
# versions of tm require before building a Document-Term Matrix), then
# build the DTM itself.
corpusPTD <- tm_map(corpus, PlainTextDocument)
dtm <- DocumentTermMatrix(corpusPTD)
termFreq <- colSums(as.matrix(dtm))
tf <- data.frame(term = names(termFreq), freq = termFreq)
tf <- tf[order(-tf[, 2]), ]
head(tf)
# What is the most frequent word? Does it look like a meaningful, topical
# word, or a very common one that shows up everywhere?


## =============================================================================
## Part 6: Removing stopwords
## =============================================================================

# Researchers often remove "stopwords" (very common words that carry
# little topical meaning) before counting or modeling. Let's try it and
# see whether it changes our top-words list.

corpus2 <- tm_map(corpus, removeWords, stopwords('english'))
corpusPTD2 <- tm_map(corpus2, PlainTextDocument)
dtm2 <- DocumentTermMatrix(corpusPTD2)
termFreq2 <- colSums(as.matrix(dtm2))
tf2 <- data.frame(term = names(termFreq2), freq = termFreq2)
tf2 <- tf2[order(-tf2[, 2]), ]
tf2[1:10, ]
cbind(tf[1:10, ], tf2[1:10, ])   # side-by-side: before vs. after stopword removal
# What is the most frequent word now? Why might we want to remove stopwords?

# 6.1. There are different dictionaries (lists) of stopwords, and they don't
# all agree. Let's look at a few.
stopwords(kind = "en")

# Here's an Arabic list from a different source than tm's default, for comparison:
head(stopwords::stopwords("ar", source = "marimo"), 20)

# 6.2. Let's try a different English stopword dictionary and see whether
# results change again.
stopwords(kind = 'SMART')
corpus3 <- tm_map(corpus, removeWords, stopwords('SMART'))
corpusPTD3 <- tm_map(corpus3, PlainTextDocument)
dtm3 <- DocumentTermMatrix(corpusPTD3)
termFreq3 <- colSums(as.matrix(dtm3))
tf3 <- data.frame(term = names(termFreq3), freq = termFreq3)
tf3 <- tf3[order(-tf3[, 2]), ]
tf3[1:10, ]
cbind(tf[1:10, ], tf2[1:10, ], tf3[1:10, ])
# How are these results different from the 'english' stopword list above?

# 6.3. You can also expand an existing stopword list, or build your own from
# scratch, to remove words that are common in YOUR dataset specifically but
# aren't in any general-purpose dictionary (e.g., a publication's own name,
# or a recurring boilerplate phrase).
custom_stopwords_seed <- c("context_specific_word_1", "context_specific_word_2")
custom_stopwords <- c(stopwords('english'), custom_stopwords_seed)
# To actually use it: corpus_custom <- tm_map(corpus, removeWords, custom_stopwords)


## =============================================================================
## Part 7: Stemming
## =============================================================================

# We can also "stem" words, keeping only their root, so that e.g., "drink"
# and "drinking" count as the same word.
corpus4 <- tm_map(corpus3, stemDocument)
corpusPTD4 <- tm_map(corpus4, PlainTextDocument)
dtm4 <- DocumentTermMatrix(corpusPTD4)
termFreq4 <- colSums(as.matrix(dtm4))
tf4 <- data.frame(term = names(termFreq4), freq = termFreq4)
tf4 <- tf4[order(-tf4[, 2]), ]
tf4[1:10, ]
cbind(tf[1:10, ], tf2[1:10, ], tf3[1:10, ], tf4[1:10, ])
# Look at a stemmed word that looks unfamiliar or truncated. Why does it
# look like that? How do these results differ from the preceding ones?


## =============================================================================
## Part 8: Automatic preprocessing (the stm package)
## =============================================================================

# Some packages preprocess your text automatically, behind the scenes. This
# is important to realize when you use a package for a different purpose
# (like topic modeling) and it happens to clean your text along the way. The
# stm package is one example: it lets you combine text data and metadata
# (information about each document), and it preprocesses the text for you
# in the process.

as_tibble(data)  # reminder of what metadata we have available (release_date, artist, billboard, etc.)

# 8.1. Run stm's automatic preprocessor. (We're reusing the same subsampled
# `data` from Part 3 here, rather than taking a separate subset, to keep
# this section consistent with the pipelines built above.)
processed <- textProcessor(data$text, metadata = data)

# 8.2. It preprocessed the data automatically -- but what exactly did it do?
# The console output gives a summary, but for the full details, check R's
# built-in help, or the package's own documentation website:
?textProcessor
browseURL("https://www.structuraltopicmodel.com/")

# 8.3. In the help window, read the explanation for the 'removestopwords'
# argument. It lets us prevent stopword removal, in case we want to. Let's
# try it. (Function arguments like this are how you adjust what a function
# actually does.)
processed2 <- textProcessor(data$text, metadata = data, removestopwords = FALSE)

# 8.4. Compare stm's automatic preprocessing against our own manual pipelines.
termFreq_auto <- colSums(as.matrix(convertCorpus(processed$documents, processed$vocab, type = "Matrix")))
tf_auto <- data.frame(term = names(termFreq_auto), freq = termFreq_auto)
tf_auto <- tf_auto[order(-tf_auto[, 2]), ]
head(tf_auto)
cbind(tf[1:10, ], tf2[1:10, ], tf3[1:10, ], tf4[1:10, ], tf_auto[1:10, ])

termFreq_auto2 <- colSums(as.matrix(convertCorpus(processed2$documents, processed2$vocab, type = "Matrix")))
tf_auto2 <- data.frame(term = names(termFreq_auto2), freq = termFreq_auto2)
tf_auto2 <- tf_auto2[order(-tf_auto2[, 2]), ]
head(tf_auto2)
cbind(tf[1:10, ], tf2[1:10, ], tf3[1:10, ], tf4[1:10, ], tf_auto[1:10, ], tf_auto2[1:10, ])
# Does stm's automatic preprocessing match any of our manual pipelines
# exactly? Where does it agree or disagree?


## =============================================================================
## Part 9: When something breaks (read this before panicking)
## =============================================================================

# "could not find function ..."
#   -> You skipped a library(...) line, or a package isn't installed yet.
#      Run install.packages("thatPackageName"), then library(thatPackageName).
#
# "cannot open file 'toy_data/20181013_lyrics_with_billboard_record.tsv'"
#   -> R is looking in the wrong working directory, or the file isn't in
#      the toy_data folder. Run getwd() to see where R is currently
#      looking. You can also set data_path in Part 3 to an absolute path
#      (e.g., "/Users/you/.../toy_data/20181013_lyrics_with_billboard_record.tsv")
#      to sidestep the issue.
#
# read.csv() takes a long time, or R feels frozen right after loading
#   -> This file has ~27,000 rows, so the first read.csv() call can take a
#      little while -- give it a minute before assuming something's wrong.
#      If it's still unresponsive after that, check the Console for an
#      error rather than re-running the line (which restarts the slow read).
#
# stemDocument() errors, or silently doesn't seem to do anything
#   -> stemDocument() relies on the SnowballC package under the hood. Run
#      install.packages("SnowballC") and try again.
#
# DocumentTermMatrix() / colSums() errors with something like "Error in
# colSums(...) : 'x' must be an array of at least two dimensions"
#   -> Your corpus is probably empty after cleaning -- e.g., every document
#      became a blank string after stripping punctuation/numbers/stopwords.
#      Check a document's content directly: corpus[[1]]$content
#      (or corpusPTD[[1]]$content) to see what's actually left.
#
# tolower() or removePunctuation() errors on your own data
#   -> Check that your text column is actually character data, not a
#      factor or something else: class(data$text). If it isn't character,
#      convert it first: data$text <- as.character(data$text).
#
# textProcessor() prints "Removing X of Y documents..."
#   -> That's expected, not an error: stm's automatic preprocessing drops
#      documents that end up empty after its own cleaning (e.g., a very
#      short snippet made entirely of stopwords/numbers).
#
# General debugging habit: run ONE line at a time and print the result
# (just type the variable's name and hit enter) so you can see exactly what
# each step produced, rather than running 20 lines and guessing which one
# broke.


## =============================================================================
## Part 10: Notes for students -- why does this matter in the era of AI?
## =============================================================================

# It might seem like preprocessing is a solved problem in the age of large
# language models; you can just hand raw text to an LLM and get an answer
# back. A few reasons it's still worth understanding what's happening under
# the hood:
#
# 1. Modern LLMs and embedding models often skip classical preprocessing
#    (stemming, stopword removal) entirely as they work directly on raw
#    text. Knowing what classical preprocessing does, and why, helps you
#    judge when that's actually fine (e.g., a general-purpose chatbot
#    query) and when you still need an explicit, interpretable pipeline
#    like the one in this lab (e.g., a reproducible word-frequency analysis
#    for a paper, or a keyword-based measure you need to defend and explain).
#
# 2. Preprocessing is often invisible unless you go looking for it. stm's
#    textProcessor() in Part 8 quietly made its own choices about
#    stopwords, case, punctuation, and numbers before you saw any results.
#    Any tool (including an R package, a Python library, an AI assistant) that
#    "just handles" your text is making preprocessing decisions on your
#    behalf. This lab is training you to notice that and check what those
#    defaults actually are, rather than trusting them blindly.
#
# 3. Garbage in, garbage out applies doubly when AI is in the loop. If you
#    feed a poorly preprocessed corpus into an LLM-based classifier or
#    summarizer, you often can't tell whether a bad result came from the
#    model or from what your preprocessing did to the input. Being able to
#    inspect corpus[[1]]$content or a DTM directly (as in this lab) gives
#    you a way to rule that out.
#
# 4. Preprocessing choices are research decisions, and reproducibility
#    depends on documenting them precisely. The tf/tf2/tf3/tf4 comparisons
#    in this lab show that "remove stopwords" vs. "don't," or "stem" vs.
#    "don't," can change which words look most important. "I asked an AI
#    to clean the text" is not a reproducible method statement in the way
#    "I removed English stopwords using tm's default list, then stemmed
#    with Porter stemming" is.
#
# 5. The debugging skill transfers directly. If you ask a coding assistant
#    to write or modify a preprocessing pipeline, understanding what
#    tm_map(), content_transformer(), and DocumentTermMatrix() actually do
#    lets you catch subtle, easy-to-miss bugs like removing stopwords
#    before lowercasing (so "The" survives removal but "the" doesn't) or
#    forgetting content_transformer() when wrapping a custom cleaning
#    function that would otherwise silently skew your results.


## =============================================================================
## Part 11: Assignment - Preprocessing and Corpus Changes
## =============================================================================

# Task: Explore how different preprocessing choices shape your corpus, and
# reflect on what these choices mean for research. Submit a PDF only, with
# figures/tables and interpretation. No code in the submission.

# 1. Corpus setup
#    - Use either the Billboard lyrics dataset from this lab, or your own
#      collected dataset from Lab 2 (or another textual dataset from this
#      course). If you use the lyrics data, feel free to raise sample_size
#      in Part 3 beyond the small demo value used above.
#    - Document simple descriptive facts: number of documents, total word
#      count (raw, before any cleaning), etc.

# 2. Compare preprocessing pipelines
#    Apply at least three different preprocessing variants. Examples include
#    (not limited to):
#      (a) Basic cleaning only (punctuation, numbers, lowercase, whitespace).
#      (b) With stopwords removed (choose one stopword list, e.g., English).
#      (c) With stemming or lemmatization.
#    For each pipeline, report: top 15 terms, and DTM size (documents x terms).

# 3. Visualize
#    - Produce at least two visualizations (e.g., side-by-side bar plots of
#      top terms, frequency-change charts, word clouds).
#    - Highlight at least two words or features that change significantly
#      depending on preprocessing choice.

# 4. Interpretation
#    Write 1-2 paragraphs (6-10 sentences) addressing:
#      - How did preprocessing alter the vocabulary distribution?
#      - Which approach seems most appropriate for your project, and why?
#      - What risks are introduced by aggressive cleaning (e.g., stemming,
#        stopword removal)?

# Submission
#    - PDF only.
#    - Include: corpus description, figures/tables, and interpretation.
#      No code included.
