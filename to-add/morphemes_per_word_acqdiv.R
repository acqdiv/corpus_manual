#################################################
# Computing means, standard deviations, and modes 
# of the number of morphemes per word across
# languages
#################################################

# Load the data
load("./Documents/Projects/ACQDIV/acqdiv_corpus_2018-06-19.rda")
load('/Users/stiv/Github/acqdiv/acqdiv/database/acqdiv_corpus_2018-06-19.rda')

# Count the number of morphemes in each word of ACQDIV
counts = data.frame(word_id = names(table(morphemes$word_id_fk)), morpheme_cts = as.numeric(table(morphemes$word_id_fk)))

# Inspect the results
head(counts) # Looking good

# Merge the counts back into the words
word_plus_counts = merge(words, counts, by = "word_id")

# Get the unique words, along with their counts and languages
tallying_table = unique(word_plus_counts[,c("word", "morpheme_cts", "language")])

# Compute the means, sds
means = tapply(tallying_table$morpheme_cts, tallying_table$language, mean)
sds = tapply(tallying_table$morpheme_cts, tallying_table$language, sd)

# Function for computing mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Compute mode
modes = tapply(tallying_table$morpheme_cts, tallying_table$language, Mode)

# Plot means
## Define colors based on (impressionistic interpretation of) 
## slopes from variation set analysis: green = positive, red = 
## negative, blue = neutral, grey = not attested
cols = c("darkgreen", "darkgreen", "darkgreen", "darkgreen", "blue", "red", "grey80", "grey80", "blue", "red")
dotchart(sort(means, decreasing=T), names(sort(means, decreasing=T)), color=cols, xlab = "Mean number of morphemes", main = "Green = pos. slope, Red = neg. slope, Blue = neutral, Grey = not attested")
