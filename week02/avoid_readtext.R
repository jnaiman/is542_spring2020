# for the text file
#install.packages("readtext") # only need to run once, comment out once you have

# read and process data
#txtfile=readtext("~/Downloads/introAVoid.txt") # note: the "~/" here means "home directory"
# or try:
txtfile = readtext::readtext("~/Downloads/introAVoid.txt")
twords = strsplit(txtfile[,2],split=" ") # split by spaces into words
# dataframe => vector
words = twords[[1]]
# take out punctuation
for (i in 1:length(words)) {
  # look for specific punctuation
  # 1 => .
  if (grepl("\\.",words[i])){ # if this punctuation is in the word
    words[i] = gsub(".","",words[i]) # replace it with an empty character
  }
  # 2 => ,
  if (grepl(",",words[i])){
    words[i] = gsub(",","",words[i])
  }
  # 3 => )
  if (grepl("\\)",words[i])){
    words[i] = gsub(")","",words[i])
  }
  # 4 => (
  if (grepl("\\(",words[i])){
    words[i] = gsub("\\(","",words[i])
  }
}

# some possible useful functions
# (1) This will give the number of characters in a word
number_of_characters = nchar(words)

# (2) Below is an example of how to count occurances of a character in each word
#  feel free to change things
# (2.i) A useful function: define a function to find the number of a certain character
countCharOccurrences <- function(char, s) {
  if (length(char) > 1) { # we have a vector (words)
    v = rep(0,length(char))
    for (i in 1:length(char)) {
      s2 <- gsub(char[i],"",s)
      v[i] = (nchar(s) - nchar(s2))
    }
  } else { # other wise, we only have one word
    s2 <- gsub(char,"",s)
    v = (nchar(s) - nchar(s2))
  }
    return (v)
}

# (2.ii) example: count occurances of "a" and "b" in a word
num_a = countCharOccurrences("a",words)
# total number of "a"'s in this piece
total_a = sum(num_a)
num_b = countCharOccurrences("b",words)
# total number of "a"'s in this piece
total_b = sum(num_b)
# plot and compare
barplot(c(total_a,total_b),names.arg=c("a","b"),xlab="Character",ylab="Frequency in Text")