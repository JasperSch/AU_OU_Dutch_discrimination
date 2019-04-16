#Jasper Schelfhout
#assignment AMSA
#Classifying Dutch words with "au" and "ou"
#
#words are from woorden.org
#exluded words with "iou" or "you" while downloading, they were all english anyway

library(stringr)
library(readr)
library(rpart)
library(rpart.plot)
library(ade4)
au_raw <- read_csv("Woorden_met_au.csv")
ou_raw <- read_csv("Woorden_met_ou.csv")

#                               $$$ Preparation of the dataframe $$$
######################################################################################################

############################
#filtering words functions #
############################

#function to return list of only words that are completely lower case in a list(Eigennamen etc zijn waarschijnlijk niet classifiseerbaar)
#use on raw list
getLowerCase <- function(wordlist){
  return(wordlist[grepl("[A-Z]",wordlist)== FALSE])
}

#function to return only words without eau of a list
#Not deleting words that have an occurences of both 'au' and 'eau' in them!
#use on raw list
getWithoutEAU <- function(wordlist){
  return(wordlist[str_count(wordlist, "au")- str_count(wordlist, "eau") > 0])
}

#function to exlude words starting with "au ", this are mainly French and have therefore wrong sound (ex: au bain-marie)
#use on raw list
getWithoutAUspace<- function(wordlist){
  return(wordlist[grepl("au .",wordlist)== FALSE])
}

#function to replace au with _ in a list, takes care of eau's
replaceAUBy_ <- function(wordlist){
  x = gsub("au", "_", wordlist)
  return(gsub("e_", "eau", x)) #to not hit 'eau's
}

#function replace ou with _ in a list
replaceOUBy_ <- function(wordlist){
  x = gsub("ou", "_", wordlist)
  return(x)
}

#function to cut of 's as last two characters and remove duplicate words obtained by doing this
cutPluralS <- function(wordlist){
  x = gsub("\`s", "", wordlist)
  x = gsub("\'s", "", x) #symbol used might differ
  return(unique(x)) # unique to also deal with twice occuring words now
}

#function to multiply word occurences with multiple "_" and return a list with all these words seperate containing the replacement
#example: wordlist = c("n_w', 'p_wp_wp_w'), replacement = "au" --> c("n_w', "pauwp_wpauw", "p_wpauwpauw", "pauwpauw_w")
#use on list where au or ou have already been replaced by "_"
separateMultiple <- function(wordlist, replacement){
  loc = str_locate_all(pattern ="_", wordlist)
  amount = sapply(loc, function(x)length(unlist(x))/2)
  df = as.data.frame(cbind(wordlist, amount), stringsAsFactors = FALSE)
  single = subset(df, subset= amount== 1)[,1]
  multiple = cbind(subset(df, subset = amount > 1), rownb = which(df$amount > 1))
  multiple_split = vector()
  j=1
  for (i in 1:length(multiple$wordlist)){
    nb = as.integer(multiple$amount[i])
    x = unlist(loc[multiple$rownb[i]])
    positions = x[1:nb]                   #select starting positions
    for (pos in positions){
      word = multiple$wordlist[i]
      substr(word, pos,pos) <- "§"        #random character that should not occur in any word
      word = gsub("_", replacement, word) #replace all "_" with replacement
      word = gsub("§", "_", word)         #back replacement of random character
      multiple_split[j]= word
      j=j+1
    }
  }
  list_split = c(single, multiple_split)
  return(list_split)
}

#function to exclude French words (difficult to optimize, just excludes typical French groups/characters)
#use after '_' replacement
excludeFrench <- function(wordlist){
  x= wordlist[grepl("é", wordlist) == FALSE]
  x= x[grepl("ê", x) == FALSE]
  x= x[grepl("è", x) == FALSE]
  x= x[grepl("à", x) == FALSE]
  x= x[grepl("_i", x) == FALSE]
  x= x[grepl("ç", x) == FALSE]
  return(x)
}

#function to subset words with max length of x since long words are fusions and rules are most important for simple words for children.
keepMaxLength <- function (wordlist, x){
  return(wordlist[nchar(wordlist) < x])
}

#Main function to filter au words
filterListAu <- function (wordlist){
  x = getLowerCase(wordlist)
  x = getWithoutEAU(x)
  x = getWithoutAUspace(x)
  x = replaceAUBy_(x)
  x = cutPluralS(x)
  x = separateMultiple(x, "au")
  x = excludeFrench(x)
  x = keepMaxLength(x, 12)
  return(x)
}

#Main function to filter ou words
filterListOu <- function(wordlist){
  x = getLowerCase(wordlist)
  x = replaceOUBy_(x)
  x = cutPluralS(x)
  x = separateMultiple(x, "ou")
  x = excludeFrench(x)
  x = keepMaxLength(x, 12)
  return(x)
}


###########################################
#Dataframe building functions             #
###########################################

#all functions are written assuming that the first collumn of the dataframe will be the wordlist.
#all functions return collumns with information that can be added to the dataframe.

#function to add collumn with integer number place of au counted from the front
getPlaceFront <- function(df){
  y= sapply(df[,1], function(x) unlist(strsplit(x, "")))
  place = unname(sapply(y, function(x) match(c("_"),x), USE.NAMES = FALSE))
  return(place)
}

#function to add collumn with integer number place of au counted from the back
getPlaceBack <- function(df){
  y= sapply(df[,1], function(x) unlist(strsplit(x, "")))
  place = unname(sapply(y, function(x) match(c("_"),rev(x)), USE.NAMES = FALSE))
  return(place)
}

#function to returning collumns with the x characters before and after _
#specify as amount how many characters you want to have
getSuroundCharacters <- function (df, amount){
  front = gsub("_.*", "", df[,1])
  back = gsub(".*_", "", df[,1])
  m = as.data.frame(matrix(nrow = length(front), ncol=amount*2))
  namesf=c()
  namesb=c()
  for(i in 1:amount){
    f <- paste("f", amount+1-i, sep = "")
    b <- paste("b", i, sep = "")
    m[,amount+1-i] = sapply(front, function(x) str_sub(x,-i,-i))
    m[,i+amount]= sapply(back, function(x) str_sub(x,i,i))
    namesf=c(namesf, f)
    namesb=c(namesb, b)
  }
  names = c(namesf, namesb)
  colnames(m) = names
  return (m)
}

#funtion to get length of word
getLength <- function(df){
  return(sapply(df[,1], function(x) nchar(x)))
}

#function to get multiple front character
getGroupFront = function(df, amount){
  front = gsub("_.*", "", df[,1])
  return(sapply(front, function(x) str_sub(x,-amount,-1)))
}

#function to get multiple back of character
getGroupBack = function(df, amount){
  back = gsub(".*_", "", df[,1])
  return(sapply(back, function(x) str_sub(x,1,amount)))
}

#function to get last letter of a word

#function to say wether or not a letter is a vowel
isVowel <- function(df) {
  vowels <- c("a", "e", "i", "o", "u")
  n <- colnames(df)
  x <- replace(vector(mode="character", length=length(n)), 1:length(n), "_is_vowel")
  names = paste(n,x, sep="")
  df[] <- as.matrix(df) %in% vowels
  d = as.data.frame(df)
  colnames(d) = names
  return (d)
}
#...

#function to make an informative dataframe from a list of words
makeDf <- function(wordlist){
  df = data.frame(word = wordlist, stringsAsFactors = FALSE)
  df = cbind(df, pos_f = getPlaceFront(df))
  df = cbind(df, pos_b = getPlaceBack(df))
  df = cbind(df, getSuroundCharacters(df, 5))
  df = cbind(df, isVowel(df[,4:13]))
  df = cbind(df, length = getLength(df))
  df = cbind(df, f_2c = getGroupFront(df,2))
  df = cbind(df, f_3c = getGroupFront(df,3))
  df = cbind(df, f_4c = getGroupFront(df,4))
  df = cbind(df, b_2c = getGroupBack(df,2))
  df = cbind(df, b_3c = getGroupBack(df,3))
  df = cbind(df, b_4c = getGroupBack(df,4))
  return(df)
}


####################################################
#Actual using of functions + R specific fine tuning#
####################################################

au_filtered = filterListAu(au_raw$word)
ou_filtered = filterListOu(ou_raw$word)
x_au <- replace(vector(mode="character", length=length(au_filtered)), 1:length(au_filtered), "au")
x_ou <- replace(vector(mode="character", length=length(ou_filtered)), 1:length(ou_filtered), "ou")

df_au = cbind(makeDf(au_filtered), class = x_au)
df_ou = cbind(makeDf(ou_filtered), class = x_ou)
df_full = rbind(df_au, df_ou)

#Adjust collumns to right class (To make use of alphabetical order in rules)
for (i in 4:13){
  df_full[,i] = as.ordered(df_full[,i])
}



#make dummy variables
for(i in c(4:13, 25:30)){
x = acm.disjonctif(df_full[i])
x[] <- x > 0
df_full = cbind(df_full,x)
}

#specific preparation for rpart
df_full = cbind(df_full[31], cbind(df_full[,2:24],df_full[,32:length(df_full)]))
colnames(df_full)= gsub("`", "shorthandSymbol",colnames(df_full)) #prevents an error


#Option to save dataframe
#write.csv(df_full, file="dataset_OU_AU.csv")



#                                 $$$ DATA ANALYSIS $$$
##################################################################################################

#########################
#Exploratory plots      #
#########################
plot(df_full$pos_f)
plot(df_full$f1) #you can see almost no vowels can come in front
title("Character in front of [au] sound")
plot(df_full$b1) #t, w and d are very common to come behind the [au]
title("Character behind [au] sound")

par(mfrow= c(5,10))
for(i in 9:13){
  for(j in 4:13){
  mOU = table(df_full[,i][df_full$class=="ou"], df_full[,j][df_full$class=="ou"])
  mAU = table(df_full[,i][df_full$class=="au"], df_full[,j][df_full$class=="au"])
  m = mOU - mAU
  image(1:ncol(m), 1:nrow(m), t(m), col = gray.colors(50, 0.1, 0.98), axes = FALSE, xlab=NA, ylab=NA)
  axis(1, 1:ncol(m), colnames(m))
  axis(2, 1:nrow(m), rownames(m))
  }
}
par(mfrow= c(1,1))

###########################
#Tree building            #
###########################
attach(df_full)
summary(df_full$class) 
#Make a tree of maxdepth 10, otherwise there will be to many rules anyway. 
#If bigger trees are made, the computations will take way to long.
tree = rpart(class ~ .
             ,data=df_full
             ,method="class", parms=list(prior=c(0.3, 0.7)), control = list(cp=0, xval=20, minsplit = 5, maxdepth=10)) 

attributes(tree)
summary(tree)
tree

prp(tree, type =0, yesno = 0, extra=101, varlen=0, clip.facs=F, fallen.leaves = T)
title("Decision tree for au_ou")

#results of cross validation
printcp(tree)
plotcp(tree)

#plotting cost complexity in relation to number of splits
plot(tree$cptable[,2],tree$cptable[,1],xlab='Number of splits',ylab='Cost complexity parameter,cp')
abline(v=10)
abline(a=0.016, b=0)

#pruning trees at specific cost complexity cp to obtain approx 10 rules
tree.pruned<-prune(tree,cp = 0.016) 
summary(tree.pruned)
tree.pruned
prp(tree.pruned, type =0, yesno = 0, extra=101, varlen=0, clip.facs=F, fallen.leaves = F, branch = 0, uniform=T)
title("Decision tree au/ou pruned at cp=0.016")
