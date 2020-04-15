# CLASS NOTES WEEK 2, Part 2 - Looking at the UN-fish database dataset

# this assumes you have the fishdata set downloaded to your Downloads folder
#  Windows users: put in the chat window where your downloads are
fishdata = read.csv("~/Downloads/undata_fish_2020.csv")

# let's make some vectors out of this data - you can use the data as 
#  a dataframe (which we'll get to later) but since many folks have a Python 
#  background, we might be more used to doing things with vectors.

# make some vectors, first country:
country = fishdata[,1]
# how about year of data?
year = fishdata[,2]
# how about type of fish
type = fishdata[,3]
# how about transaction type? (import, export, re-export/import)
transaction = fishdata[,4]
# how about the cash amount of the transaction?
trade_usd = fishdata[,5]
# how about the weight of the fish in kg?
weight = fishdata[,6]
# how about the quantity name?
quant_name = fishdata[,7]
# looks like some of of the "quantity" measures are weight, or # of items, or nothing
