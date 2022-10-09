library(tidyverse)
#what if we applied the actual quotient method?
source("sl-method.R")
party_votes<- readr::read_csv("../../data/votes-for-registered-parties-by-electorate_2017.csv",skip = 1)
electorate_votes_winners<- readr::read_csv("../../data/winning-electorate-candidates_2017.csv",skip = 1)
#View(party_votes)

maori_general_totals<- (
  party_votes %>% filter(Electorate %in% c("General Electorate Totals","Māori Electorate Totals"))
)



#determine parties who will be included in the count (5% or won an electorate)
all_total<-party_votes[nrow(party_votes),]
all_total_numeric_cols_bool<-sapply(data.frame(all_total),class)=="numeric"
all_total_numeric_cols<-all_total[all_total_numeric_cols_bool]

parties_in_parliament <- calculate_parties_in_parliament(all_total_numeric_cols,electorate_votes_winners)




#cool, now we know which parties won, we can allocate seats to those parties...
#we're going to approximate the actual formula because the full formula is complex and 
winning_party_mg_totals <- maori_general_totals[,c("Electorate",parties_in_parliament)]
get_approx_figures(winning_party_mg_totals)


total_enrolled<- data.frame("General"=3046216,"Māori"=251793 )

seats_maori<-floor(total_enrolled$Māori/sum(total_enrolled)*120)
seats_general<-120-seats_maori

print("separate maori and list")
general<-sl_method(winning_party_mg_totals[1,parties_in_parliament],seats=seats_general)
maori <- sl_method(winning_party_mg_totals[2,parties_in_parliament],seats=seats_maori)
print(general+maori)

#we'd probably have to apply the vote allocation method I had in mind
#that is, actual proportions of enrolled voters are not 111:9 for General:Maori
#it's more like 

general_totals<-winning_party_mg_totals[1,parties_in_parliament]
maori_total<-winning_party_mg_totals[2,parties_in_parliament]



reallocated_votes<-reallocate_votes(total_enrolled,maori_total, general_totals)
winning_party_general_totals_with_reallocation<-reallocated_votes$General
winning_party_maori_totals_with_reallocation<-reallocated_votes$Māori

print("separate maori and list, with maori vote reallocation")
general<-sl_method(winning_party_general_totals_with_reallocation,seats=seats_general)
maori <- sl_method(winning_party_maori_totals_with_reallocation,seats=seats_maori)
general
maori
print(general+maori)
#
print("current")
print(sl_method(colSums(winning_party_mg_totals[parties_in_parliament]),seats=120))

#For the 2014 election, Maori look set to get one more list vote.

#Hypothetical Maori-allocated 120 seat parliament (for comparison: are we moving toward maori preference or away?)
print("hypothetical Maori 120 seat Parliament")
print(sl_method(winning_party_maori_totals_with_reallocation,seats=120))