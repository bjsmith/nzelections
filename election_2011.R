library(tidyverse)
#what if we applied the actual quotient method?
source("sl-method.R")
party_votes<- readr::read_csv("../../data/e9_part4_2011_parties.csv",skip = 1)
electorate_votes_winners<- readr::read_csv("../../data/e9_part6_2011_electorates.csv",skip = 1)
#data cleaning
electorate_votes_winners$Party<-toupper(electorate_votes_winners$Party)
colnames(party_votes)[1]<-"Electorate"

maori_general_totals<- (
  party_votes %>% filter(Electorate %in% c("General Electorate Totals","Maori Electorate Totals"))
)



#determine parties who will be included in the count (5% or won an electorate)
all_total<-party_votes[nrow(party_votes),]
all_total_numeric_cols_bool<-sapply(data.frame(all_total),class)=="numeric"
all_total_numeric_cols<-all_total[all_total_numeric_cols_bool]

parties_in_parliament <- calculate_parties_in_parliament(all_total_numeric_cols,electorate_votes_winners)




#cool, now we know which parties won, we can allocate seats to those parties...
#we're going to approximate the actual formula because the full formula is complex and 
winning_party_mg_totals <- maori_general_totals[,c("Electorate",parties_in_parliament)]
# get_approx_figures(winning_party_mg_totals)

total_enrolled<- data.frame("General"=2837747,"Maori"=233100)
#https://www.electionresults.govt.nz/electionresults_2011/e9/html/e9_part9_1.html
seats_maori<-floor(total_enrolled$Maori/sum(total_enrolled)*120)
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
print(general)
print(maori)
print(general+maori)
#
print("current")
print(sl_method(colSums(winning_party_mg_totals[parties_in_parliament]),seats=120))

#For the 2014 election, Maori look set to get one more list vote.

#Hypothetical Maori-allocated 120 seat parliament (for comparison: are we moving toward maori preference or away?)
print("hypothetical Maori 120 seat Parliament")
print(sl_method(winning_party_maori_totals_with_reallocation,seats=120))

print("In this scenario, Maori party would end up with one more seat than shown, because they won three list seats in 2011.")

#OK a little more work: which parties did Maori role voters prefer, relative to the general?
print("likelihood of Maori role voter to vote for each party relative to general role voter")
print((maori_total/sum(maori_total))/(general_totals/sum(general_totals)))