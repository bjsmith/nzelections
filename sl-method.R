
sl_method <- function(total_votes_vec,seats,electorates=NA){
  #now we can apply saint Laguë method:
  
  if(any(class(total_votes_vec)=="numeric")){
    total_votes_vec<-data.frame(t(total_votes_vec))
  }
  #as an example, this needs to be a function repeated separately for each list
  #seats=113
  
  sl_rounds<-seats*2 #arbitrary, not sure how long we'll need to go, but x2 should do it.
  votes_raw_rounds<-data.frame(total_votes_vec,check.names=FALSE)[rep(1,sl_rounds),]
  votes_rounds_result<-votes_raw_rounds/data.frame((1:sl_rounds*2-1))[,rep(1,ncol(votes_raw_rounds))]
  #now we pick the highest set of numbers from that to get the seats
  last_value<-sort(unlist(votes_rounds_result),decreasing = TRUE)[seats]
  #now we select all the results equal to or larger than that number
  #and sum for each party
  
  # if (!is.na(electorates)){
  #   print("folding in electorate wins")
  # }
  # 
  return(colSums(votes_rounds_result>=last_value))
}

calculate_parties_in_parliament <- function(all_total_numeric_cols,electorate_votes_winners_2020){
  all_total_pct_all <- (
    all_total_numeric_cols/all_total_numeric_cols$`Total Valid Party Votes`
  )
  
  passed_5pct_threshold <- colnames(all_total_pct_all)[all_total_pct_all>=0.05]
  passed_5pct_threshold <- passed_5pct_threshold[
    (passed_5pct_threshold %in% c( "Electorate","Total Valid Party Votes"  , "Total Party Votes Counted"))==FALSE]
  parties_winning_an_electorate <- unique(electorate_votes_winners_2020$Party)
  parties_in_parliament<-union(parties_winning_an_electorate,passed_5pct_threshold)
  return(parties_in_parliament)
}


get_approx_figures<-function(winning_party_mg_totals){
  
  #number of seats for each list (Maori and general) determined previously by number of enrolled voters.
  #we're sayin 9 for Maori, 111 for general. Then...
  TotalSeats <- c(111,9)
  total_votes_for_winning_parties<-rowSums(winning_party_mg_totals[,parties_in_parliament])
  winning_party_mg_pcts<-winning_party_mg_totals
  winning_party_mg_pcts[,parties_in_parliament] <- winning_party_mg_pcts[,parties_in_parliament]/total_votes_for_winning_parties
  #now simply multiply by total seats as a proportion of 100
  winning_party_mg_seats<-winning_party_mg_totals
  winning_party_mg_seats[,parties_in_parliament] <- winning_party_mg_pcts[,parties_in_parliament]*TotalSeats
  winning_party_mg_seats_rd<-winning_party_mg_seats
  winning_party_mg_seats_rd[,parties_in_parliament]<-round(winning_party_mg_seats_rd[,parties_in_parliament])
  return(winning_party_mg_seats_rd[,parties_in_parliament])
  #close. interesting that it has resulted in FEWER seats for the Maori party.
  #this is because the Maori party vote was split between general and Maori electorate and couldn't be summed across them.
  #but also, because we only allocated 8 seats to the Maori party under this method.
  
}

reallocate_votes<-function(total_enrolled,maori_total,general_totals){
  #total_enrolled<- data.frame("General"=3273567,"Māori"=276013 )
  #seat allocations should be
  colnames(total_enrolled)<-c("General","Māori")
  ideal_allocations<-total_enrolled/sum(total_enrolled)*120
  whole_allocations<-floor(total_enrolled/sum(total_enrolled)*120)
  #that means we need to take a small proportion of Maori votes and put them in the general, in order to not 'waste' them.
  #what proportion?
  
  
  maori_votes_to_reallocate<-(ideal_allocations$Māori-whole_allocations$Māori)/ideal_allocations$Māori
  print(paste("Proportion of all Māori roll votes that are re-allocated to general roll:", maori_votes_to_reallocate))
  winning_party_general_totals_with_reallocation <- (
    general_totals +maori_total*maori_votes_to_reallocate
  )
  winning_party_maori_totals_with_reallocation <- (
    maori_total -maori_total*maori_votes_to_reallocate
  )
  
  return(list("General"=winning_party_general_totals_with_reallocation,
              "Māori"=winning_party_maori_totals_with_reallocation))
}

get_combined_seat_entitlement <- function(party_votes_summary,electorate_votes_winners){
  electorate_votes_winners$Party<-gsub(" ",".",electorate_votes_winners$Party)
  names(party_votes_summary)<-gsub(" ",".",names(party_votes_summary))
  party_seat_entitlements <- data.frame("Party"=names(party_votes_summary),"PartyVoteSeats"=party_votes_summary)
  electorate_win_table<-data.frame(table(electorate_votes_winners$Party))
  seat_table<-merge(party_seat_entitlements,electorate_win_table,by.x="Party",by.y="Var1",all=TRUE)
  seat_table$TotalSeats<-apply(seat_table[,c("PartyVoteSeats","Freq")],1,function(x){max(x,na.rm=TRUE)})
  seats_vec<-seat_table$TotalSeats
  names(seats_vec)<-seat_table$Party
  return(seats_vec)
}

print_summary_mg_report <- function(party_votes,total_enrolled, electorate_votes_winners){
  
  party_votes$Electorate<-gsub("Māori","Maori",party_votes$Electorate)
  colnames(total_enrolled)[colnames(total_enrolled)=="Māori"]<-"Maori"
  
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
  
  
  #https://www.electionresults.govt.nz/electionresults_2011/e9/html/e9_part9_1.html
  seats_maori<-floor(total_enrolled$Maori/sum(total_enrolled)*120)
  seats_general<-120-seats_maori
  
  print("separate maori and list")
  general<-sl_method(winning_party_mg_totals[1,parties_in_parliament],seats=seats_general)
  maori <- sl_method(winning_party_mg_totals[2,parties_in_parliament],seats=seats_maori)
  combined<-get_combined_seat_entitlement(general+maori,electorate_votes_winners)
  print(combined)
  #we'd probably have to apply the vote allocation method I had in mind
  #that is, actual proportions of enrolled voters are not 111:9 for General:Maori
  #it's more like 
  
  general_totals<-winning_party_mg_totals[1,parties_in_parliament]
  maori_total<-winning_party_mg_totals[2,parties_in_parliament]
  
  
  
  reallocated_votes<-reallocate_votes(total_enrolled,maori_total, general_totals)
  winning_party_general_totals_with_reallocation<-reallocated_votes$General
  winning_party_maori_totals_with_reallocation<-reallocated_votes$Māori
  
  print("CURRENT SYSTEM")
  current_system_raw<-sl_method(colSums(winning_party_mg_totals[parties_in_parliament]),seats=120)
  current_system<-get_combined_seat_entitlement(current_system_raw,electorate_votes_winners)
  print(paste0("Total (",as.character(sum(current_system)),")"))
  print(current_system)
  
  print("PROPOSED: separate maori and list, with maori vote reallocation")
  general<-sl_method(winning_party_general_totals_with_reallocation,seats=seats_general)
  maori <- sl_method(winning_party_maori_totals_with_reallocation,seats=seats_maori)
  new_totals_raw<-(general+maori)
  new_totals<-get_combined_seat_entitlement(new_totals_raw,electorate_votes_winners)
  print(paste0("Total (",as.character(sum(new_totals)),")"))
  print(new_totals)
  print("Maori")
  print(maori)
  print("General")
  print(general)
  #
  print("PARTY SEAT CHANGES")
  print(new_totals-current_system)
  
  
  #For the 2014 election, Maori look set to get one more list vote.
  
  #Hypothetical Maori-allocated 120 seat parliament (for comparison: are we moving toward maori preference or away?)
  # print("hypothetical Maori 120 seat Parliament")
  # print(t(sl_method(winning_party_maori_totals_with_reallocation,seats=120)))
  
  #OK a little more work: which parties did Maori role voters prefer, relative to the general?
  print("difference in proportion of Maori roll voters and general roll voters voting for each party")
  print(t((maori_total/sum(maori_total)) - (general_totals/sum(general_totals))))
  
}