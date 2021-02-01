# diagnosing and fixing data discrepancies (mismatched IDs, missingness, etc.)

# this is the address file that Rebecca sent me
all_addr = read.csv('M:/wildfires/birth_data/Jean_2018_2019_addresses_120720.csv')
all_ids = all_addr$VS_unique

# this is the result of geocoding the addresses in all_addr
coded_addr = read.csv('M:/wildfires/coded_address.csv')
coded_ids = coded_addr$VS_unique

# what IDs in all_addr are missing in coded_addr?
#  this could indicate geocoding failure
all_minus_coded = setdiff(all_ids, coded_ids)

# what IDs are in coded_addr that are not in all_addr?
#  this could indicate a parsing issue in ArcGIS
coded_minus_all = setdiff(coded_ids, all_ids)


# how many failures do commas account for?
comma_problem = all_addr %>% filter(grepl(',', PGB_RESIDENCE_ADDRESS_125))
num_comma = nrow(comma_problem)

# are these comma failures part of the all_minus_coded?
comma_tbl = table(comma_problem$VS_unique %in% all_minus_coded)
comma_tbl[1] == num_comma
# yes
# create a file with th 15,144 comma problems
comma_rmvd = comma_problem %>% mutate(PGB_RESIDENCE_ADDRESS_125
                                      = gsub(',', '', PGB_RESIDENCE_ADDRESS_125))
write.csv(comma_rmvd, file= 'M:/wildfires/geocode_redo/commas_removed.csv', row.names = FALSE)

# this leaves some missing ids still to be accounted for
#  what are the IDs missing from the coded addresses that are NOT a comma problem
not_comma_ids = setdiff(all_minus_coded, comma_problem$VS_unique)

# do all these not_comma_ids have quotation marks in the address column?
quotes <- all_addr %>% filter(VS_unique %in% not_comma_ids & grepl('"', PGB_RESIDENCE_ADDRESS_125))
# this is only 8 of the 9,789 failures

not_quotes = setdiff(not_comma_ids, quotes$VS_unique)
# these might be parsing issues related to the quotes/comma issue

# create a file with all the not comma problems, quotes removed
other_failures = all_addr %>% filter(VS_unique %in% not_comma_ids)
other_failures <- other_failures %>% mutate(PGB_RESIDENCE_ADDRESS_125 = 
                                              gsub('"', '', PGB_RESIDENCE_ADDRESS_125))

write.csv(other_failures, file = 'M:/wildfires/geocode_redo/other_failures.csv', row.names = FALSE)
