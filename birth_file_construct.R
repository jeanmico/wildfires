# this is the BIRTHS file construction script for the wildfires project
# input: 
#  birth files from Rebecca
#  results from address geocoding
# output:
#  file with all birth details and covariates
# EXPOSURE information is not used in this script
#  exposure is not in the output file created by this script

library(tidyverse)

# read in data files from Rebecca
base_raw = read.csv('M:/wildfires/birth_data/Jean_2018_2019_addresses_120720.csv')

added_vars1 = read.csv('M:/wildfires/birth_data/Jean_added_2018_19_vars_122830.csv')



# read in geocoded addresses file
colskeep <- c('VS_unique', 'Score', 'X', 'Y', 'UR10', 'PGB_RESIDENCE_COUNTY_126', 'TRACTCE10','BLOCKCE10','GEOID10')

# List all files ending with csv in directory
csv_files = list.files(path = 'M:/addr_1819_full/', pattern = "csv$", full.names = TRUE)

# Read each csv file into a list
addr <- data.frame(matrix(ncol=length(colskeep),nrow=0))
colnames(addr) = colskeep
for (f in csv_files) {
  tmp = read_csv(f) 
  tmp <- tmp %>% select(colskeep)
  addr <- rbind(addr, tmp)
}
write.csv(addr, 'M:/wildfires/coded_address.csv', row.names = FALSE)


# merge the files
birth_df = merge(base_raw, added_vars1, by = 'VS_unique')
birth_df = merge(birth_df, addr, by = 'VS_unique')

# write FULL output

write.csv(birth_df, file = paste('M:/wildfires/birth_file_full', Sys.Date(), '.csv', sep=''), row.names = FALSE)

# restrict to bay area counties; write output
bay_counties = c('001', '013', '041', '075', '081', '085')

bay_addr = birth_df %>% filter(PGB_RESIDENCE_COUNTY_126.x %in% bay_counties)
write.csv(bay_addr, file = paste('M:/wildfires/birth_file_bayarea', Sys.Date(), '.csv', sep = ''), row.names = FALSE)
