# this takes the unprocessed data, cleans & organizes
# outputs file to be read in by main analysis script

tst = births
colnames(tst)

# test out using pivot_longer

b = tst %>% pivot_longer(key = 'race', hispanic_OSHPD:other_race_OSHPD, value=count)

fa <- factor('a')
fb <- factor("b")
fab <- factor(c("a", "b"))
fab
tst$hispanic_OSHPD

race_eth = c("hispanic_OSHPD", "black_OSHPD", "asian_OSHPD", "other_race_OSHPD")
a = strsplit(race_eth, "_")


#Read in the bay area data file
raw_data = read.csv('/Volumes/Padlock/wildfires/birth_file_bayarea2021-02-01.csv')

#drop unnecessary columns

# rename remaining columns

# combine columns into factors; add in missing levels (ex "white")

# write output file