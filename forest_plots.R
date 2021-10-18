fnames = list.files('/Volumes/Padlock/wildfires/results/', pattern = 'loglinear', full.names = TRUE)

exps = c('binary_exposedTRUE', 'pm25count_50', 'pm25count_100', 'pm25mean', 'pm25peak', 'firemean', 'firetert')
timetypes = c('preterm', 'pt_vearly', 'pt_early', 'pt_mid', 'pt_late')

models = data.frame(matrix(nrow = 0, ncol = 8))
colnames(models) =   c("term",   "estimate",   "std.error", "statistic",      "p.value", "fire", 'pttype', 'crude')

for (i in fnames) {
  tmp = read.csv(i)
  tmp  = tmp %>% filter(term %in% exps) %>% 
    mutate(fire = case_when(length(grep('fire', basename(i))) == 1 ~ 1,
                                                                           TRUE ~ 0)) %>%
    mutate(pttype = str_extract(basename(i), "(.+?)(?=\\d)")) %>% # get everything before the first numeric character
    mutate(crude = grepl('crude', basename(i)))
  
  models = rbind(models, tmp)
}

write.csv(models, file = output_name('models'), row.names = FALSE)

models = models %>% mutate(RR = exp(estimate)) %>% mutate( RRlow = exp(estimate - std.error*1.96)) %>% 
                                                             mutate( RRhigh = exp(estimate + std.error*1.96)) %>%
  select(-c(estimate, std.error, statistic))


models <- models %>% mutate(ptdecode = case_when(
  pttype == 'preterm' ~ 'Preterm',
  pttype == 'pt_early' ~ 'Early Preterm',
  pttype == "pt_vearly" ~ "Very Early Preterm",
  pttype == "pt_late" ~ "Late Preterm"
  ))

models$ptdecode = factor(models$ptdecode, levels = c("Preterm", "Very Early Preterm", "Early Preterm", "Late Preterm"))

basedf = models %>% filter(fire == 0)
firedf = models %>% filter(fire == 1)

basedf <- basedf %>% filter(pttype != 'pt_mid')

basedf <- basedf %>% mutate(decode= case_when(
  term == 'pm25mean' ~ "Mean PM2.5",
  term == 'binary_exposedTRUE' ~ "Exposed to fire",
  term == 'pm25count_100' ~ "Days above 100",
  term == 'pm25count_50' ~ 'Days above 50'
  ))



q1 <- ggplot(basedf, aes(x = decode, y = RR, ymin = RRlow, ymax = RRhigh, color = crude)) + 
  #geom_pointrange() + 
  geom_linerange(position=position_dodge(width = -0.5), size=.75) +
  geom_point(position=position_dodge(width = -0.5), size=3) +
  scale_color_manual(values=c('#000000', '#bb5566'))+
  scale_y_log10() + 
  coord_flip() + 
  geom_hline(yintercept =1, linetype = 'dashed') + 
  xlab('Exposure metric') + 
  ylab('Relative risk') + 
  theme_bw() +
  facet_grid(cols = vars(ptdecode))
q1

ggsave(q1, file = image_name('forest_adj'), width = 7, height = 5)


