# compare DW-Nominate distributions of legislator ideal points for absence points

require(readr)
require(dplyr)
require(ggplot2)
require(ggjoy)
require(tidyr)
require(forcats)

load_dw_cong <- read_csv('data/HSall_members.csv') %>% select(icpsr,congress,dim1,party_code)
dw_votes <- read_csv('data/HSall_votes.csv')

# only votes since WWII

dw_votes_postww <- filter(dw_votes,congress>78)
dw_votes_postww <- left_join(dw_votes_postww,load_dw_cong,by=c('congress','icpsr'))

prop.table(table(dw_votes_postww$cast_code))

# now do a really cool ggploy plot

mutate(dw_votes_postww, present=if_else(cast_code==9,0,1),
       party_code=factor(party_code,labels=c('Democrat','Conservative',
                                              'Republican','Independent',
                                             'Ind. Democrat',
                                             'Progressive',
                                             'Liberal',
                                             'American Labor')))  %>% 
  filter(chamber=='Senate',!is.na(dim1),present==0,
         party_code %in% c('Democrat','Republican')) %>%
  group_by(congress,rollnumber) %>% 
  summarize(mean_d=mean(dim1)) %>% 
  ggplot(aes(x=mean_d,y=fct_rev(factor(congress)))) +
  geom_joy(rel_min_height = 0.005,alpha=0.5,fill='blue',colour='blue',
           scale=4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_minimal(base_size = 14) + 
  geom_vline(xintercept=0.0,linetype=3) +
  ylab('Senate') +
  xlab('Left to Right DW-Nominate (Ideology) Scores') +
  theme(axis.text.y = element_text(size=10,vjust = 0),
        panel.grid.major.x=element_blank())

ggsave('dw_abs.png')

