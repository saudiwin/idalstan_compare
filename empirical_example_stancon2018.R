# collect data for new empirical example for StanCon

require(rtweet)
require(dplyr)
require(tidyr)
require(ggplot2)
require(readr)
require(idealstan)
require(RSQLite)
require(forcats)
require(stringr)

food_db <- dbConnect(SQLite(),'data/database.sqlite')

sample_db <- dbReadTable(food_db,'Reviews')
coffee_codes <- group_by(sample_db,ProductId) %>% 
  summarize(sum_count=sum(str_count(Summary,'[Cc]offee|[Cc]ofee')),
            text_count=sum(str_count(Text,'[Cc]offee|[Cc]ofee'))) %>% 
  filter(sum_count>3 | text_count>5)
just_coffee <- filter(sample_db,ProductId %in% coffee_codes$ProductId) %>% 
  distinct(UserId,ProductId,Text,.keep_all=T)

#constrain starbucks coffee

starbucks_count <- mutate(just_coffee,
                      starbucks=str_count(Text,"[Ss]tarbucks")) %>% 
  group_by(ProductId) %>% 
  summarize(st_count=sum(starbucks)) %>% 
  arrange(desc(st_count))

# remove coffees below the median rating (5)

coffee_count_prod <- group_by(just_coffee,ProductId) %>% summarize(tot_unique=length(unique(UserId))) %>% 
  filter(tot_unique<30)
coffee_count_user <- group_by(just_coffee,UserId) %>% summarize(tot_unique=length(unique(ProductId))) %>% 
  filter(tot_unique<3)
just_coffee <- anti_join(just_coffee,coffee_count_prod,by='ProductId') %>% 
  anti_join(coffee_count_user,by='UserId')

# Figure out which bills have a lot of diversity in reviews

# now mutate the data so that we can get IDs

coffee_matrix <- select(just_coffee,
                        ProductId,
                        UserId,
                        Score) %>% 
  group_by(ProductId,UserId) %>% 
  summarize(mean_score=round(mean(Score))) %>% 
  ungroup %>% 
  mutate(ProductId=fct_relevel(factor(ProductId),'B003GTR8IO'),
         UserId=as.numeric(factor(UserId))) %>% 
  spread(key = ProductId,value = mean_score)

coffee_matrix_ready <- as.matrix(select(coffee_matrix,-UserId))
#coffee_matrix_ready <- coffee_matrix_ready[-readRDS('get_rid_of_these.rds'),]
row.names(coffee_matrix_ready) <- coffee_matrix$UserId 
coffee_data <- idealstan::id_make(score_data=coffee_matrix_ready,
                       person_data=data_frame(person.names=1:nrow(coffee_matrix),
                                             group=rep("O",nrow(coffee_matrix))),
                        ordinal=T,
                       inflate=F,
                       low_val=1,
                       high_val=5,
                       middle_val=c(2,3,4),
                       outcome_label_type='none')
coffee_model <- id_estimate(idealdata=coffee_data,
                            model_type=3,
                            fixtype='vb',
                            restrict_ind_high = 1,
                            restrict_params='person',
                            auto_id = F,
                            use_vb=T)
require(idealstan)
coffee_model <- readRDS('coffee_model.rds')

# Plot all the item discrimination parameters

id_plot_all_hist(coffee_model,params = 'regular_discrim')

results <- id_plot_all_hist(coffee_model,params='regular_discrim',
                            return_data = T)

results$plot_data %>% 
  filter(obs_type=='median_pt') %>% 
  separate(param_id,into=c('param1','param2','num')) %>%
  mutate(num=colnames(coffee_matrix_ready)[as.numeric(num)]) %>% 
  arrange(value) %>% slice(1:10) %>% 
  left_join(just_coffee,by=c('num'='ProductId')) %>% 
  write_csv('data/low_coffee_discrim.csv')


results$plot_data %>% 
  filter(obs_type=='median_pt') %>% 
  separate(param_id,into=c('param1','param2','num')) %>%
  mutate(num=colnames(coffee_matrix_ready)[as.numeric(num)]) %>% 
  arrange(desc(value)) %>% slice(1:10) %>% 
  left_join(just_coffee,by=c('num'='ProductId')) %>%
  write_csv('data/high_coffee_discrim.csv')


#Highest positive discrimination - some European brand

id_plot_legis(coffee_model,group_color=F,group_overlap = T,
              person_labels=F,person_ci_alpha = .05,
              bill_plot=429,
              group_labels=F,
              point_size=2,
              show_score = c('1','2','3','4','5'),
              text_size_group = 4)

#Highest negative discrimination - some weird brand on Amazon

id_plot_legis(coffee_model,group_color=F,group_overlap = T,
              person_labels=F,person_ci_alpha = .05,
              bill_plot=364,text_size_group=4,
              show_score = c('1','2','3','4','5')) 

#Starbucks

id_plot_legis(coffee_model,group_color=F,group_overlap = T,
              person_labels=F,person_ci_alpha = .05,
              bill_plot=1,text_size_group=4,
              show_score = c('1','2','3','4','5')) 

#Near zero discrim

id_plot_legis(coffee_model,group_color=F,group_overlap = T,
              person_labels=F,person_ci_alpha = .05,
              bill_plot=168,text_size_group=4,
              show_score = c('1','2','3','4','5')) 

# What are these products?

just_coffee %>% filter(ProductId==colnames(coffee_matrix_ready)[168]) %>% View

# apparently, generic K-cups

just_coffee %>% filter(ProductId==colnames(coffee_matrix_ready)[419]) %>% View
 
all_params <- as.array(coffee_model@stan_samples)
#bayesplot::mcmc_trace(all_params,regex_pars='restrict_low')
check_params <- rstan::extract(coffee_model@stan_samples,'L_full')[[1]][,1,] %>% 
  apply(2,sd)
get_rid_of_these <- which(check_params>20)
lookat <- lapply(get_rid_of_these,function(i) {
  table(coffee_matrix[i,])
})
