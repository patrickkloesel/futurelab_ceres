#1. Get introductions

oecd_introductions = oecd_grouped
#we set NA values to 0 for now which means we will catch the first non-zero value whether or not there was an NA before. 
#introductions that came from NA are then filtered out later
oecd_introductions$Value[is.na(oecd_introductions$Value)] = 0
oecd_introductions = oecd_introductions %>% group_by(ISO,Module,Policy) %>% filter(year == year[min(which(Value>0))])

#now filter out all 1996 & 1997 introductions (we can't distinguish whether this is an introduction or a preexisting policy, take conservative approach here)

oecd_introductions = oecd_introductions[!oecd_introductions$year == 1996,]
oecd_introductions = oecd_introductions[!oecd_introductions$year == 1997,]


#add a marker for introductions
oecd_introductions$introduction = 1
#
# #subset and merge
oecd_introductions_sub = as.data.frame(oecd_introductions[c('ISO','Module','Policy','year','introduction')])
#
oecd_grouped = merge(oecd_grouped,oecd_introductions_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$introduction[is.na(oecd_grouped$introduction)] = 0

#now filter out all the introductions that came from NA changes 
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(introduction = ifelse(is.na(lag(Value)), 0,introduction)) %>%
  ungroup()

#2. Get policy phase out

oecd_phaseout = oecd_grouped 

#we catch all zero values after a non-zero value
oecd_phaseout = oecd_phaseout %>% group_by(ISO,Module,Policy) %>% filter(Value == 0 & lag(Value, default = 0) > 0)

#now filter out all 2022 (and 2021?) phase out
oecd_phaseout = oecd_phaseout[!oecd_phaseout$year == 2022,]
#oecd_phaseout = oecd_phaseout[!oecd_phaseout$year == 2021,]

#add a marker for phase outs
oecd_phaseout$phase_out = 1
#
# #subset and merge
oecd_phaseout_sub = as.data.frame(oecd_phaseout[c('ISO','Module','Policy','year','phase_out')])
#
oecd_grouped = merge(oecd_grouped,oecd_phaseout_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$phase_out[is.na(oecd_grouped$phase_out)] = 0


#3. Get policy intensifications

## Set all NAs to 0 such that NA jumps are not counted for jumps for sure. 
oecd_grouped$Value[is.na(oecd_grouped$Value)] = 0
oecd_grouped$diff[is.na(oecd_grouped$diff)] = 0
oecd_grouped$diff_2[is.na(oecd_grouped$diff_2)] = 0

#make dummies if we get an introduction from first or second lag
oecd_grouped = oecd_grouped %>% mutate(diff_adj = ifelse(diff>=2,1,0), diff_2_adj = ifelse(diff_2>=2,1,0))

####### HERE FILTER OUT T1 FROM THE 2RDD
# second dataset <- selecting only rdd storage and rdd renewable
# DELETE CASE WHEN: diff_adj==1 & introduction==0 & diff_2_adj==0
#####

##if there is a jump in lag(t-1), don't also count a jump in t-2
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj = ifelse(lag(diff_adj) == 1, 0,diff_2_adj)) %>%
  ungroup()

# MERGE AGAIN WITH RDD

## if there is an introduction in t-1, don't also count a jump in t-2 if it increases by 1
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj = ifelse(lag(introduction) == 1, 0,diff_2_adj)) %>%
  ungroup()

oecd_grouped$diff_2_adj[is.na(oecd_grouped$diff_2_adj)] = 0


#4. get policy slow phase out (loosening)

oecd_slowout = oecd_grouped
#make dummies if we get a negative jump from first or second lag
oecd_slowout = oecd_slowout %>% mutate(diff_adj_neg = ifelse(diff<=-2,1,0), diff_2_adj_neg = ifelse(diff_2<=-2,1,0))

##if there is a negative jump in lag(t-1), don't also count a negative jump in t-2
oecd_slowout <- oecd_slowout %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj_neg = ifelse(lag(diff_adj_neg) == 1, 0,diff_2_adj_neg)) %>%
  ungroup()

## if there is a phase-out in t-1, don't also count a jump in t-2 if it increases by 1
oecd_slowout <- oecd_slowout %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj_neg = ifelse(lag(phase_out) == 1, 0,diff_2_adj_neg)) %>%
  ungroup()

oecd_slowout$diff_2_adj_neg[is.na(oecd_slowout$diff_2_adj_neg)] = 0

oecd_slowout <- oecd_slowout %>% filter(diff_adj_neg==1 | diff_2_adj_neg==1)



#subset
oecd_slowout_sub = as.data.frame(oecd_slowout[c('ISO','Module','Policy','year','diff_2_adj_neg', 'diff_adj_neg')])
#merge
oecd_grouped = merge(oecd_grouped,oecd_slowout_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$diff_adj_neg[is.na(oecd_grouped$diff_adj_neg)] = 0
oecd_grouped$diff_2_adj_neg[is.na(oecd_grouped$diff_2_adj_neg)] = 0

############

#5. #keep all jumps, introductions, phaseouts, slowouts 
oecd_grouped = oecd_grouped[oecd_grouped$diff_adj==1 | oecd_grouped$diff_2_adj==1 | oecd_grouped$introduction==1 | oecd_grouped$phase_out==1 | oecd_grouped$diff_adj_neg ==1 | oecd_grouped$diff_2_adj_neg==1 ,]

# check that no phaseout and slow phaseout is also an intro or tightening
oecd_grouped %>% filter(introduction ==1 & phase_out ==1)
oecd_grouped %>% filter(diff_adj ==1 & diff_adj_neg ==1)
oecd_grouped %>% filter(diff_2_adj ==1 & diff_2_adj_neg ==1)
oecd_grouped %>% filter(diff_adj ==1 & diff_2_adj_neg ==1)
oecd_grouped %>% filter(diff_2_adj ==1 & diff_adj_neg ==1)


oecd_grouped = as.data.frame(oecd_grouped)
rownames(oecd_grouped) <- NULL



