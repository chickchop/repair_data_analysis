############### repair process df ################
######## add start, end time columns
repair_process_df$timestamp <- as.character(repair_process_df$timestamp, format="%Y-%m-%d %H:%M")

repair_process_df <- repair_process_df %>%
    mutate(start_time=if_else(eventtype == "start", timestamp, character(1)), end_time = if_else(eventtype == "complete", timestamp, character(1)))

tmp <- repair_process_df %>% group_by(caseID,taskID) %>% count()
tmp <- data.frame(tmp)
one_event_lis <- tmp %>% select(taskID, n) %>% filter(n==1)  %>% distinct(taskID)
two_event_lis <- tmp %>% select(taskID, n) %>% filter(n==2)  %>% distinct(taskID)
one_event_lis <- one_event_lis$taskID
two_event_lis <- two_event_lis$taskID

rm(tmp)

tmp_1 <- repair_process_df %>% filter(taskID %in% one_event_lis) 
tmp_1_i <- tmp_1 %>% filter(taskID != "ExternRepair") %>% mutate(start_time = end_time)
tmp_1_e <- tmp_1 %>% filter(taskID == "ExternRepair") %>% mutate(end_time = start_time)
tmp_1 <- bind_rows(tmp_1_i,tmp_1_e)

tmp_2 <- repair_process_df %>% filter(taskID %in% two_event_lis)
tmp_2_s <- tmp_2 %>% filter(eventtype == "start")
tmp_2_c <- tmp_2 %>% filter(eventtype == "complete")
tmp_2_s <- tmp_2_s %>% mutate(key = paste(caseID,taskID))
tmp_2_c <- tmp_2_c %>% mutate(key = paste(caseID,taskID))
tmp_2_c <- tmp_2_c %>% select(key,end_time)
tmp_2 <- left_join(tmp_2_s, tmp_2_c, by="key")
tmp_2 <- tmp_2 %>% select(-key,-end_time.x)
tmp_2 <- rename(tmp_2, end_time = end_time.y)

repair_process_df <- bind_rows(tmp_1,tmp_2)
repair_process_df <- repair_process_df %>% select(-eventtype, -timestamp)
repair_process_df$start_time <- ymd_hm(repair_process_df$start_time)
repair_process_df$end_time <- ymd_hm(repair_process_df$end_time)

rm(tmp_1,tmp_1_e,tmp_1_i,tmp_2,tmp_2_c,tmp_2_s,one_event_lis,two_event_lis)


################# repair attribute df ################
tmp_1 <- repair_attribute_df %>% filter(!is.na(contact)) %>% select(caseID,contact)
tmp_2 <- repair_attribute_df %>% filter(!is.na(RepairType)) %>% select(caseID,RepairType)
tmp_3 <- repair_attribute_df %>% filter(!is.na(RepairInternally)) %>% select(caseID,RepairInternally)
tmp_4 <- repair_attribute_df %>% filter(!is.na(EstimatedRepairTime)) %>% select(caseID,EstimatedRepairTime)
tmp_5 <- repair_attribute_df %>% filter(!is.na(RepairCode)) %>% select(caseID,RepairCode)
repair_attribute_df <- left_join(tmp_1,tmp_2, by = "caseID")
repair_attribute_df <- left_join(repair_attribute_df,tmp_3, by = "caseID")
repair_attribute_df <- left_join(repair_attribute_df,tmp_4, by = "caseID")
repair_attribute_df <- left_join(repair_attribute_df,tmp_5, by = "caseID")

rm(tmp_1,tmp_2,tmp_3,tmp_4,tmp_5)


################ process mining data set ####################
repair_process_mining_df <- left_join(repair_process_df,repair_attribute_df, by="caseID")


################ export data ################
# out_data <- repair_process_mining_df
# path_ <- "C:/Users/ko/Desktop/repair_sample.csv"
# write_csv(out_data, path = path_, na = "")
