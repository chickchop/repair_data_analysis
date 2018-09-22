########## repair_data import ############
RepairExample_English <- read_csv("data/RepairExample_English.csv", na = c("", "NA"),
                                  col_types = cols(caseID = col_character() , RepairCode = col_character(),
                                                   timestamp = col_datetime(format = "%Y-%m-%d %H:%M"),
                                                   objectKey = col_character()))

######### transform ###############
RepairExample_English$caseID <- as.factor(RepairExample_English$caseID)
RepairExample_English$taskID <- as.factor(RepairExample_English$taskID)
RepairExample_English$originator <- as.factor(RepairExample_English$originator)
RepairExample_English$eventtype <- as.factor(RepairExample_English$eventtype)
RepairExample_English$contact <- as.factor(RepairExample_English$contact)
RepairExample_English$objectKey <- as.factor(RepairExample_English$objectKey)
RepairExample_English$RepairCode <- as.factor(RepairExample_English$RepairCode)
RepairExample_English$RepairType <- as.factor(RepairExample_English$RepairType)


repair_process_df <- RepairExample_English %>% select(caseID,taskID,originator,eventtype,timestamp)
repair_attribute_df <- RepairExample_English %>% select(caseID,contact,RepairType,objectKey,RepairInternally,
                                                    EstimatedRepairTime,RepairCode,RepairOK)
