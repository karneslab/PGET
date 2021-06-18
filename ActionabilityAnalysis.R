#presence of actionable varinats analysis----------------------

  #split case data into Y/N actionable variant
    casedata_y <- filter(casedata, actionable_genotypes_highly == "Y")
    casedata_n <- filter(casedata, actionable_genotypes_highly == "N")
    
    
  #wilcox test for actionable variants and outcomes  (repeat for additional outcomes) 
    wilcox.test(grade~actionable_genotypes_highly,data=casedata)
    
    #repeat for N group
    mean(casedata_y$grade, na.rm = T)
    sd(casedata_y$grade, na.rm = T)
 #---------------------------------------------
    
  #split control data into Y/N actionable variant
    controldata_y <- filter(controldata, actionable_genotypes_highly == "Y")
    controldata_n <- filter(controldata, actionable_genotypes_highly == "N")
    
    
   #wilcox test for actionable variants and outcomes  (repeat for additional outcomes) 
    wilcox.test(grade~actionable_genotypes_highly,data=casedata)
    
    #repeat for N group 
    mean(controldata_y$grade, na.rm = T)
    sd(controldata_y$grade, na.rm = T)

  
#z transformed actionability score correlation--------------------------------------------------------------
  caseyear1 <- casedata%>%
    select(c("id","year_x","know_diff", "att_diff","cs_diff","a_diff", "number_of_actionable_genotypes_highly", 
             "number_of_drugs_highly","age", "gpa",
             "sex", "attendance", "genomics", "genetics", "personalexp", "prepharm","grade"))%>%
    filter(year_x==1)
  
  
  caseyear2 <- casedata%>%
    select(c("id","year_x","know_diff", "att_diff","cs_diff","a_diff","number_of_actionable_genotypes_highly", 
             "number_of_drugs_highly","age", "gpa",
             "sex", "attendance", "genomics", "genetics", "personalexp", "prepharm","grade"))%>%
    filter(year_x==2)
  
  caseyear3 <- casedata%>%
    select(c("id","year_x","know_diff", "att_diff","cs_diff","a_diff","number_of_actionable_genotypes_highly", 
             "number_of_drugs_highly","age", "gpa",
             "sex", "attendance", "genomics", "genetics", "personalexp", "prepharm","grade"))%>%
    filter(year_x %in% c("3","4"))
  
  # scale variants
  caseyear1$number_of_actionable_genotypes_highly <- scale(caseyear1$number_of_actionable_genotypes_highly)
  
  caseyear2$number_of_actionable_genotypes_highly <- scale(caseyear2$number_of_actionable_genotypes_highly)
  
  caseyear3$number_of_actionable_genotypes_highly <- scale(caseyear3$number_of_actionable_genotypes_highly)
  
  #scale drugs 
  caseyear1$number_of_drugs_highly <- scale(caseyear1$number_of_drugs_highly)
  
  caseyear2$number_of_drugs_highly <- scale(caseyear2$number_of_drugs_highly)
  
  caseyear3$number_of_drugs_highly <- scale(caseyear3$number_of_drugs_highly)
  
  
  case_total <- rbind(caseyear1, caseyear2, caseyear3)%>%
    rename(z_variants = number_of_actionable_genotypes_highly)%>%
    rename(z_drugs = number_of_drugs_highly)

#person correlation (repeat for other outcomes)

  cor <- cor.test(casedata$know_diff, casedata$number_of_actionable_genotypes_highly, method = c("pearson"))
  print(cor)
  
  median(case_total$z_variants)
  IQR(case_total$z_variants)