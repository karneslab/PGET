#att_diff (Table 2) (substitute for alternate outcomes -> a_diff, cs_diff)
    #all participants, retro/pre 
      mean(cleanstudentdata$a_before)
      sd(cleanstudentdata$a_before)
      
      mean(cleanstudentdata$a_after)
      sd(cleanstudentdata$a_after)
      
      wilcox.test(cleanstudentdata$a_before, cleanstudentdata$a_after)
    
    
    #case/control     
      mean(casedata$a_diff)
      sd(casedata$a_diff)
      
      mean(controldata$a_diff)
      sd(controldata$a_diff)
      
      wilcox.test(casedata$a_diff, controldata$a_diff)



#wilcoxon rank sum
     #retro (Table S7, S8)
        wilcox.test(as.numeric(q1_1)~case_x, data = cleanstudentdata, alternative = "two.sided")
        
        mean(casedata$q1_1)
        sd(casedata$q1_1)
        
        mean(controldata$q1_1)
        sd(controldata$q1_1)
        
        w=table(casedata$q1_1)
        w
        w=table(controldata$q1_1)
        w 


    #post (Table S7, S8)
      wilcox.test(as.numeric(q1_2)~case_x, data = cleanstudentdata, alternative = "two.sided")
      
      mean(casedata$q1_2)
      sd(casedata$q1_2)
      
      mean(controldata$q1_2)
      sd(controldata$q42_2)
      
      w=table(casedata$q1_2)
      w
      w=table(controldata$q1_2)
      w 
      
      #performance differance 
      cleanstudentdata$q1diff <- cleanstudentdata$q1_2 - cleanstudentdata$q1_1
      wilcox.test(as.numeric(q1diff)~case_x, data = cleanstudentdata, alternative = "two.sided")
      
      casedata <- cleanstudentdata %>%
        filter(case_x==1)
      
      controldata <- cleanstudentdata %>%
        filter(case_x==0)
      
      mean(casedata$q1diff)
      sd(casedata$q1diff)
      
      mean(controldata$q1diff)
      sd(controldata$q1diff)
      

  