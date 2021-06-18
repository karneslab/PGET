
#know_diff (Table 2) (substitute for alternate outcomes -> a_diff, cs_diff)
  #all participants, retro/pre 
    mean(cleanstudentdata$preknowsum)
    sd(cleanstudentdata$preknowsum)
    
    mean(cleanstudentdata$postknowsum)
    sd(cleanstudentdata$postknowsum)
    
    wilcox.test(cleanstudentdata$preknowsum, cleanstudentdata$postknowsum)
    
  
  #case/control     
    mean(casedata$know_diff)
    sd(casedata$know_diff)
    
    mean(controldata$know_diff)
    sd(controldata$know_diff)
    
    wilcox.test(casedata$know_diff, controldata$know_diff)
    
    summarise(casedata$know_diff)


#McNemars on knowledge questions (Table 3)---------------------------------------------

  #all students (repeat for individual questions)
    mcnemar_data <- table(cleanstudentdata$q1pre, cleanstudentdata$q1post)
    print.table(mcnemar_data)
    mcnemar.test(mcnemar_data, y = NULL)
    
    count(cleanstudentdata, q1pre)
    count(cleanstudentdata, q1post)


#wilcoxon all knowledge questions, pre/post (Table 3)
    
    wilcox.test(cleanstudentdata$preknowsum, cleanstudentdata$postknowsum)
    
   
#Chi square test (Table S9)---------------------------------------
    #pre-know questions (wihtout yates correction)
    
      contingency_table <- table(cleanstudentdata$case_x, cleanstudentdata$q1pre)
      
      print.table(contingency_table)
      chisq.test(contingency_table,correct=FALSE)
    
    
    #post-know questions (without yates correction)
    
      contingency_table <- table(cleanstudentdata$case_x, cleanstudentdata$q1post)
      
      print.table(contingency_table)
      chisq.test(contingency_table,correct=FALSE)
    
    
#Wilcoxon test for pre/post, case/control (Table S9)----------------------------------
      
      #find difference in score pre and post
      cleanstudentdata$q1_diff <- cleanstudentdata$q1post - cleanstudentdata$q1pre
      
      #wilcoxon test on score difference
      wilcox.test(q1_diff~case_x,data=cleanstudentdata)
    


