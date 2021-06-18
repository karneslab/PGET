library(scales)

#scale
  cleanstudentdata$know_diffscale <- rescale(cleanstudentdata$know_diff, to = c(1, 16)) 
  
  cleanstudentdata$att_diffscale <- rescale(cleanstudentdata$att_diff, to = c(1, 102)) 
  
#Knowledge regression
  summary(know <- glm(as.integer(know_diffscale) ~ case_x+
                        year_x+
                        attendance+
                        age+
                        sex+
                        prepharm+
                        genetics+
                        personalexp+
                        campus+
                        gpa, family="poisson", data=cleanstudentdata))
  
  

#Attitudes Regressio
  summary(att <- glm(att_diffscale ~ case_x+
                     year_x+
                     attendance+
                     age+
                     sex+
                     prepharm+
                     genetics+
                     personalexp+
                     campus+
                     gpa, family="poisson", data=cleanstudentdata))

#Grades Regression
  summary(grade <- glm(grade ~ case_x+
                       year_x+
                       attendance+
                       age+
                       sex+
                       prepharm+
                       genetics+
                       personalexp+
                       campus+
                       gpa, family="poisson", data=cleanstudentdata))


#Pearson Chi^2 and Dispersion ###
  library(msme)
  
  P__disp(know)
  P__disp(att)
  P__disp(grade)
  

#Negative Binomial Regression for Attitudes

  summary(m2 <- glm.nb(att_diffscale ~ case_x+
                       year_x+
                       attendance+
                       age+
                       sex+
                       prepharm+
                       genetics+
                       personalexp+
                       campus+
                       gpa, data=cleanstudentdata))

