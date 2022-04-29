# Setup ===========================================================================================

# Clear the working space
rm(list = ls())

# Set working directory
setwd("G:/我的雲端硬碟/Data Science/Major&Score")

# load packages
library(tidyverse)
library(stargazer)
library(sandwich)
library(rvest)
library(xml2)
library(readxl)
library(stringi)

# turn off scientific notation
options(scipen = 9)

# Schools and Majors table  ==============================================================================
#大學校院系所彙整表
#data source: https://ws.moe.edu.tw/001/Upload/4/relfile/0/5040/02797dd0-2c24-43f9-b3e6-8844d1a25620.csv

#load csv
majortable_path = "02797dd0-2c24-43f9-b3e6-8844d1a25620.csv"
majortable <- read.csv(majortable_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble


#drop replicates
majortable2 <- majortable[!duplicated(majortable[c("V1", "V12")]) | duplicated(majortable[c("V1", "V12")], fromLast = TRUE),]
while (nrow(majortable2) != nrow(majortable)){
  majortable <- majortable2
  majortable2 <- majortable[!duplicated(majortable[c("V1", "V12")]) | duplicated(majortable[c("V1", "V12")], fromLast = TRUE),]
}
rm(majortable2)

#rename the variables while dropping other ones
majortable <- majortable %>% transmute(school = V4, sc_index = as.factor(V1), public = (V2 == "公立"), 
                                       major = V12, ma_index = as.factor(V11), area = as.factor(V6), area_index = as.factor(V5),
                                       bigdomain = as.factor(V8), bigdom_index = as.factor(V7), domain = as.factor(V10), dom_index = as.factor(V9))

#rename levels in areas for future visualization
levels(majortable$area) <- list(Medicine = "醫藥衛生及社會福利", Engineering = "工程、製造及營建", Science = "自然科學、數學及統計", Agriculture = "農業、林業、漁業及獸醫",
                                Information = "資訊通訊科技", BusinessNLaw = "商業、管理及法律", SocialSc = "社會科學、新聞學及圖書資訊", 
                                Service = "服務", Arts = "藝術及人文", Education = "教育", Other = "其他")

majortable <- majortable %>% mutate(stem = (area == c('Engineering', 'Science', 'Agriculture', 'Medicine', 'Information')))

# Advanced Subjects Test ==============================================================================

#指考
#original file is in .pdf from https://www.uac.edu.tw/downloads.htm, transformed to CSV with tabula

#load csv
score_path = "tabula-110_04.csv"
mns <- read.csv(score_path, check.names = FALSE, quote="", header = FALSE, stringsAsFactors=FALSE) %>% as_tibble()
mns <- mns %>% transmute(index = as.integer(V1), school = as.factor(V2), size = as.integer(V5), score = as.double(V6), abscore = as.double(V8), oldmajor = V3, weighttxt = V4) %>% subset(!is.na(index))

#drop different "groups" and reduce major name to just "XX系", "XX學程", 
mns$major <- sub("統計科學組", "", mns$oldmajor) #but 系統科學組 is a group
mns$major <- sub("系統", "placeholder", mns$major) #系統科學 is the name of a major, don't want to drop that
mns$major <- sub("系..*", "系", mns$major)
mns$major <- sub("placeholder", "系統", mns$major)
mns$major <- sub("學程..*", "學程", mns$major)
mns$major <- sub("\"", "", mns$major)

#merge with majortable
mns <- merge(mns, majortable, by =c("major", "school"), all.x = TRUE, all.y = FALSE)
mns <- mns[!duplicated(mns$index),]

#track majors with no matched index
mns <- mns %>% mutate(scma_match = !is.na(ma_index))

#now we calculate average score and adjusted average score using weights

#convert the weights string ex: "國x1.25 英x1.5" into subject-specific weight
wlist = tibble(
  index = c(1:nrow(mns)),
  txt = NA,
  chi = NA, #國
  eng = NA, #英
  maa = NA, #數甲
  mab = NA, #數乙 
  his = NA, #歷
  geo = NA, #地
  soc = NA, #公
  phy = NA, #物
  chm = NA, #化
  bio = NA, #生
  pro = NA #術
)

#find the weight corresponding to certain major
findweight = function(temp, subject){
  
  place = NA
  weight = 0
  place <- temp %>% stri_locate_first(fixed = fixed(subject))
  weight <- temp %>% str_sub(place[[2]]+2, place[[2]]+5) %>% as.double()
  
  if (is.na(weight)){
    return(0)
  }else{
    return(weight)
  }
}

vfindweight <- Vectorize(findweight) #vectorize the function

wlist$txt <- mns$weighttxt
wlist$chi <- mns$weighttxt %>% vfindweight("國")
wlist$eng <- mns$weighttxt %>% vfindweight("英")
wlist$maa <- mns$weighttxt %>% vfindweight("數甲")
wlist$mab <- mns$weighttxt %>% vfindweight("數乙")
wlist$his <- mns$weighttxt %>% vfindweight("歷")
wlist$geo <- mns$weighttxt %>% vfindweight("地")
wlist$soc <- mns$weighttxt %>% vfindweight("公")
wlist$phy <- mns$weighttxt %>% vfindweight("物")
wlist$chm <- mns$weighttxt %>% vfindweight("化")
wlist$bio <- mns$weighttxt %>% vfindweight("生")
wlist$pro <- mns$weighttxt %>% vfindweight("術")

#add subject median to reflect difficulty difference between subjects
med = c(60, 54, 38, 54, 70, 62, 56, 38, 51, 60) #data from https://www.ner.gov.tw/news/6119d85cf36aef00079e71cc, median score of 110 test
med = med/mean(med) #normalize

wlist <- wlist %>% mutate(weight = chi + eng + maa + mab + his + geo + soc + phy + chm + bio + pro,
                          ad_weight = chi*med[[1]] + eng*med[[2]] + maa*med[[3]] + mab*med[[4]] + 
                            his*med[[5]] + geo*med[[6]] + soc*med[[7]] + phy*med[[8]] + chm*med[[9]] + bio*med[[10]] + pro)

#compute average weight and difficulty-adjusted average weights
mns$av_score <- mns$score/wlist$weight
mns$adav_score <- mns$score/wlist$ad_weight

#cleanup
rm(majortable, wlist)

# Salary from 104.com ==============================================================================

#data source: https://www.104.com.tw/jb/career/

#if method == 0, use pre-saved data, otherwise get data from 104
getsalary = function(mns, method, filepath){ 
  
  if (method == 0){
    
    #read pre-saved csv, generated from pervious session using method = 1
    salary <- read.csv(filepath)
    mns <- merge(mns, salary[c("index", "avsalary")], by = "index")
    
  }
  else{
    #retrived data directly from 104, code takes a while to run
    
    #get the urls corresponding to each school
    topurl = "https://www.104.com.tw/jb/career/department/navigation"
    select_schools = "body > div.container > div > div.mb.span-24 > dl > dd > ul > li > a"
    schoolurl = read_html(topurl) %>% html_elements(select_schools) %>% html_attr("href")
    
    #deal with discrepancy between 104 and MOE names
    sublist = "私立|\\(.\\)|\\(..\\)|\\(...\\)|\\(....\\)|\\(.....\\)|\\(......\\)"
    names = read_html(topurl) %>% html_elements(select_schools) %>% html_text()
    names = gsub(sublist, "", names)
    
    #prepare tibbles
    schools = tibble(
      name = names,
      schsal = NA,
      url = schoolurl
    )
    
    majorurl = tibble(
      school = character(),
      index = character(),
      url = character()
    )
    
    #find the urls corresponding to schools and majors
    for(i in c(1:nrow(schools))){
      
      #get all the majors under school i
      urls = NA
      urls = read_html(str_c("https://www.104.com.tw", schoolurl[i])) %>% html_elements(".a2") %>% html_attr("href")
      
      #get urls for these majors
      for(j in c(1:length(urls))){
        t1 = urls[j] %>% str_split("mid=")
        majorurl = majorurl %>% add_row(
          school = names[i],
          url = urls[j],
          index = t1[[1]][2]
        )
      }
    }
    
    #since only distribution is given on 104 page, define the function that approximate average salary from distribution
    dist_to_av = function(temp){
      if(is.na(temp[1])){return(NA)}
      salary = 0
      plast = 0
      for(j in c(1:length(temp))){
        t = temp[j] %>% str_split("%")
        port = (as.double(t[[1]][1])-plast)/100 - plast
        aver = as.integer(str_extract(t[[1]][2],"\\d\\d\\d\\d\\d")) + 5000
        salary = salary + port*aver
        plast = port + plast
      }
      return(salary)
    }
    
    #add new columns into mns to prepare for adding salary
    mns = mns %>% add_column(avsalary = NA, salamethod = NA)
    
    for(i in c(1:nrow(mns))){ 
      temp = NA
      if(is.na(mns$ma_index[i])){next}
      
      #for a school-major in mns, get the corresponding school url from majorurl
      match = subset(majorurl, school == mns$school[i]) %>% subset(index == mns$majorl_index[i])
      
      #get the major specific webpage by adding the url retrived earlier
      url <- str_c("https://www.104.com.tw", match$url[1])
      if(is.na(url)){next}
      
      #get the webpage, the css selector select the texts related to the distribution of wage 
      page <- read_html(url)
      major_average_select = "body > div.container > div > div.ls.span-17 > div.m-box.w-salaryReport > div.content > ul > li > div > div.main > div > div.content"
      temp <- page %>% html_nodes(major_average_select) %>% html_text()
      
      #calculate the average wage using function defied ealier
      mns$avsalary[i] = dist_to_av(temp)
      
    }
    
    #write data to csv for future use 
    write.csv(mns[c("index", "avsalary")], filepath)
  }
  
  #if the major have salary record on 104, diectsal = true
  mns$directsal <- (!is.na(mns$avsalary))
  return(mns)
}

#call the function, use method = 1 if want to scrap from 104 again, but takes a while
mns <- getsalary(mns, 0, "104salary.csv")

mns <- mns %>% rename(sal_match = directsal, salary = avsalary)

# Data from MOE  ==============================================================================

#student teacher ratio
teacher_path = "majorteacher.csv"
teacher <- read.csv(teacher_path, header = FALSE, skip = 1, check.names = FALSE, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble()
teacher <- teacher %>% subset(V1 == 109) %>%  transmute(ma_index = as.factor(as.integer(V6)), sc_index = as.factor(as.integer(V4)), teacher = as.integer(V8)) 

#data saparates bachelor, master, and phd students and teachers, add them together since they share the resources
teacher <- teacher %>% group_by(ma_index, sc_index) %>% summarize(.groups = "keep", teacher = sum(teacher))

student_path = "majorstudent.csv"
student <- read.csv(student_path, header = FALSE, skip = 1, check.names = FALSE, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble()
student <- student %>% subset(V1 == 109) %>%  transmute(ma_index = as.factor(as.integer(V6)), sc_index = as.factor(as.integer(V4)), student = as.integer(V9)) 
student <- student %>% group_by(ma_index, sc_index) %>% summarize(.groups = "keep", student = sum(student))

ts <- merge(student, teacher, by = c("ma_index", "sc_index"))
ts <- ts %>% mutate(ts_ratio = student/teacher)

tuition_path = "tuition.csv"
tuition <- read.csv(tuition_path, header = FALSE, skip = 1, check.names = FALSE, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble()
tuition <- tuition %>% subset(V1 == 109) %>%  transmute(ma_index = as.factor(as.integer(V6)), sc_index = as.factor(as.integer(V4)), tuition = as.integer(V9)+as.integer(V10))
#tuition is the sum of 學費+雜費

mns <- merge(mns, ts, by = c("ma_index", "sc_index"), , all.x = TRUE, all.y = FALSE)
mns <- merge(mns, tuition, by = c("ma_index", "sc_index"), , all.x = TRUE, all.y = FALSE)


rm(student, teacher, ts, tuition)

# Schools data  ==============================================================================

#school-wise data is from https://udb.moe.edu.tw/

ts_path = "teacherstudent.csv"
ts_ratio <- read.csv(ts_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble
ts_ratio <- ts_ratio %>% subset(V1 == 109) %>%  transmute(index = as.factor(as.integer(V4)), sc_size = V6, sc_teacher = V7, scts_ratio = V8) 

financial_pu_path = "financial_public.csv"
finan_pu <- read.csv(financial_pu_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble
finan_pu <- finan_pu %>% subset(V1 == 109) %>%  transmute(index = as.factor(as.integer(V4)), asset = V7, debt_ratio = V8) 

financial_pr_path = "financial_private.csv"
finan_pr <- read.csv(financial_pr_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble
finan_pr <- finan_pr %>% subset(V1 == 109) %>%  transmute(index = as.factor(as.integer(V4)), asset = V7, debt_ratio = V8) 

finan <- rbind(finan_pr, finan_pu)

budget_pu_path = "budget_public.csv"
bud_pu <- read.csv(budget_pu_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble
bud_pu <- bud_pu %>%  subset(V1 == 109) %>% transmute(index = as.factor(as.integer(V4)), income = V7, tuition_ratio = V8)

budget_pr_path = "budget_private.csv"
bud_pr <- read.csv(budget_pr_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble
bud_pr <- bud_pr %>%  subset(V1 == 109) %>% transmute(index = as.factor(as.integer(V4)), income = V7, tuition_ratio = V8)

budget <- rbind(bud_pr, bud_pu)

budget <- merge(budget, ts_ratio, by = "index", all.x = TRUE, all.y = FALSE)
budget <- merge(budget, finan, by = "index", all.x = TRUE, all.y = FALSE)
budget <- budget %>% mutate(income_per_student = income / sc_size)

#create a dataset for school-wise comparison
schools <- mns %>% subset(scma_match) %>% group_by(school) %>% summarize(index = last(sc_index, order_by = scma_match),
                                                                         public = last(public, order_by = scma_match),
                                                                         score = mean(adav_score, na.rm = TRUE), 
                                                                         salary = mean(salary, na.rm = TRUE), 
                                                                         tuition = mean(tuition, na.rm = TRUE),
                                                                         sal_ratio = sum(sal_match)/n(), 
                                                                         med_school = sum((bigdom_index == 91)*size, na.rm = TRUE)/sum(size, na.rm = TRUE), 
                                                                         majorcount = n())
schools <- merge(schools, budget, by = "index", all.x = TRUE, all.y = FALSE)

mns <- merge(mns, budget, by.x = "sc_index", by.y = "index")

summary(schools)

rm(finan, finan_pr, finan_pu, ts_ratio, bud_pr, bud_pu, budget, school_sal_coef, school_score_coef)

# Domains data  ==============================================================================

#there is discrepency between domain indexing betwwen old and new data, so need to get reference table, original is in pdf so transformed to csv with tabula
#source: https://stats.moe.gov.tw/files/bcode/10609_5%E6%9C%80%E8%BF%91%E5%85%A9%E6%AC%A1%E4%BF%AE%E6%AD%A3%E6%9E%B6%E6%A7%8B%E5%B0%8D%E7%85%A7%E8%A1%A8.pdf

reference <- read.csv("reference.csv",check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble() %>% transmute(new = V2, old = V3) 

reference <- reference %>% mutate(old = str_extract(old, "\\d+"), new = str_extract(new, "\\d+")) %>% 
                           mutate(old = as.factor(old), new = as.character(as.integer(new))) %>% subset(!(is.na(old))|!(is.na(new)))

newindex = NA

for (i in c(1:nrow(reference))){
  if(!is.na(reference$new[i])){
    newindex = reference$new[i]
  } else {
    reference$new[i] = newindex
  }
}

reference <- reference %>% subset(!is.na(old))

#https://data.gov.tw/dataset/31158
domain_outcome_path = "學類.csv"
domout <- read.csv(domain_outcome_path, check.names = FALSE, header = FALSE, skip = 1, stringsAsFactors=FALSE, fileEncoding = "UTF-8-BOM") %>% as_tibble()
domout <- domout %>% transmute(old = as.factor(V1), old_dom = V2, salary_4y = as.integer(V19), employ_4y = as.double(str_extract(V20, "\\d+"))/100)

domout <- merge(domout, reference, by = "old")

#old domain is more detailed than new, so grouped into statistics for new domain
domout <- domout %>% group_by(new) %>% summarise(salary_4y = mean(salary_4y, na.rm = TRUE),
                                                 employ_4y = mean(employ_4y, na.rm = TRUE)) %>% 
                     mutate(new = as.factor(as.integer(new))) %>% rename(index = new)

#create a dataset for domain-wise comparison
domains <- mns %>% subset(scma_match) %>% group_by(domain) %>% summarize(index = as.character(last(dom_index)),
                                                                         stem = last(stem),
                                                                         area = last(area),
                                                                         bigdomain = last(bigdomain),
                                                                         bigdom_index = last(bigdom_index),
                                                                         score = mean(adav_score, na.rm = TRUE), 
                                                                         salary = mean(salary, na.rm = TRUE), 
                                                                         tuition = mean(tuition, na.rm = TRUE),
                                                                         sal_ratio = sum(sal_match)/n(), 
                                                                         count = n(),
                                                                         size = sum(size, na.rm = TRUE)) %>% subset(count > 5)
domains <- merge(domains, domout, by = "index", all.x = TRUE, all.y = FALSE)

mns <- merge(mns, domout, by.x = "dom_index", by.y = "index")

summary(domains)

rm(domout, reference, domain_sal_coef, domain_score_coef)

# School-Major Exploration  ==============================================================================

summary(mns)

#are majors with missing index different from others? 
mns %>% ggplot(aes(x = scma_match, y = size)) + geom_boxplot() #smaller
mns %>% ggplot(aes(x = scma_match, y = adav_score)) + geom_boxplot() #worse

#are majors with missing salary different from others? 
mns %>% ggplot(aes(x = sal_match, y = size)) + geom_boxplot() #smaller
mns %>% ggplot(aes(x = sal_match, y = adav_score)) + geom_boxplot() #a bit better

#distribution
mns %>% ggplot(aes(x = adav_score))+geom_density()
mns %>% ggplot(aes(x = salary))+geom_density()
#compare to taiwan's income distribution

#how does score relates to different variables

#area
mns %>% subset(scma_match) %>% ggplot(aes(x = salary, y = adav_score, color = stem)) + geom_point()
mns %>% subset(scma_match) %>% ggplot(aes(x = area, y = adav_score)) + geom_boxplot()
mns %>% subset(scma_match) %>% ggplot(aes(x = area, y = salary)) + geom_boxplot()

#public
mns %>% subset(scma_match) %>% ggplot(aes(x = salary, y = adav_score, color = public)) + geom_point() + geom_smooth(method = lm)

#student-teacher ratio
mns %>% subset(scma_match) %>% ggplot(aes(x = ts_ratio, y = adav_score, color = public)) + geom_point() + coord_fixed(xlim = c(0, 75)) + stat_ellipse()
mns %>% subset(scma_match) %>% ggplot(aes(x = area, y = ts_ratio)) + geom_boxplot()

#tuition
mns %>% subset(scma_match) %>% ggplot(aes(x = public, y = tuition)) + geom_boxplot()
mns %>% subset(scma_match) %>% ggplot(aes(x = area, y = tuition)) + geom_violin()
mns %>% subset(scma_match) %>% subset(public) %>% ggplot(aes(x = score, y = tuition, color = area)) + geom_point()
# School Exploration  ==============================================================================

#does school with more majors that don't have salary data different from others?
schools %>% ggplot(aes(x = sal_ratio, y = score, color = public)) + geom_point()
schools %>% ggplot(aes(x = sal_ratio, y = size, color = public)) + geom_point()

#how does score relates to salary
schools %>% ggplot(aes(x = salary, y = score, color = public))+geom_point()+geom_smooth(method = lm)

#competitive private school are medical schools
schools %>% ggplot(aes(x = salary, y = score, color = (med_school>.5)|public)) + geom_point()

#medical schools are really different
schools %>% ggplot(aes(x = med_school>.5, y = score)) + geom_boxplot()
schools %>% ggplot(aes(x = med_school>.5, y = asset)) + geom_boxplot()

#how does teacher-studet ratio relate to  score
schools %>% ggplot(aes(x = ts_ratio, y = score, color = public))+geom_point()

#financials and score
schools %>% ggplot(aes(x = asset, y = score, color = public))+geom_point()
schools %>% ggplot(aes(x = debt_ratio, y = score, color = public))+geom_point()
schools %>% ggplot(aes(x = income_per_student, y = score, color = public))+geom_point()
schools %>% ggplot(aes(x = tuition_ratio, y = score, color = public))+geom_point()
schools %>% ggplot(aes(x = tuition, y = score, color = public))+geom_point()

# Domain Exploration  ==============================================================================

#how related are the two estimates of salary, not really.......
d <- domains %>% subset(!is.na(salary_4y))
cor(d$salary, d$salary_4y)

#excluding medical and small sample size domains, bit better
d <- d %>% subset(bigdom_index != 91) %>% subset(sal_ratio >= .25)
cor(d$salary, d$salary_4y)

d %>% ggplot(aes(x = salary, y = salary_4y))+geom_point()

#By domain
#since salary_4y is official statistics, we proceed with it

domains %>% ggplot(aes(x = salary_4y, y = score, color = (bigdom_index == 91)))+geom_point() #medical related majors are outliers

domains %>% subset(bigdom_index != 91) %>% ggplot(aes(x = salary_4y, y = score))+geom_point()+geom_smooth(method = lm)

areas <- domains %>% group_by(area) %>% summarize(emp = mean(employ_4y, na.rm = TRUE), sal = mean(salary_4y, na.rm = TRUE), score = mean(score), na.rm = TRUE)
areas %>% ggplot(aes(x = sal, y = emp, color = area, size = 10)) + geom_point()
areas %>% ggplot(aes(x = sal, y = score, color = area, size = 10)) + geom_point()

# Analysis ==============================================================================

#funciton for heteroskesticity-robust SE
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#score explanation
reg1_1 <- lm(adav_score ~ public + ts_ratio + stem + tuition, data = mns)
reg1_2 <- lm(adav_score ~ public + ts_ratio + stem + tuition + income_per_student, data = mns)
reg1_3 <- lm(adav_score ~ public + ts_ratio + stem + tuition + income_per_student + salary_4y + employ_4y, data = mns)


stargazer(reg1_1, reg1_2, reg1_3, 
          se=list(cse(reg1_1), cse(reg1_2), cse(reg1_3)), 
          title="Test", type="text", 
          df=FALSE, digits=3)

#salary explanation
reg2_1 <- lm(salary ~ public + ts_ratio + stem + tuition, data = mns)
reg2_2 <- lm(salary ~ public + ts_ratio + stem + tuition + income_per_student, data = mns)

stargazer(reg2_1, reg2_2,  
          se=list(cse(reg2_1), cse(reg2_2)), 
          title="Test", type="text", 
          df=FALSE, digits=3)

reg3_1 <- lm(salary ~ public + scts_ratio, data = schools)
reg3_2 <- lm(salary ~ public + med_school + scts_ratio, data = schools)
reg3_3 <- lm(salary ~ public + med_school + scts_ratio + tuition, data = schools)
reg3_4 <- lm(salary ~ public + med_school + scts_ratio + tuition + income_per_student , data = schools)

stargazer(reg3_1, reg3_2, reg3_3, reg3_4,
          se=list(cse(reg3_1), cse(reg3_2),cse(reg3_3),cse(reg3_4)), 
          title="Test", type="text", 
          df=FALSE, digits=6)

reg4_1 <- lm(score ~ public + ts_ratio, data = schools)
reg4_2 <- lm(score ~ public + med_school + scts_ratio, data = schools)
reg4_3 <- lm(score ~ public + med_school + scts_ratio + tuition_ratio, data = schools)
reg4_4 <- lm(score ~ public + med_school + scts_ratio + tuition_ratio + income_per_student , data = schools)

stargazer(reg4_1, reg4_2, reg4_3, reg4_4,
          se=list(cse(reg4_1), cse(reg4_2),cse(reg4_3),cse(reg4_4)), 
          title="Test", type="text", 
          df=FALSE, digits=6)
