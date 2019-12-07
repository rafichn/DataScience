library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "DSN=Collage;Trusted_Connection=yes;", timeout = 10)
classrooms <- dbReadTable(con, "Classrooms")
students <- dbReadTable(con, "Students")
teachers <- dbReadTable(con, "Teachers")
departments <- dbReadTable(con, "Departments")
courses <- dbReadTable(con, "Courses")

library(dplyr)

#quastion 1 

Q1<- inner_join(classrooms, courses, by="CourseId")
Q1a<-inner_join(Q1, departments, by="DepartmentId")
Q1a %>% group_by(DepartmentName) %>% summarise(count = n_distinct(StudentID))


##############
## Q2. How many students have each course of the English department and the 
##     total number of students in the department?
##############
Q2 <- (courses %>%   
         left_join(classrooms, by = "CourseId")%>% 
         left_join(departments, by = "DepartmentId")) %>%
        filter(DepartmentId == "1") %>%
  group_by(DepartmentId, CourseId, CourseName) %>% 
  summarise(num_of_students=(count=n()))
Q2
##############
## Q3. How many small (<22 students) and large (22+ students) classrooms are 
##     needed for the Science department?
##############
Q3 <- (courses %>% left_join(classrooms, by = "CourseId")) %>%   
  group_by(CourseId,CourseName) %>%  
  filter(DepartmentId == 2) %>%   
  summarise(num_of_students=n_distinct(StudentID))%>%   
  mutate(Classification = ifelse(num_of_students>21,"Big",    
                                 ifelse(num_of_students<22, "Small"))) %>%
  group_by(Classification)%>%  
  summarise(count=n()) 
Q3
##############
## Q4. A feminist student claims that there are more male than female in the 
##     College. Justify if the argument is correct
##############
Q4 <- students %>% 
  group_by(Gender) %>% 
  summarise(count=n())
Q4
##############
## Q5. For which courses the percentage of male/female students is over 70%?
##############
Q5 <- (courses %>% inner_join(classrooms, by = "CourseId") %>%  
  inner_join(students, by = "StudentID")) %>%
  group_by(CourseId, CourseName, Gender) %>%  
  summarise(Totalgender=n()) %>%      
  mutate(Percent = Totalgender/sum(Totalgender)*100) %>% 
  group_by(CourseId, CourseName, Percent)  %>%  
  filter(Percent > 70)
Q5
##############
## Q6. For each department, how many students passed with a grades over 80?
##############

Q6 <- (classrooms %>% left_join(courses, by = "CourseId") %>% 
         left_join(departments, by = "DepartmentId")) %>% 
  filter(degree > 80) %>%  
  group_by(DepartmentId, DepartmentName) %>%  
  summarise(num_of_students=n_distinct(StudentID))
Q6
##############
## Q7. For each department, how many students passed with a grades under 60?
##############
Q7 <- (classrooms %>% left_join(courses, by = "CourseId") %>% 
         left_join(departments, by = "DepartmentId")) %>% 
  filter(degree < 60) %>%  
  group_by(DepartmentId, DepartmentName) %>%  
  summarise(num_of_students=n_distinct(StudentID))
Q7
##############
## Q8. Rate the teachers by their average student's grades (in descending order).
##############
Q8 <- (classrooms %>%
         left_join(courses, by = "CourseId") %>% 
         left_join(teachers, by = "TeacherId")) %>%
  group_by(TeacherId, FirstName, LastName) %>% #
  summarise(degree=mean(degree, na.rm = T)) %>% 
  arrange(desc(degree))
Q8
##############
## Q9. Create a dataframe showing the courses, departments they are associated with, 
##     the teacher in each course, and the number of students enrolled in the course 
##     (for each course, department and teacher show the names).
##############
Q9 <- (classrooms %>% 
         left_join(courses, by = "CourseId") %>% 
         left_join(teachers, by = "TeacherId") %>%
         left_join(departments, by = "DepartmentId")) %>%
  group_by(CourseId, CourseName, DepartmentId, DepartmentName, FirstName) %>% 
  summarise(num_of_students=n_distinct(StudentID))  
Q9
##############
## Q10. Create a dataframe showing the students, the number of courses they take, 
##      the average of the grades per class, and their overall average (for each student 
##      show the student name).
##############
Q10 <- (students %>% 
          inner_join(classrooms, by = "StudentID") %>%
          inner_join(courses, by = "CourseId")) %>%
  group_by(StudentID, FirstName, LastName) %>% 
  summarise(NumOfCourses=n(),TotalAVG=mean(degree, na.rm = T)) 

Q10a <- (students %>% 
          inner_join(classrooms, by = "StudentID") %>%
          inner_join(courses, by = "CourseId") %>%
          inner_join(departments, by = "DepartmentId")) %>% 
  group_by(StudentID, FirstName, LastName, DepartmentName) %>%
  summarise(avg_by_dep=mean(degree, na.rm = T))


Q10b <-(Q10 %>% inner_join(Q10a, by = c("StudentID","FirstName","LastName"))) %>%
  group_by (StudentID, FirstName, LastName, NumOfCourses, TotalAVG)

library(tidyr)

Q10c <- spread(data = Q10b, key = DepartmentName, value = avg_by_dep)
