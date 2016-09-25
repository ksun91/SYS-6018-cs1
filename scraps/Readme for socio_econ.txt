Readme for socio_econ.csv
The columns "average.SAT.score", "High.school.drop.", and "TotStud"
where created by data from the files "HighSchools.xlsx", "filtered_SAT_campus.xlsx" and "filtered_drop_out_campus.xlsx"

Data from "HighSchools.xlsx" were taken from http://www.greatschools.org/texas/austin/schools/?gradeLevels=h
Data from "filtered_SAT_campus.xlsx" and "filtered_drop_out_campus.xlsx" were taken from http://tea.texas.gov/Reports_and_Data/Student_Data/

"average.SAT.score" was created by summing up Math, Reading, and Writing SAT scores

"TotStud" refers to the total amount of students in the zipcode for the calculation of "average.SAT.score" and 
"High.school.drop." 

IMPORTANT: the case1 team needs to decide whether to drop some of these data points for these columns due to low "TotStud"

NOTE: There exist more zipcodes than highschools. Also, there are some missing high schools from http://tea.texas.gov/Reports_and_Data/Student_Data/ 
