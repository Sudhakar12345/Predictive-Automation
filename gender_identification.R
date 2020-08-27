library(gender)
names = c("rahul","sandeep","ankit","sarvesh","raja","baby","saneesh","rama","krishna","rani","sruthi","priyanka","rashmi","devayani","sneha")
gender(names, years = c(1932, 2012), method = c("ssa", "ipums", "napp",
                                                "kantrowitz", "genderize", "demo"), countries = c("United States", "Canada",
                                                                                                  "United Kingdom", "Germany", "Iceland", "Norway", "Sweden","India"))
library(genderizeR)
genderize(names, genderDB, blacklist = NULL, progress = TRUE)
x = c("Winston J. Durant, ASHP past president, dies at 84",
      "Gold Badge of Honour of the DGAI Prof. Dr. med. Norbert R. Roewer Wuerzburg",
      "The contribution of professor Yu.S. Martynov (1921-2008) to Russian neurology",
      "JAN BASZKIEWICZ (3 JANUARY 1930 - 27 JANUARY 2011) IN MEMORIAM",
      "Maria Sklodowska-Curie")
givenNames = findGivenNames(x)
givenNames = givenNames[count>40]
genderize(x, genderDB=givenNames, blacklist=NULL)
