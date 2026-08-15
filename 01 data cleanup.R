dx <- read.csv("combined-data.csv")

names(dx)[3] <- "finalexam"
names(dx)[6] <- "attend"

# analyze final course grade and final exam grade separately?
dx$has.exam <- ifelse(is.na(dx$finalexam), 0, 1)
dx$has.grade <- ifelse(is.na(dx$grade), 0, 1)

ftable(has.exam~has.grade, data=dx)

# attendance can't be <0 or >100
dx$attend <- pmax(0, pmin(100, dx$attend))

# define final exam and course letter grades
dx$fe.letter <- cut(dx$finalexam,
                    breaks=c(-Inf, 60, 70, 80, 90, Inf),
                    labels=c("F", "D", "C", "B", "A"),
                    right=FALSE)
dx$co.letter <- cut(dx$grade,
                    breaks=c(-Inf, 60, 70, 80, 90, Inf),
                    labels=c("F", "D", "C", "B", "A"),
                    right=FALSE)

# order factors
dx$sem <- factor(dx$sem, levels=
                    c("Spring24", "Spring25", "Sum25", "Fall25","Spring26"),
                   ordered=TRUE)

# characterize data
table(dx$sem)
# Spring24 Spring25    Sum25   Fall25 Spring26 
#       66      162       37      100      299 

table(dx$class)
# biol 1107 biol 3300 chem 1211 chem 3361 math 1190 math 2202 
#       134       105       228        60        37       100 

ftable(sem~class, data=dx)
#          sem Spring24 Spring25 Sum25 Fall25 Spring26
#class                                                
#biol 1107            0        0     0      0      134
#biol 3300            0        0     0      0      105
#chem 1211           66      162     0      0        0
#chem 3361            0        0     0      0       60
#math 1190            0        0    37      0        0
#math 2202            0        0     0    100        0

# clean up
dx$has.exam <- NULL
dx$has.grade <- NULL
dx$letter <- NULL
names(dx)[4] <- "coursepct"

# attendance is expressed in percentage participation
# i.e., 100 - %absent
# for a MWF 50 minute class:
# 14 weeks * 3 meetings/week * 50 min/meeting = 2100 minutes
# 1 absence = 50 / 2100 = 2.4%

# for a 75 minute class:
# 14 weeks * 2 meetings/week * 75 min/meeting = 2100 minutes
# 1 absence = 75 / 2100 = 3.6%

# write out cleaned up version
write.csv(dx, "cleaned-data.csv", row.names=FALSE)

# end script!
