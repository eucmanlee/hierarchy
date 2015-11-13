
#Date: 14Jun2015
#Author: Eucman Lee
#Proj: Interpersonal Learning across different NW architecture
#Desc: 
# This script is a reporting and analysis routine for inter-personal learning simulation.
# The corresponding reporting functions are specified below.

#Date  : 11Nov2015
#Author: Eucman Lee
#Proj  : Interpersonal Learning on organizational hierarchy
#Desc: : 
# This script is about generating analysis reports for inter-personal learning simulation.
# First read output data from the dedicated folders and combine them to generate a predefined
# graphs written in R.




###################################################
#package installations
###################################################

# The following packages are supposed to be installed once. 
# install.packages("dplyr"): This will provide a greate flexibility on data manuplation
# install.packages("gridExtra")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("sqldf")
require(plyr)
require(gridExtra) #arrangeGrob()
require(dplyr)
require(plyr)
require(ggplot2)
require(sqldf)
options(encoding = "UTF-8")


#source reporting files
setwd("/home/eucman/proj/hrcy/02_rcode")
source('importfiles.r')  #Importing files function
source('report.r')       #Reporting functions

#the raw data location.
setwd("/home/eucman/proj/hrcy/04_orglearning/04_scenarios/004_SN_RealHierarchy/02_result")


#READ FILES

#1 impf is supposed to read files using file name pattern
files <- c("Result.csv", "Result5.csv","Result6.csv")
h.raw <- NULL #let's make sure that the variable is empty
h.raw <- impf(files) #the imported files often find to be problematic. e.g. read numbers as factor or vise versa.

h.raw <- transform(h.raw, 
                   scenario_num=as.numeric(as.character(scenario_no)),
                   scenario_no=as.factor(as.character(scenario_no)),
                   AvgPerformance=as.numeric(as.character(AvgPerformance)),
                   dissimilarity=as.numeric(as.character(dissimilarity)),
                   intvp_num = as.numeric(as.factor(intvp)),
                   time=as.numeric(as.character(time)), 
                   GroupCbit=as.numeric(as.character(GroupCbit)),
                   GCbit_fh=as.numeric(as.character(GCbit_fh)),
                   GCbit_sh=as.numeric(as.character(GCbit_sh)),
                   group=as.factor(group), intvp=as.factor(intvp), 
                   change=as.numeric(as.character(change)), 
                   gnum=as.numeric(as.character(group)),
                   halign=as.factor(as.character(align))
                   )

d.h   <- select(h.raw, 
                scenario_no,scenario_num,
                AvgPerformance,GroupCbit,
                GCbit_fh, 
                GCbit_sh, 
                group,
                time, 
                align,
                halign,
                gnum, 
                change,
                intvp,
                dissimilarity,
                S
                )


#2 import individual obs when it is available
#This is used when the obs of individual run (i.e. the results are averaged over N number of trials)
d.id.raw <-impf("indif")
d.id <- transform(d.id.raw, 
                  scenario_num=as.numeric(as.character(scenario_no)),
                  scenario_no=as.factor(as.character(scenario_no)),
                  trial_no=as.numeric(as.character(trial_no)),
                  AvgPerformance=as.numeric(as.character(AvgPerformance)),
                  dissimilarity=as.numeric(as.character(dissimilarity)),
                  time=as.numeric(as.character(time)), 
                  GroupCbit=as.numeric(as.character(GroupCbit)),
                  GCbit_fh=as.numeric(as.character(GCbit_fh)),
                  GCbit_sh=as.numeric(as.character(GCbit_sh)),
                  group=as.factor(group), 
                  intvp=as.factor(intvp), 
                  change=as.numeric(as.character(change)), 
                  gnum=as.numeric(as.character(group)),
                  halign=as.factor(as.character(align))
                  )

d.id <- select(d.id, scenario_no,scenario_num,trial_no,AvgPerformance,GroupCbit,GCbit_fh, GCbit_sh, group,time, align,halign,gnum, change,intvp,dissimilarity)
#d.id <- filter(d.id, trial_no !=NA)


#3. Plot of simple diffusion by scenario
temp<- filter(d.id, gnum==100, scenario_num==5, trial_no<50, trial_no>40)
diffusion(temp)

ggplot(data=temp, 
       aes(x=time, y=AvgPerformance, group=trial_no,colour=trial_no)) + 
  geom_line() + 
  geom_point()

#4. Distribution: draw performance distirbution.
#dist_avgper(data, bin), bar chart
temp <-filter(d.id, gnum==100,scenario_num==1,time==500)
dist_avgper(temp,4)

temp <-filter(d.id, gnum==100,time==500)
temp %>% group_by(scenario_num) %>% 
  summarise(p.mean = mean(AvgPerformance), p.sd = sqrt(var(AvgPerformance)))

temp<-sqldf('select scenario_no, AvgPerformance from temp')


#den_1 (data,bin), density plot by scenario
temp <-filter(d.id,time==50,gnum==100)
den_1 (temp, 1)


#now calculate the first difference
#cal_diff(data,lagg)
sd.id3 <- filter(d.id,time>0, time<500)
temp<- cal_diff1(sd.id3, 5)
temp<- cal_diff2(sd.id3, 1)

#den_2 (data,bin), density plot by scenario
temp2<-filter(temp,gnum==3,scenario_num==1)
den_2(temp2,0.1)
den_3(temp2,4)


##################################################
# The beginning of the performance report
##################################################


##1.1 Plot Avg Per
#-- always better to check the patterns of trainsitent values

ggplot(data=filter(d.h, intvp==2 & gnum==100 &(align==0.0|align==0.1) & S==4), aes(x=time, y=AvgPerformance, group=halign, colour=halign)) + geom_line() + geom_point()

ggplot(data=filter(d.h, intvp==1 & gnum==2 &(align==0.0|align==0.1|align==0.7)), aes(x=time, y=GroupCbit, group=halign, colour=halign)) + geom_line() + geom_point()
ggplot(data=filter(d.h, intvp==1 & gnum==3 &(align==0.0|align==0.1|align==0.7)), aes(x=time, y=GCbit_fh, group=halign, colour=halign)) + geom_line() + geom_point()


ggplot(data=filter(d.h,gnum==100), 
       aes(x=time, y=AvgPerformance, group=scenario_no, colour=scenario_no)) + 
  geom_line() + geom_point()


#-- plot transitent patterns of average performance "difference"
# transitp(data,intvp,anchor level,align,num of levels)
# transitcb(intvp,anchor level,align, name of cbit, ylabel, title)
# 
temp=filter(d.h, scenario_num==5)
transitp(temp,2,3,0.0,7)
#transitcb(d.h,1,1,0.2,"GroupCbit", "Group correct bit difference", "Transitent Correct Bit performance difference by level")
#transitcb(d.h,1,1,0.1,"GCbit_sh", "First half Cbit difference", "Transitent Correct Bit(first half) performance difference by level")

##1.2 Correct Bit
ggplot(data=d.h, aes(x=time, y=GCbit_fh, group=group, colour=group)) + geom_line() + geom_point()
ggplot(data=d.h, aes(x=time, y=GCbit_sh, group=group, colour=group)) + geom_line() + geom_point()

##1.3 Num of Local Sup

#-- plot transient patterns of local superior frequency
#transitls (data)
d.change=filter(d.h, gnum==2& (intvp==1) &(align==0.0|align==0.1|align==0.7))
transitls(d.change)
transitdis(d.change)

#2.End period performance
d.he <-filter(d.h, time==500)
##2.1 End period avg performance over increasing align
ggplot(data=filter(d.he, intvp==1 & gnum==100), aes(x=align, y=AvgPerformance, group=group, colour=group)) + geom_line() + geom_point()
ggplot(data=filter(d.he, intvp==2 & gnum==100), aes(x=align, y=AvgPerformance, group=group, colour=group)) + geom_line() + geom_point()

# This is the end period performance by scenario
sv
ggplot(data=filter(d.he, gnum==100), aes(x=scenario_no, y=AvgPerformance)) + geom_line() + geom_point()
plot(d.he$scenario_no, d.he$AvgPerformance)
# plotper(data)
d.plot=filter(d.he, gnum==100)
plot(sv,d.plot$AvgPerformance)
plotper(d.plot)

##2.2 End period cor bit
ggplot(data=d.he, aes(x=align, y=GroupCbit, group=group, colour=group)) + geom_line() + geom_point()
ggplot(data=filter(d.he,(gnum==4)) , aes(x=align, y=GroupCbit, group=intvp, colour=intvp)) + geom_line() + geom_point()

ggplot(data=filter(d.he, (gnum<5) & (gnum>1)), aes(x=align, y=GroupCbit, group=group, colour=group)) + geom_line() + geom_point()
ggplot(data=d.he, aes(x=align, y=GCbit_fh, group=group, colour=group)) + geom_line() + geom_point()
ggplot(data=d.he, aes(x=align, y=GCbit_sh, group=group, colour=group)) + geom_line() + geom_point()

# plotcbit
# plotcbit(data,measure)
d.plot=filter(d.he,(gnum==2))
plotcbit(d.plot, "GroupCbit")
plotcbit(d.plot, "GCbit_sh")
plotdis_hint(d.plot, "dissimilarity")


d.plot=filter(d.he,intvp==1 &(align==0.0|align==0.1|align==0.7) & gnum<100)
#chage the level name
levels(d.plot$group)[levels(d.plot$group)=="3"]<-"Mgr"
levels(d.plot$group)[levels(d.plot$group)=="2"]<-"Group 1"
levels(d.plot$group)[levels(d.plot$group)=="1"]<-"Group 2"
plotdis_gnum(d.plot, "dissimilarity")

#------------------------------------------------------#
# Report 001
d.plot <- filter(d.he,(intvp==1)& gnum<100)
#plot correct by by group
#plotcbit2(data)
plotcbit2(d.plot)


##############################################################
