transitp <- function(data,x,y,z,n)
{
  d.per <- filter(data, intvp==x &(align==z) & gnum<100)
  d.per_temp <- filter(data, intvp==x & gnum==y & (align==z)) #group 2 is the starndard level
  d.per_temp2<-d.per_temp
  for (i in 1:(n-2))
  {
    d.per_temp2 <- rbind(d.per_temp2,d.per_temp)  
  }
  
  d.per$savgp <- d.per_temp$AvgPerformance
  d.per$ravgp <- d.per$AvgPerformance - d.per$savgp
  
  ggplot(data=d.per, aes(x=time, y=ravgp, group=group, colour=group)) + 
    geom_line(aes(linetype=group)) +
    scale_colour_hue (name="Level") +
    scale_linetype_discrete(name="Level") +
    xlab("Time Period") +
    ylab("Performance Difference") +
    ggtitle("Transitent learning performance difference by level") +
    theme_bw()
}

transitcb <- function(data,x,y,z,k,j,l)
{
  
  d.per <- filter(data, intvp==x &(align==z) & gnum<100)
  d.per_temp <- filter(d.h, intvp==x & gnum==y & (align==z)) #group y is the starndard level
  d.per_temp <- rbind(d.per_temp,d.per_temp,d.per_temp) #this changes according to the # of levels
  d.per$savgp <- d.per_temp[[k]]
  d.per$ravgp <- d.per[[k]] - d.per$savgp
  
  ggplot(data=d.per, aes(x=time, y=ravgp, group=group, colour=group)) + 
    geom_line(aes(linetype=group)) +
    scale_colour_hue (name="Level") +
    scale_linetype_discrete(name="Level") +
    xlab("Time Period") +
    ylab(j) +
    ggtitle(l) +
    theme_bw()
}

transitls<- function(x){
  #------------------------------------------------------#
  # Report 002
  ggplot(data=x, aes(x=time, y=change, group=halign, colour=halign)) + 
    geom_line(aes(linetype=halign)) +
    scale_colour_hue (name="H.interv") +
    scale_linetype_discrete(name="H.interv") +
    xlab("Time Period") +
    ylab("Average Num. of Local Per Superiors") +
    ggtitle("Transitent Number of Local Performance Superiors") +
    theme_bw()
  #------------------------------------------------------#
}
transitdis<- function(x){
  #------------------------------------------------------#
  # Report 002
  ggplot(data=x, aes(x=time, y=dissimilarity, group=halign, colour=halign)) + 
    geom_line(aes(linetype=halign)) +
    scale_colour_hue (name="H.interv") +
    scale_linetype_discrete(name="H.interv") +
    xlab("Time Period") +
    ylab("Average Local Knowledge Dissimilarity") +
    ggtitle("Transitent local knowledge dissimilarity") +
    theme_bw()
  #------------------------------------------------------#
}

plotper<-function(x)
{
ggplot(data=x, aes(x=align, y=AvgPerformance, group=intvp, colour=intvp, shape=intvp)) + 
  geom_line(aes(linetype=intvp)) + 
  geom_point(size=3) +
  scale_colour_hue (name="Policy") +
  scale_shape_manual (name="Policy", values=c(22,21)) +
  scale_linetype_discrete(name="Policy") +
  xlab("Degree of hierarchical intervention") +
  ylab("End Period Average Performance") +
  ggtitle("Learning Performance over increasing hierarchical intervention") +
  theme_bw()
#------------------------------------------------------#
}

plotcbit <- function(x,z)
{
  m=x[[z]]
  #x=d.plot
  #z="GroupCbit"
  x$m <- m
  ggplot(data=x , aes(x=align, y=m, group=intvp, colour=intvp)) +
    geom_line(aes(linetype=intvp)) + 
    geom_point(size=3) +
    scale_colour_hue (name="Policy") +
    scale_shape_manual (name="Policy", values=c(22,21)) +
    scale_linetype_discrete(name="Policy") +
    xlab("Degree of hierarchical intervention") +
    ylab("End Period Average Correct Bit") +
    ggtitle("End period correct bits over increasing hierarchical intervention") +
    theme_bw()
  #------------------------------------------------------#
}

plotcbit2 <- function(x)
{
  ggplot(data=x , aes(x=align, y=GCbit_sh, group=group, colour=group)) +
    geom_line(aes(linetype=group)) + 
    geom_point(size=3) +
    scale_colour_hue (name="Group") +
    scale_shape_manual (name="Group", values=c(22,21)) +
    scale_linetype_discrete(name="Group") +
    xlab("Degree of hierarchical intervention") +
    ylab("End Period Average Correct Bit") +
    ggtitle("End period correct bits over increasing hierarchical intervention") +
    theme_bw()
}

plotdis_hint <- function(x,z)
{
  m=x[[z]]
  #x=d.plot
  #z="GroupCbit"
  x$m <- m
  ggplot(data=x , aes(x=align, y=m, group=intvp, fill=intvp)) +
#    geom_line(aes(linetype=intvp)) + 
#    geom_point(size=3) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_hue(name="Policy") +      # Set legend title  
#  scale_colour_hue (name="Policy") +
#   scale_shape_manual (name="Policy", values=c(22,21)) +
#   scale_linetype_discrete(name="Policy") +
    xlab("Degree of hierarchical intervention") +
    ylab("End Period Dissimilarity") +
    ggtitle("End period dissimilarity over increasing hierarchical intervention") +
    theme_bw()
  #------------------------------------------------------#
}

plotdis_gnum <- function(x,z)
{
  m=x[[z]]
  #x=d.plot
  #z="GroupCbit"
  x$m <- m
  ggplot(data=x , aes(x=group, y=m, group=halign, fill=halign)) +
    #    geom_line(aes(linetype=intvp)) + 
    #    geom_point(size=3) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_hue(name="H.Intv") +      # Set legend title  
    #  scale_colour_hue (name="Policy") +
    #   scale_shape_manual (name="Policy", values=c(22,21)) +
    #   scale_linetype_discrete(name="Policy") +
    xlab("Group") +
    ylab("End Period Dissimilarity") +
    ggtitle("End period dissimilarity over different hierarchical level") +
    theme_bw()
  #------------------------------------------------------#
}

dist_avgper <- function(dd,bin)
{
  ggplot(dd, aes(x = AvgPerformance)) + 
    geom_histogram(binwidth = bin, fill = 'grey40', colour = 'white') +
    #scale_x_continuous(breaks = seq(0, 100, 20), limits = c(-5, 100), 
    #                   expand = c(0, 0)) +
    #scale_y_continuous(breaks = seq(0, 40, 10), expand = c(0, 2)) +
    #scale_x_log10()+
    labs(x = "Avg Per", y = "Count of orgs") +
    theme_bw() 
  #theme(panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank())
}
diffusion <- function(dd)
{
  ggplot(data=dd, 
         aes(x=time, y=AvgPerformance, group=scenario_no,colour=scenario_no)) + 
    geom_line() + 
    geom_point()
}
  
den_1 <- function(dd, bin)
{
  ggplot(dd, aes(x=AvgPerformance, ..density.., colour = scenario_no)) + geom_freqpoly(binwidth = bin) 
}

den_2 <- function(dd,bin)
{
  ggplot(dd, aes(first_diff, ..density.., colour = scenario_no)) +
    scale_x_log10() +
    geom_freqpoly(binwidth = bin) 
  
}
den_3 <- function(dd,bin)
{
  ggplot(dd, aes(first_diff, ..density.., colour = scenario_no)) +
    #scale_x_log10() +
    scale_y_log10() +
    geom_freqpoly(binwidth = bin) 
  
}

cal_diff1 <- function(dd,l)
{
  temp<-dd %>%
    group_by(trial_no,scenario_no, gnum) %>%
    mutate(first_diff = abs(AvgPerformance - lag(AvgPerformance,l))/lag(AvgPerformance,l))
  return (temp)
}

cal_diff2 <- function(dd,l)
{
  temp<-dd %>%
    group_by(trial_no,scenario_no, gnum) %>%
    mutate(first_diff = (AvgPerformance - lag(AvgPerformance,l)))
  return (temp)
}