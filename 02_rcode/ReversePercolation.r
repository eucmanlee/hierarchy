#####################################################################
#Date:04Nov2015
#Author: Eucman Lee
#Proj:   Reverse percolation and hierarchical network
#Desc:
# I have devised to generate sample network for experiments.
# The networks scenarios include SF, random and hierarchic network.
# This script is designed to carry out a series of experiments of
# reverse percolation. You know what it is. 
# After those experimentation, a simple regraphs will will be provided.
# This is an outcome of an efforts to consolidate prior scripts. 
# e.g. I merged sub-functions in a one script doc.
######################################################################

###################################################
# (2015.09.24)
# Necessary package installations
###################################################
# install.packages('gridExtra')  #what is it?
require(plyr)
require(dplyr)
require(ggplot2)
require(igraph)      #network analysis lib
require(sqldf)       #you need this for MySql script
require(gridExtra)   #shit...what is it?

###################################################
# (2015.09.24)
# Setting Environment
###################################################

#set the working directory
setwd("/home/eucman/proj/hrcy/02_rcode")
options(encoding = "UTF-8")  #without encoding specification, error often appears. 

#########################################################
# (2015.08.13)
# Generating hierarchical network graphs
#########################################################
source("/home/eucman/proj/hrcy/02_rcode/GenHierarchy_subfunctions.r") #load subfunction file

# Below, you need to put some numbers to generate hierarchy. 
# Stupid, but working.
sc  <-6                        #sc :  span of control.
n   <-1+sc+sc^2 +sc^3          #n  :   middle management one level obove the leaf members.
n_r <-1+sc+sc^2 +sc^3 +sc^4    #n_r: size of population

#Generate team network: h with span of control sc
h<- g_teamnetwork(sc,n)

#Generate reporting structure - pure tree structure
report<-graph.tree(n_r, sc, mode=c("undirected"))

#combine reporting and team network: i.e. tree + team network 
final<-graph.union(h,report)
final<-simplify(final,remove.multiple=TRUE,remove.loop=TRUE) #eleminate dups

#Set level and name of each node: for simulation of inter-personal learning, 
#The name and levels are important. 
V(final)$lv<-shortest.paths(final,1)[1,] +1
V(final)$name <- as.character(V(final))

#Export networks: this netowrk file makes the simulation easier than prior shitty ones. 
level <- V(final)$lv
export_net(report,"formalnetwork_l7.txt",h,"teamnetwork_l7.txt",level,"individuallevel_l7.txt")



#########################################################
## (2015.08.13)
## Generating other networks for comparison.
#########################################################

# use the same num of nodes and edges: 
# I have intended to control those factors and
# appears to be each in this setting. 
nn<-vcount(final)
ne<-ecount(final)

### ER (i.e. Random Graph)
g.er <- erdos.renyi.game(nn, ne, type=c("gnm"), directed=F, loops=FALSE) #there are different types (i.e. generating mechanisms)
#g.er$layout<-layout.fruchterman.reingold (g.er,niter=10000)

### Scale free
g.sf <- static.power.law.game(nn, ne, exponent.out=3, exponent.in=-1, loops=F, multiple=F) #The scale free parameters may change in the future. (decisions are made in an ad hoc manner...oh..gosh.)
#g.sf$layout<-layout.fruchterman.reingold (g.sf,niter=10000)

# Plot graphs (do not plot when nn is large. It will stop all process.)
# plot(g.er, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Random Network")
# plot(g.sf, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Scale Free Network")

# Plot degree distribution (look cool)
p1 <- fit_power_law(g.er)  #random
p2 <- fit_power_law(g.sf)  #scale free
p3 <- fit_power_law(final) #hierarchy

# Merge plots
# This graph does not look cool.
#grid.arrange(p1, p2, p3, ncol=2)
#ggsave("DegDist.png", width=10, height=5)

# Import real hierarchy network and set the level
# The real hierarchy comes from a large korean telcom.
dat        <- read.csv(file = "network.csv", header = TRUE)
g.rh       <- graph.data.frame(dat, directed = FALSE)
t.node     <- V(g.rh)[V(g.rh)$name=="1"] #You need to know who is the BOSS in the hierarchy. So that you can cal the level from it. 
V(g.rh)$lv <- shortest.paths(g.rh,t.node)[1,] + 1

# Plot real hierarchy network: note that drawing large network is the stupid and time consuming job. 
# g.rh$layout <- layout.fruchterman.reingold (g.rh, niter = 10000)
# plot(g.rh, vertex.label = NA, edge.arrow.size = 0.02, vertex.size = 0.5, xlab = "Real Hierarchy")


#########################################
# Simulation - Randomly delete verteces
# This is the experiments carried out 
# by prior percolation related experiments
#########################################

#define report data frame: You need to create empty boxes. 
result<-data.frame(idx = numeric(),
                   nv  = numeric(),
                   ne  = numeric(),
                   apl = numeric(),
                   mcz = numeric(),
                   acz = numeric(),
                   type= as.character(),
                   sn  = as.character())



# Export graphs for 'Gephi'
# Gephi is an effective tool to draw large scale netowrks. 
# The function below appears to be very effective
# write.graph(g.rh, "hierarchy.gml", format = "gml")

## Assign graphs and specify the type of networks
## (What a stupid naming it is....)
g1      <- final
g2      <- g.er
g3      <- g.sf
g4      <- g.rh

g1.type <- 'HRY'    #Hierarchy
g2.type <- 'ER'     #E-R (Random)
g3.type <- 'SF'     #Scale free
g4.type <- 'R.HRY'  #Real hierarchy

## Three scenarios: attack three ways
## r(random), t(from the top of the hierarchy), b (from the bottom)
## scenarios are meaningful for hierarchy but why others? Stupid...
result <- r.attack(g1, g1.type, result)
result <- r.attack(g2, g2.type, result)
result <- r.attack(g3, g3.type, result)
result <- t.attack(g1, g1.type, result)
result <- t.attack(g2, g2.type, result)
result <- t.attack(g3, g3.type, result)
result <- b.attack(g1, g1.type, result)
result <- b.attack(g2, g2.type, result)
result <- b.attack(g3, g3.type, result)


## The attack(s) which are specifically designed for real hierarchy
## i.e. delete nodes either from
## higher rank (kh.attack), lower rank (kl.rank) or random (r.attack)
result <- r.attack (g4, g4.type, result)
result <- kh.attack(g4, g4.type, result)
result <- kl.attack(g4, g4.type, result)

##############################################################
# Experiments with real hierarchy network:
# You know, we need to repeat and average the simulation results
##############################################################

trials <- 100

# Sorry, I don't know whether I can set up seed number here or not

#Create a container
result.r<-data.frame(   trial=numeric,            #trials
                        idx=numeric(),            #index
                        nv=numeric(),             #num of nodes
                        ne=numeric(),             #num of edges
                        apl=numeric(),            #average path length
                        mcz=numeric(),            #max cluster size
                        acz=numeric(),            #average cluster size
                        type=as.character(),      #network type
                        sn=as.character())        #test scenario: e.g. bottom attack (:-)

#Exert simulations
result.r <- r.attack.r (g1, g1.type, result.r, trials)
result.r <- kh.attack.r(g1, g1.type, result.r, trials)
result.r <- kl.attack.r(g1, g1.type, result.r, trials)

###################################################
# Reports
###################################################

#MySQL is very useful here. :-)
rd1 <- sqldf('SELECT * FROM result WHERE type!="HRY" AND idx<100 AND sn!="b_Attack"')

gh1 <-  ggplot(data=rd1, aes(x=idx, y=apl, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size = 3, aes(shape = sn)) +
  xlab("Number of failure") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()

ggplot(data=rd1, aes(x=idx, y=mcz, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggplot(data=rd1, aes(x=idx, y=acz, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Avg size of other clusters") +
  ggtitle("Avg size of clusters over increasing number of node failure") +
  theme_bw()

ggplot(data=rd1, aes(x=idx, y=apl, group=type, color=type)) +
  facet_wrap(~sn) +
  geom_point(size=3, aes(shape=type)) +
  xlab("Number of failure") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()

rd2 <- sqldf('SELECT * FROM result WHERE sn!="Attack"')

ggplot(data=rd2, aes(x=idx, y=apl, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()

ggsave("result1.png",width=10, height=5)

ggplot(data=rd2, aes(x=idx, y=mcz, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggsave("result2.png",width=10, height=5)

ggplot(data=rd2, aes(x=idx, y=acz, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Avg size of other clusters") +
  ggtitle("Avg size of clusters over increasing number of node failure") +
  theme_bw()

ggsave("result3.png",width=10, height=5)

rd3<-sqldf('SELECT * FROM result WHERE (sn="kl_Attack" OR sn="kh_Attack" OR sn="Failure") AND type = "HRY" ')


#Normalize the time (i.e. idx) and max cluster size (i.e. mcz)
rd3$nidx=rd3$idx/max(rd3$idx)
rd3$nmcz=rd3$mcz/max(rd3$idx)


ggplot(data=rd3, aes(x=idx, y=apl, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()

ggsave("result1.png",width=10, height=5)

ggplot(data=rd3, aes(x=idx, y=mcz, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggsave("result2.png",width=10, height=5)

ggplot(data=rd3, aes(x=nidx, y=nmcz, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  scale_x_log10()+
  xlab("f (=fraction of failure, log10)") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggsave("result2.png",width=10, height=5)

ggplot(data=rd3, aes(x=idx, y=acz, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Avg size of other clusters") +
  ggtitle("Avg size of clusters over increasing number of node failure") +
  theme_bw()

ggsave("result3.png",width=10, height=5)


## Real hierarchy
rd3<-sqldf('SELECT * FROM result WHERE (sn="kl_Attack" OR sn="kh_Attack" OR sn="Failure") AND type="R.HRY" ')

#normalize the parameters
#idx (=time), mcz(=max cluster size), apl (=avg path length)
#acz (=average cluster size)
rd3$nidx=rd3$idx/max(rd3$idx)
rd3$nmcz=rd3$mcz/max(rd3$idx)
rd3$napl=rd3$apl/max(rd3$idx)
rd3$nacz=rd3$acz/max(rd3$idx)


ggplot(data=rd3, aes(x=idx, y=apl, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()

ggsave("result1.png",width=10, height=5)

ggplot(data=rd3, aes(x=nidx, y=napl, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  scale_x_log10()+
  xlab("f (=fraction of failure, log10)") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()


ggplot(data=rd3, aes(x=idx, y=mcz, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggsave("result2.png",width=10, height=5)

ggplot(data=rd3, aes(x=nidx, y=nmcz, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  scale_x_log10()+
  xlab("f (=fraction of failure, log10)") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggsave("result2.png",width=10, height=5)

ggplot(data=rd3, aes(x=idx, y=acz, group=sn, color=sn)) +
  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  xlab("Number of failure") +
  ylab("Avg size of other clusters") +
  ggtitle("Avg size of clusters over increasing number of node failure") +
  theme_bw()

ggsave("result3.png",width=10, height=5)


##################################
# Result: Mutiple trials
##################################
result.r$nidx=result.r$idx/max(rd3$idx)
result.r$nmcz=result.r$mcz/max(rd3$idx)
result.r$napl=result.r$apl/max(rd3$idx)
result.r$nacz=result.r$acz/max(rd3$idx)


ggplot(data=result.r, aes(x=nidx, y=nmcz, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  scale_x_log10()+
  xlab("f (=fraction of failure, log10)") +
  ylab("Size of largest cluster") +
  ggtitle("Size of largest cluster over increasing number of node failure") +
  theme_bw()

ggsave("result1.png",width=10, height=5)

ggplot(data=result.r, aes(x=nidx, y=napl, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  scale_x_log10()+
  xlab("f (=fraction of failure, log10)") +
  ylab("Diameter") +
  ggtitle("Diameter over increasing number of node failure") +
  theme_bw()

ggsave("result2.png",width=10, height=5)

ggplot(data=result.r, aes(x=nidx, y=nacz, group=sn, color=sn)) +
  #  facet_wrap(~type) +
  geom_point(size=3, aes(shape=sn)) +
  scale_x_log10()+
  xlab("f (=fraction of failure, log10)") +
  ylab("Avg size of other clusters") +
  ggtitle("Avg size of clusters over increasing number of node failure") +
  theme_bw()

ggsave("result3.png",width=10, height=5)


#################################################################
##subfunction(s) set 1
#################################################################
r.attack = function(g1,g1.type,result){
  
  
  g       <- g1
  g.type  <- g1.type
  nn      <- as.integer(vcount(g))
  
  for (i in seq(1:(nn-2))){
    vnum<-vcount(g)
    
    #  dg=degree(g)
    #  V(g)$dd=dg
    #  V(g)$drank=ave(V(gg)$dd, FUN = rank)
    #  which.max(V(g)$drank)
    
    random.deletes <- sample(1:vnum,1,replace=F)
    g<-delete_vertices(g, random.deletes)
    apl=average.path.length (g, directed=F, unconnected=TRUE)
    cl=clusters(g)
    mcz=max(cl$csize)
    acz<-0
    if (cl$no>1)  {
      acz<-(sum(cl$csize) - mcz)/(cl$no-1)
    }
    result <- rbind(result,
                    data.frame(
                      idx=i,
                      nv=vcount(g),
                      ne=ecount(g),
                      apl=apl,
                      mcz=mcz,
                      acz=acz,
                      sn='Failure',
                      type =g.type
                    )
    )
    
  }
  return(result)
}

r.attack.r =function(g1,g1.type,result,trial){
  
  for (j in seq(1:trial))
  {
    
    
    g<-g1
    g.type<-g1.type
    nn<-as.integer(vcount(g))
    for (i in seq(1:(nn-2))){
      vnum<-vcount(g)
      
      #  dg=degree(g)
      #  V(g)$dd=dg
      #  V(g)$drank=ave(V(gg)$dd, FUN = rank)
      #  which.max(V(g)$drank)
      random.deletes <- sample(1:vnum,1,replace=F)
      g<-delete_vertices(g, random.deletes)
      apl=average.path.length (g, directed=F, unconnected=TRUE)
      
      cl=clusters(g)
      mcz=max(cl$csize)
      acz<-0
      if (cl$no>1)  {
        acz<-(sum(cl$csize) - mcz)/(cl$no-1)
      }
      result <- rbind(result,
                      data.frame(
                        trial=j,
                        idx=i,
                        nv=vcount(g),
                        ne=ecount(g),
                        apl=apl,
                        mcz=mcz,
                        acz=acz,
                        sn='Failure',
                        type =g.type
                      )
      )
      
    }
  }
  return(result)
}

t.attack =function(g1,g1.type,result){
  g<-g1
  g.type<-g1.type
  nn<-as.integer(vcount(g) )
  for (i in seq(1:(nn-2))){
    vnum<-vcount(g)
    
    dg=degree(g)
    V(g)$dd=dg
    V(g)$drank=ave(V(g)$dd, FUN = rank)
    random.deletes<-which.max(V(g)$drank)[1]
    
    #random.deletes <- sample(1:vnum,1,replace=F)
    g<-delete_vertices(g, random.deletes)
    apl=average.path.length (g, directed=F, unconnected=TRUE)
    cl=clusters(g)
    mcz=max(cl$csize)
    acz<-0
    if (cl$no>1)  {
      acz<-(sum(cl$csize) - mcz)/(cl$no-1)
    }
    result <- rbind(result,
                    data.frame(
                      idx=i,
                      nv=vcount(g),
                      ne=ecount(g),
                      apl=apl,
                      mcz=mcz,
                      acz=acz,
                      sn='Attack',
                      type =g.type
                    )
    )
    
  }
  return(result)
}

b.attack =function(g1,g1.type,result){
  g<-g1
  g.type<-g1.type
  nn<-as.integer(vcount(g) )
  for (i in seq(1:(nn-2))){
    vnum<-vcount(g)
    
    dg=betweenness(g, normalized=T,directed=F)
    V(g)$dd=dg
    V(g)$drank=ave(V(g)$dd, FUN = rank)
    random.deletes<-which.max(V(g)$drank)[1]
    
    #random.deletes <- sample(1:vnum,1,replace=F)
    g<-delete_vertices(g, random.deletes)
    apl=average.path.length (g, directed=F, unconnected=TRUE)
    cl=clusters(g)
    mcz=max(cl$csize)
    acz<-0
    if (cl$no>1)  {
      acz<-(sum(cl$csize) - mcz)/(cl$no-1)
    }
    result <- rbind(result,
                    data.frame(
                      idx=i,
                      nv=vcount(g),
                      ne=ecount(g),
                      apl=apl,
                      mcz=mcz,
                      acz=acz,
                      sn='b_Attack',
                      type =g.type
                    )
    )
    
  }
  return(result)
}

#Targetted attack for hierarchical network with rank specified in V(g)$lv
## The higher management will be deleted first (kh.attack: attack higher rank management)
kh.attack =function(g1,g1.type,result){
  g<-g1
  g.type<-g1.type
  nn<-as.integer(vcount(g) )
  for (i in seq(1:(nn-2))){
    vnum<-vcount(g)
    
    
    V(g)$dd=V(g)$lv
    #rank high when the value of level is low
    V(g)$drank=ave(-V(g)$dd, FUN = rank)
    random.deletes<-which.max(V(g)$drank)[1]
    
    #random.deletes <- sample(1:vnum,1,replace=F)
    g<-delete_vertices(g, random.deletes)
    apl=average.path.length (g, directed=F, unconnected=TRUE)
    cl=clusters(g)
    mcz=max(cl$csize)
    acz<-0
    if (cl$no>1)  {
      acz<-(sum(cl$csize) - mcz)/(cl$no-1)
    }
    result <- rbind(result,
                    data.frame(
                      idx=i,
                      nv=vcount(g),
                      ne=ecount(g),
                      apl=apl,
                      mcz=mcz,
                      acz=acz,
                      sn='kh_Attack',
                      type =g.type
                    )
    )
    
  }
  return(result)
}

kh.attack.r =function(g1,g1.type,result,trial){
  for (j in seq(1:trial))
  {
    
    
    g<-g1
    g.type<-g1.type
    nn<-as.integer(vcount(g) )
    for (i in seq(1:(nn-2))){
      vnum<-vcount(g)
      
      
      V(g)$dd=V(g)$lv
      #rank high when the value of level is low
      V(g)$drank=ave(-V(g)$dd, FUN = rank)
      random.deletes<-which.max(V(g)$drank)[1]
      
      #random.deletes <- sample(1:vnum,1,replace=F)
      g<-delete_vertices(g, random.deletes)
      apl=average.path.length (g, directed=F, unconnected=TRUE)
      cl=clusters(g)
      mcz=max(cl$csize)
      acz<-0
      if (cl$no>1)  {
        acz<-(sum(cl$csize) - mcz)/(cl$no-1)
      }
      result <- rbind(result,
                      data.frame(
                        trial=j,
                        idx=i,
                        nv=vcount(g),
                        ne=ecount(g),
                        apl=apl,
                        mcz=mcz,
                        acz=acz,
                        sn='kh_Attack',
                        type =g.type
                      )
      )
      
    }
  }
  return(result)
}
#lower employees first
kl.attack =function(g1,g1.type,result){
  g<-g1
  g.type<-g1.type
  nn<-as.integer(vcount(g) )
  for (i in seq(1:(nn-2))){
    vnum<-vcount(g)
    
    
    V(g)$dd=V(g)$lv
    #rank high when the value of level is also high
    V(g)$drank=ave(V(g)$dd, FUN = rank)
    random.deletes<-which.max(V(g)$drank)[1]
    
    #random.deletes <- sample(1:vnum,1,replace=F)
    g<-delete_vertices(g, random.deletes)
    apl=average.path.length (g, directed=F, unconnected=TRUE)
    cl=clusters(g)
    mcz=max(cl$csize)
    acz<-0
    if (cl$no>1)  {
      acz<-(sum(cl$csize) - mcz)/(cl$no-1)
    }
    result <- rbind(result,
                    data.frame(
                      idx=i,
                      nv=vcount(g),
                      ne=ecount(g),
                      apl=apl,
                      mcz=mcz,
                      acz=acz,
                      sn='kl_Attack',
                      type =g.type
                    )
    )
    
  }
  return(result)
}

kl.attack.r =function(g1,g1.type,result,trial){
  for(j in seq(1:trial))
  {
    
    
    g<-g1
    g.type<-g1.type
    nn<-as.integer(vcount(g) )
    for (i in seq(1:(nn-2))){
      vnum<-vcount(g)
      
      
      V(g)$dd=V(g)$lv
      #rank high when the value of level is also high
      V(g)$drank=ave(V(g)$dd, FUN = rank)
      random.deletes<-which.max(V(g)$drank)[1]
      
      #random.deletes <- sample(1:vnum,1,replace=F)
      g<-delete_vertices(g, random.deletes)
      apl=average.path.length (g, directed=F, unconnected=TRUE)
      cl=clusters(g)
      mcz=max(cl$csize)
      acz<-0
      if (cl$no>1)  {
        acz<-(sum(cl$csize) - mcz)/(cl$no-1)
      }
      result <- rbind(result,
                      data.frame(
                        trial=j,
                        idx=i,
                        nv=vcount(g),
                        ne=ecount(g),
                        apl=apl,
                        mcz=mcz,
                        acz=acz,
                        sn='kl_Attack',
                        type =g.type
                      )
      )
      
    }
  }
  return(result)
}

##subfunction set 2
fit_power_law = function(graph) {
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot.d=data.frame(probability,degree)
  p<-ggplot(data=plot.d, aes(x=degree, y=probability)) +
    #facet_wrap(~type) +
    geom_point(size=3) +
    xlab("Degree (log)") +
    ylab("Probability (log)") +
    ggtitle("Degree Distribution") +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()
  
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)",
       col = 1, main = "Degree Distribution")
  curve(power.law.fit, col = "red", add = T, n = length(d))
  return(p)
}

