#!/bin/bash

# This script is for base scenario of orgnaizational learning
# Date: 2015.11.08
# Author: Eucman Lee

# I: scenario number
# f: formal network file name
# t: informal network (team network) file name
# s: seed number
# p: Learning probability
# T: simulation period
# b: Network Char (Beta)
# o: output file name
# i: Individual Performance
# g: Group information
# n: Number of groups: This groups are levels within the organization
# r: the Number of trials
# l: the biased Learning probability	<??? what is the biased learning?
# G: the biased Learning group - informal networks of the biased group would be deleted by the biased learning probability.
# W: team group information
# N: the number of team groups
# M: Size of knowledge tuple
# S: Non linear performance parameter
# P: Total number of nodes
# X: The ratio of cutting line of command
# L: The starting period of layoff
# R: The rate of lay off in the group of middle managers
# V: degree of vertical alignment
# C: probability of connection
# D: number of draw
# d: radius
# k: network output files
# a: max span of control
# y: knowledge endowment setting, y% of knowledge will be endowed to the first.
# c: the correct bit group number. the group c will ahve y % of total and c+1 will have the rest. The rest will have zero
# v: the top down intervention policy 1=randome 2=selective intervention

#Specify the directory
#SNLOCATION="/home/eucman/proj/hrcy/04_orglearning/04_scenarios/004_SN_RealHierarchy" 
SNLOCATION=$(pwd)
echo $SNLOCATION
RESULT="$SNLOCATION/../02_result"
DATAFOLDER="/home/eucman/proj/hrcy/04_orglearning/03_data/01_RealHierarchy"
CODE="/home/eucman/proj/hrcy/04_orglearning/01_code"

cd $RESULT
#rm Result*
rm groupInfoflowResult*
rm outnetwork*

cd $SNLOCATION
rm nohup.out



nohup $CODE/olh -I 1 -f $DATAFOLDER/formalnetwork.txt -t $DATAFOLDER/teamnetwork.txt -s 1200 -p 0.2 -T 500 -b 0 -o $RESULT/Result1.csv -i $RESULT/groupInfoflowResult0.txt -g $DATAFOLDER/individuallevel.txt -n 7 -r 100 -l 0.0 -G 1 -W $DATAFOLDER/teamgroup.txt -N 3 -M 100 -S 4 -P 4420 -X 0.0 -L 1 -R 0.000 -V 0.0 -v 2 -C 1.00 -D 0 -d 2 -k $RESULT/outnetwork0.txt -a 20 & 

nohup $CODE/olh -I 2 -f $DATAFOLDER/formalnetwork.txt -t $DATAFOLDER/teamnetwork.txt -s 1200 -p 0.2 -T 500 -b 0 -o $RESULT/Result2.csv -i $RESULT/groupInfoflowResult0.txt -g $DATAFOLDER/individuallevel.txt -n 7 -r 100 -l 0.0 -G 1 -W $DATAFOLDER/teamgroup.txt -N 3 -M 100 -S 4 -P 4420 -X 0.0 -L 1 -R 0.000 -V 0.1 -v 2 -C 1.00 -D 0 -d 2 -k $RESULT/outnetwork0.txt -a 20 & 

nohup $CODE/olh -I 3 -f $DATAFOLDER/formalnetwork.txt -t $DATAFOLDER/teamnetwork.txt -s 1200 -p 0.2 -T 500 -b 0 -o $RESULT/Result3.csv -i $RESULT/groupInfoflowResult0.txt -g $DATAFOLDER/individuallevel.txt -n 7 -r 100 -l 0.0 -G 1 -W $DATAFOLDER/teamgroup.txt -N 3 -M 100 -S 1 -P 4420 -X 0.0 -L 1 -R 0.000 -V 0.0 -v 2 -C 1.00 -D 0 -d 2 -k $RESULT/outnetwork0.txt -a 20 & 

nohup $CODE/olh -I 4 -f $DATAFOLDER/formalnetwork.txt -t $DATAFOLDER/teamnetwork.txt -s 1200 -p 0.2 -T 500 -b 0 -o $RESULT/Result4.csv -i $RESULT/groupInfoflowResult0.txt -g $DATAFOLDER/individuallevel.txt -n 7 -r 100 -l 0.0 -G 1 -W $DATAFOLDER/teamgroup.txt -N 3 -M 100 -S 1 -P 4420 -X 0.0 -L 1 -R 0.000 -V 0.1 -v 2 -C 1.00 -D 0 -d 2 -k $RESULT/outnetwork0.txt -a 20 & 

nohup $CODE/olh -I 5 -f $DATAFOLDER/formalnetwork.txt -t $DATAFOLDER/teamnetwork.txt -s 1200 -p 0.2 -T 500 -b 0 -o $RESULT/Result5.csv -i $RESULT/groupInfoflowResult5.txt -g $DATAFOLDER/individuallevel.txt -n 7 -r 100 -l 0.0 -G 1 -W $DATAFOLDER/teamgroup.txt -N 3 -M 100 -S 8 -P 4420 -X 0.0 -L 1 -R 0.000 -V 0.0 -v 2 -C 1.00 -D 0 -d 2 -k $RESULT/outnetwork0.txt -a 20 & 

nohup $CODE/olh -I 6 -f $DATAFOLDER/formalnetwork.txt -t $DATAFOLDER/teamnetwork.txt -s 1200 -p 0.2 -T 500 -b 0 -o $RESULT/Result6.csv -i $RESULT/groupInfoflowResult5.txt -g $DATAFOLDER/individuallevel.txt -n 7 -r 100 -l 0.0 -G 1 -W $DATAFOLDER/teamgroup.txt -N 3 -M 100 -S 8 -P 4420 -X 0.0 -L 1 -R 0.000 -V 0.1 -v 2 -C 1.00 -D 0 -d 2 -k $RESULT/outnetwork0.txt -a 20 & 
