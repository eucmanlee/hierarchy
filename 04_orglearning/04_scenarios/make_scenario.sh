#!/bin/bash 

echo "Enter new scenario name:"
read name
mkdir $name
cd $name
mkdir 00_shell 01_data 02_result 03_report 
touch scenario_description.txt


