import os
import sys
import fileinput

solarFileR = open("mkg2006.05t.txt",'r')
solarFileW = open("PAR.dat",'w')

for line in solarFileR:
	if "AirTemp" in line:
		print('AirTemp')
	elif "ID" in line:
		print('ID')
        elif "2005 365" in line:
		print('2005')
	else:
		solarFileW.write(line)
solarFileR.close()
solarFileW.close()
