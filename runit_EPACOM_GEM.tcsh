#!/bin/tcsh -f
# file: runit_EPACOM_GEM.tcsh
#clear

echo " "
echo "Begin runit_EPACOM_GEM.tcsh, the run script for "
echo "EPACOM_GEM.exe"
echo " "
echo "If the log file logfile exists, delete it"
echo " "

if(-e logfile) then
   echo "logfile exists, so delete it"
   rm logfile 
   echo "logfile has been deleted"
else
   echo "logfile does not exist, " 
   echo "so it doesn't have to be deleted" 
endif
echo " "
echo "Run the executable EPACOM_GEM.exe"
echo "and write messages on stdout and stderr to the logfile file"

./EPACOM_GEM.exe >& logfile

set result = $status

echo " "
echo "If the exit code of EPACOM_GEM.exe is 2, there"
echo "was a normal successful exit from the program. If the"
echo "exit code is other than 2, the program did not run"
echo "successfully to completion. Now print the exit status variable."
echo "and test it to see if it is 2 or /= 2"
echo " "
echo "Exit status variable value = $result"

echo " "
if($result == 2) then
   echo "The executable"
   echo "EPACOM_GEM.exe successfully executed to completion"
echo " "   
else
   echo "The executable"
   echo "EPACOM_GEM.exe did NOT successfully execute to completion"
echo " "   
endif

echo " "
echo "Code Results are in logfile"
echo " "
echo " Finished executing runit_EPACOM_GEM.tcsh script "
echo " "
