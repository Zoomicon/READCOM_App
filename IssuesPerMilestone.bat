@echo off

if "%2"=="" goto Syntax

set ListMilestones=gh api -X GET /repos/%1/%2/milestones -f state=all -f sort=number -f direction=desc --jq ".[] | .title" 
::^| bash -c ^'sort -rV^' ::IF PLACED AT END OF PREVIOUS LINE WOULD NEED WINDOWS SUBSYSTEM FOR LINUX AND A LINUX DISTRO INSTALLED FROM WINDOWS STORE FOR WSL2 TO SORT SEMANTICALLY BASED ON THE MILESTONE TITLE, BUT CAN JUST SORT BY MILESTONE NUMBER IF THEY WERE CREATED IN SAME TEMPORAL ORDER AS THE SEMANTIC SORT

::sort=due_on (using "number" instead)
::state CAN TAKE VALUES: open/closed/all

::call :ListMilestones %1 %2
call :IssuesPerMilestone %1 %2

pause
exit /b 0

::---------------------------

:Syntax
echo Syntax: %0 OWNER_NAME REPO_NAME
echo Needs gh command (GitHub CLI) to be installed
exit /b 1

::---------------------------

:ListMilestones
%ListMilestones%
exit /b

::---------------------------

:IssuesPerMilestone
setlocal

::-- not supported
::set GH_PAGER="cat"

::-- keep list of milestones
::-- also tried using below "for /F %%i in ('%ListMilestones%')" to avoid using a temporary file, but didn't make it to work
%ListMilestones% > listMilestones.txt

for /F %%i in (listMilestones.txt) do (
  echo Milestone %%i
  gh issue list --milestone "%%i"  --search "sort:updated-desc" --state "all" --repo "%1/%2"
  ::APPEND THIS TO SHOW JUST ISSUE NUMBER AND TITLE:  --jq ".[] | (.number|tostring)+\" \"+.title" --json number,title
  echo.
)

::-- remove temporary file
del listMilestones.txt

exit /b
