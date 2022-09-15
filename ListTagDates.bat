@echo off

git log --tags --simplify-by-decoration --pretty="format:%%ci %%d" > TagDates.txt
::using %% to escape the % character (^% won't work there)

type TagDates.txt

echo.
