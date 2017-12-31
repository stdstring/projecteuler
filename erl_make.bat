echo BUILD NIF
cd nif
call make.bat
if %ERRORLEVEL% NEQ 0 exit /B 1
cd ..
echo BUILD COMMON
cd common
call erl_make.bat
if %ERRORLEVEL% NEQ 0 exit /B 1
cd ..
echo BUILD AND RUN COMMON TESTS
cd common_tests
call erl_make.bat
if %ERRORLEVEL% NEQ 0 exit /B 1
cd ..
echo NUMERICAL TASK
cd numerical_task
call erl_make.bat
if %ERRORLEVEL% NEQ 0 exit /B 1
cd ..
echo PROJECT EULER
cd project_euler
call erl_make.bat
if %ERRORLEVEL% NEQ 0 exit /B 1
cd ..
echo COMPLETE