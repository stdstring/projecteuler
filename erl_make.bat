echo BUILD NIF
cd nif
call make.bat
if %ERRORLEVEL% NEQ 0 (
    echo NIF BUILD FAILED
    exit /B 1
)
cd ..
echo BUILD COMMON
cd common
call erl_make.bat
if %ERRORLEVEL% NEQ 0 (
    echo COMMON BUILD FAILED
    exit /B 1
)
cd ..
echo BUILD AND RUN COMMON TESTS
cd common_tests
call erl_make.bat
if %ERRORLEVEL% NEQ 0 (
    echo COMMON TESTS BUILD AND RUN FAILED
    exit /B 1
)
cd ..
echo BUILD NUMERICAL TASK
cd numerical_task
call erl_make.bat
if %ERRORLEVEL% NEQ 0 (
    echo NUMERICAL TASK BUILD FAILED
    exit /B 1
)
cd ..
echo BUILD PROJECT EULER
cd project_euler
call erl_make.bat
if %ERRORLEVEL% NEQ 0 (
    echo BUILD PROJECT EULER FAILED
    exit /B 1
)
cd ..
echo COMPLETE