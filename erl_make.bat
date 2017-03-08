echo BUILD NIF
cd nif
rmdir bin /S /Q
mkdir bin
cd bin
REM TODO (std_string) : think about this solution
IF "%PROCESSOR_ARCHITECTURE%"=="x86" ( cmake -G "Visual Studio 14 2015" .. ) ELSE ( cmake -G "Visual Studio 14 2015 Win64" .. )
cmake --build . --config Debug
cd ..\..
echo BUILD COMMON
cd common
call erl_make.bat
cd ..
echo BUILD AND RUN COMMON TESTS
cd common_tests
call erl_make.bat
cd ..
echo NUMERICAL TASK
cd numerical_task
call erl_make.bat
cd ..
echo PROJECT EULER
cd project_euler
call erl_make.bat
cd ..
echo COMPLETE