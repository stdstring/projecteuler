echo BUILD NIF
cd nif
call make.bat
cd ..
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