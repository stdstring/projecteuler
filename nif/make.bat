rmdir make /S /Q
mkdir make
cd make
REM TODO (std_string) : think about this solution
IF "%PROCESSOR_ARCHITECTURE%"=="x86" ( cmake -G "Visual Studio 14 2015" .. ) ELSE ( cmake -G "Visual Studio 14 2015 Win64" .. )
cmake --build . --config Debug
cd ..
rmdir bin /S /Q
mkdir bin
copy make\bin\MutableArrayNif\Debug\*.dll bin\