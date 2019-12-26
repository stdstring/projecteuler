rmdir make /S /Q
mkdir make
cd make
REM TODO (std_string) : think about this solution
IF "%PROCESSOR_ARCHITECTURE%"=="x86" ( cmake -G "Visual Studio 15 2017" .. ) ELSE ( cmake -G "Visual Studio 15 2017 Win64" .. )
cmake --build . --config Debug
cd ..
rmdir bin /S /Q
mkdir bin
copy make\bin\MutableUInt8ArrayNif\Debug\*.dll bin\ /Y