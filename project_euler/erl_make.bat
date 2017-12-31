if not exist ebin (mkdir ebin)
erl -noshell -pa ../common/ebin -eval "case make:all() of up_to_date -> init:stop(); error -> init:stop(1) end"
if %ERRORLEVEL% NEQ 0 exit /B 1
copy data\*.* ebin /Y