if not exist ebin (mkdir ebin)
copy ..\nif\bin\* ebin\ /Y
erl -noshell -eval "case make:all() of up_to_date -> init:stop(); error -> init:stop(1) end"
if %ERRORLEVEL% NEQ 0 exit /B 1