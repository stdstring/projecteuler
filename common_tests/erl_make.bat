if not exist ebin (mkdir ebin)
rem erl -noshell -run make all -run init stop
erl -noshell -eval "case make:all() of up_to_date -> init:stop(0); error -> init:stop(1) end"
if %ERRORLEVEL% NEQ 0 exit /B 1
erl -noshell -pa ebin -pa ../common/ebin -eval "case eunit:test([{dir, \"ebin\"}], [verbose]) of ok -> init:stop(); _ -> init:stop(1) end"
if %ERRORLEVEL% NEQ 0 exit /B 1