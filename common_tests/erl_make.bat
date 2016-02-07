if not exist ebin (mkdir ebin)
erl -noshell -run make all -run init stop
erl -noshell -pa ebin -pa ../common/ebin -eval "eunit:test([{dir, \"ebin\"}], [verbose])" -run init stop