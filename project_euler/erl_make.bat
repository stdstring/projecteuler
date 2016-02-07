if not exist ebin (mkdir ebin)
erl -noshell -pa ../common/ebin -run make all -run init stop
copy data\*.* ebin /Y