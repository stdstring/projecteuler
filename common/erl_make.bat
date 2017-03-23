if not exist ebin (mkdir ebin)
copy ..\nif\bin\* ebin\ /Y
erl -noshell -run make all -run init stop