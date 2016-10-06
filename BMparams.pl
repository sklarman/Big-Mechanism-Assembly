sparql_setup('52.26.26.74', 80, '/sparql-auth', authorization(digest(dba, bigos))).
%sparql_setup('127.0.0.1', 80, '/sparql-auth', authorization(digest(dba, bigos))).
%sparql_setup('127.0.0.1', 8890, '/sparql-auth', authorization(digest(dba, dba))).

%commandProblog('cmd /c "C:/Users/user/Dysk Google/problog2.1/problog-cli.py"').
%commandProblog('"C:/Users/user/AppData/Local/Programs/Python/Python35-32/python.exe" "C:/Users/user/Dysk Google/problog2.1/problog-cli.py"').
%commandProblog('cmd /c "C:/Users/Szymon/Google Drive/problog2.1/problog-cli.py"').
commandProblog('"C:/Program Files (x86)/Python 3.5/python.exe" "C:/Users/csstssk/Google Drive/problog2.1/problog-cli.py"').
%commandProblog('cmd /c "C:/Users/Administrator/Desktop/Assembly/problog/problog-cli.py"').

:- prolog_load_context(directory, Dir),  asserta(user:file_search_path(my_home, Dir)).

:- asserta(user:file_search_path(index_data, 'C:/Users/user/Dysk Google/PrologBM')).
%:- asserta(user:file_search_path(index_data, 'C:/Users/csstssk/Google Drive/PrologBM')).
%:- asserta(user:file_search_path(index_data, 'C:/Users/Administrator/Desktop/Assembly')).

%indexFolder('/isi-cards-6-13-2016/PMC534114/*.*', 'isi-cards-6-13-2016').
indexFolder('/IsiCombined/*.*', 'IsiCombined-2016-8-10').
