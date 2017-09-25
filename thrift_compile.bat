@echo off

set THRIFT_DIR=src_thrift

@echo on
@echo -------------------------------------------------------------------------
@echo 1. generate files
@echo -------------------------------------------------------------------------
@echo off

thrift -o   %THRIFT_DIR% -strict -r -v --gen erl %THRIFT_DIR%\erlang_python.thrift
thrift -out src_py       -strict -r -v --gen py  %THRIFT_DIR%\erlang_python.thrift

@echo on
@echo -------------------------------------------------------------------------
@echo 2. list content of generated file directories
@echo -------------------------------------------------------------------------
@echo off

dir %THRIFT_DIR%\gen-erl

@echo on
@echo -------------------------------------------------------------------------
@echo 3. move the generated files
@echo -------------------------------------------------------------------------
@echo off

move /Y %THRIFT_DIR%\gen-erl\*.erl src
move /Y %THRIFT_DIR%\gen-erl\*.hrl include

@echo on
@echo -------------------------------------------------------------------------
@echo 4. delete generated file directories
@echo -------------------------------------------------------------------------
@echo off

rmdir %THRIFT_DIR%\gen-erl
dir %THRIFT_DIR%\*.*
