%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2017 9:24 AM
%%%-------------------------------------------------------------------
-module(q_fs).
-author("madalin").

-include_lib("q/include/q.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([

  is_dir/1,
  is_regular_file/1,

  dir_get_files_list/1, dir_get_files_list/2, dir_get_files_list/4,
  dir_create/1,
  dir_create_recursive/1,
  dir_delete/1,
  dir_delete_recursive/1

]).

-export([file_delete/1]).
-export([file_match_extension/2]).
-export([file_match_type/2]).

-export([files_list_delete_recursive/2]).

-export([safe_file_write/2]).
-export([safe_file_write_consult/2]).
-export([safe_file_write_serialize/2]).
-export([safe_file_read/1]).
-export([safe_file_read_consult/1]).
-export([safe_file_read_unserialize/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_dir(File) ->
  case file:read_file_info(File) of
    {ok, #file_info{type=directory}} -> true;
    _ -> false
  end.

is_regular_file(File) ->
  case file:read_file_info(File) of
    {ok, #file_info{type=regular}} -> true;
    _ -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dir
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates the specified directory
-spec dir_create(Directory) -> ok | {error, Reason} when
  Directory :: name_all(),
  Reason :: posix() | badarg.
dir_create(Directory) -> file:make_dir(Directory).

%% @doc Creates the specified directory recursive
-spec dir_create(Directory) -> ok | {error, Reason} when
  Directory :: name_all(),
  Reason :: posix() | badarg.
dir_create_recursive("/") -> ok;
dir_create_recursive(<<"/">>) -> ok;
dir_create_recursive(Directory) ->

  FatherDirectory = filename:dirname(Directory),
  case is_dir(FatherDirectory) of

    true -> dir_create(Directory);

    false when FatherDirectory =:= Directory ->

      %% Protect against infinite loop
      {error, einval};

    false ->

      case dir_create_recursive(FatherDirectory) of
        ok -> dir_create(Directory);
        Error -> Error
      end

  end.

%% @doc Delete the specified directory
dir_delete(Directory) -> file:del_dir(Directory).

%% @doc Delete the specified directory and all files recursive
dir_delete_recursive(Directory) ->

    case file:list_dir(Directory) of
        {ok, []} -> dir_delete(Directory);
        {ok, Filenames} ->
            %% Delete all files in this directory
            case files_list_delete_recursive(Filenames, Directory) of
                ok -> dir_delete(Directory);
                Error -> Error
            end;
        Error -> Error
    end.

%% @spec dir_get_files_list(Directory) -> {ok, Filenames} | {error, Reason}
%% @doc Returns list of all FILES from the specified directory.
dir_get_files_list(Directory) -> file:list_dir(Directory).

%% @spec dir_get_files_list(Directory,Type) -> {ok, Filenames} | {error, Reason}
%% @doc Returns list of FILES from the specified directory having the aspecified types.
dir_get_files_list(Directory, Type) ->

    case file:list_dir(Directory) of
        {ok, Filenames} -> {ok, dir_files_check_type(Directory,Filenames,Type)};
        Error -> Error
    end.


%% @spec dir_get_files_list(Directory,Type,Extension,CaseSensitive) -> {ok, Filenames} | {error, Reason}
%% @doc Returns list of files from the specified directory having the specified type and extension.
dir_get_files_list(Directory, Type, Extension, CaseSensitive) ->

    case file:list_dir(Directory) of
        {ok, Filenames} -> {ok, dir_files_check_type_and_ext(Directory,Filenames,Type,Extension,CaseSensitive)};
        Error -> Error
    end.

%% @spec dir_files_check_type(Directory,Filenames,Type) -> [filename1, filename2...] | []
%% @doc Iterate files and sort those having the specified type.
dir_files_check_type(Directory,Filenames,Type) -> dir_files_check_type(Directory,Filenames,Type,[]).
dir_files_check_type(Directory,[H|T],Type,Accumulator) ->
    FullFileName = filename:join([Directory, H]),
    case file_match_type(FullFileName,Type) of
        true ->
          NewAccumulator = lists:append([Accumulator,[H]]),
            dir_files_check_type(Directory,T,Type,NewAccumulator);
        false ->
            dir_files_check_type(Directory,T,Type,Accumulator)
    end;
dir_files_check_type(_Directory,[],_Type,Accumulator) -> Accumulator.

%% @spec dir_files_check_type_and_ext(Directory,Filenames,Type,Extension,CaseSensitive)
%% @doc Iterate files and sort those having the specified type and extension.
dir_files_check_type_and_ext(Directory,Filenames,Type,Extension,CaseSensitive) -> dir_files_check_type_and_ext(Directory,Filenames,Type,Extension,CaseSensitive,[]).
dir_files_check_type_and_ext(Directory,[H|T],Type,Extension,CaseSensitive,Accumulator) ->

    FullFileName = filename:join([Directory, H]),

    %% First we match type because directories for example can also have name containing extension in them
    MatchType = file_match_type(FullFileName,Type),
    case MatchType of
        true ->
            MatchExt = file_match_extension(H,Extension,CaseSensitive),
            case MatchExt of
                true ->
                    NewAccumulator = lists:append([Accumulator,[H]]),
                    dir_files_check_type_and_ext(Directory,T,Type,Extension,CaseSensitive,NewAccumulator);
                false -> dir_files_check_type_and_ext(Directory,T,Type,Extension,CaseSensitive,Accumulator)
            end;
        _ ->
            dir_files_check_type_and_ext(Directory,T,Type,Extension,CaseSensitive,Accumulator)
    end;
dir_files_check_type_and_ext(_Directory, [], _Type, _Extension, _CaseSensitive, Accumulator) -> Accumulator.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Delete the specified file
file_delete(File) -> file:delete(File).

%% @doc Check the specified file type to see if match the specified one.
%% Returns true if file is one of the specified type, false if not or {error, Reason} if file cann not be accessed or doesn't exist.
%% WARNING: remember FileName must be full filename so read_file_info know what file must check (not only filename without path).
%% Example:
%%
%% file_match_type("/full/path/to/filename/file", device).
%% file_match_type("/full/path/to/filename/file", directory).
%% file_match_type("/full/path/to/filename/file", other).
%% file_match_type("/full/path/to/filename/file", regular).
%% file_match_type("/full/path/to/filename/file", symlink).
%%
%% Or any combination of types:
%% file_match_type("/full/path/to/filename/file",[device, directory, other, regular, symlink]).
file_match_type(FileName,Type) when is_atom(Type) -> file_match_type(FileName,[Type]);
file_match_type(FileName,Type) when is_list(Type) ->

    Result = file:read_file_info(FileName),
    case Result of

        {ok, FileInfo} ->

            FileType = FileInfo#file_info.type,
            lists:member(FileType, Type);

        {error, Reason} -> {error, Reason}

    end.

%% @doc Check the specified file extension to see if match the specified one. Check is case sensitive. lang doesn't match LANG.
file_match_extension(FileName, Extension) -> file_match_extension(FileName, Extension, true).

%% @doc Check the specified file extension to see if match the specified one.
%% Use true for case sensitive check and false otherwise.
file_match_extension(FileName, Extension, Any) when erlang:is_binary(FileName) -> file_match_extension(binary_to_list(FileName),Extension,Any);
file_match_extension(FileName, Extension, Any) when erlang:is_binary(Extension) -> file_match_extension(FileName,binary_to_list(Extension),Any);
file_match_extension(FileName, Extension, Any) when erlang:is_list(FileName), erlang:is_list(Extension) -> file_match_extension_case(FileName,Extension,Any).
file_match_extension_case(FileName,Extension,true) ->
    FileExtension = filename:extension(FileName),
    case FileExtension of
        Extension -> true;
        _ -> false
    end;
file_match_extension_case(FileName,Extension,false) ->
    FileExtension = string:to_lower(filename:extension(FileName)),
    LowerExtension= string:to_lower(Extension),
    case FileExtension of
        LowerExtension -> true;
        _ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% files_list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Delete the specified files from the specified directory recursive
%% Files list can contain both files and directories names. All file names must not contain path and must
%% be relative to the specified directory. For deleting a mixed list of files from different directories
%% use q_fs:files_list_delete_recursive/1
files_list_delete_recursive([H|T], Directory) ->

    FullFileName = filename:join([Directory, H]),
    case filelib:is_dir(FullFileName) of
        true ->
            case dir_delete_recursive(Directory) of
                ok -> files_list_delete_recursive(T, Directory);
                Error -> Error
            end;
        false ->
            case file:delete(FullFileName) of
                ok -> files_list_delete_recursive(T, Directory);
                Error -> Error
            end
    end;
files_list_delete_recursive([], _Directory) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% safe_file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Writes the contents of the iodata term Bytes to file Filename
%% taking all measure necessary to be able to restore the original file in case of unsuccessful write.
%%
%% The file is created if it does not exist.
%% If it exists, creates a backup of the original file, overwrite the previous contents and delete the backup file.
%% If the file write is interrupted any attempt to read the file again will restore the original file.
%%
%% Returns ok if successful, otherwise q_error().

-spec safe_file_write(Filename, Bytes) -> ok | Error when
    Filename :: name_all(),
    Bytes :: iodata(),
    Error :: q_error().

safe_file_write(Filename, Bytes) -> gen_server:call(q_fs_srv_safe_file, {write, Filename, Bytes}, infinity).

%% @doc Writes the contents of the List to file filename using the format understood by file:consult/1 function.
%%
%% The file is created if it does not exist.
%% If it exists, creates a backup of the original file, overwrite the previous contents and delete the backup file.
%% If the file write is interrupted any attempt to read the file again will restore the original file.
%%
%% Please notice that this doesn't offer any protection from concurrently access to the file using os file functions.
%% Those functions are designed only to make sure Q doesn't access a file concurrently and to offer a recovery mechanism
%% in case write operation fail.
%%
%% Returns ok if successful, otherwise q_error().

-spec safe_file_write_consult(Filename, TermsList) -> ok | Error when
    Filename :: name_all(),
    TermsList :: [term()],
    Error :: q_error().

safe_file_write_consult(Filename, TermsList) ->

    safe_file_write(Filename, lists:map(fun(Term) -> io_lib:format("~tp.~n", [Term]) end, TermsList)).

%% @doc Serialize and writes the Term to the file Filename.
%%
%% The file is created if it does not exist.
%% If it exists, creates a backup of the original file, overwrite the previous contents and delete the backup file.
%% If the file write is interrupted any attempt to read the file again will restore the original file.
%%
%% Please notice that this doesn't offer any protection from concurrently access to the file using os file functions.
%% Those functions are designed only to make sure Q doesn't access a file concurrently and to offer a recovery mechanism
%% in case write operation fail.
%%
%% Returns ok if successful, otherwise q_error().

-spec safe_file_write_serialize(Filename, Term) -> ok | Error when
    Filename :: name_all(),
    Term :: term(),
    Error :: q_error().

safe_file_write_serialize(Filename, Term) ->

    SerializedTerm = erlang:term_to_binary(Term),
    safe_file_write(Filename, SerializedTerm).

%% @doc Writes the contents of the iodata term Bytes to file Filename
%% taking all measure necessary to be able to restore the original file in case of unsuccessful write.
%%
%% The file is created if it does not exist.
%% If it exists, creates a backup of the original file, overwrite the previous contents and delete the backup file.
%% If the file write is interrupted any attempt to read the file again will restore the original file.
%%
%% Returns ok if successful, otherwise q_error().

-spec safe_file_read(Filename) -> {ok, Binary} | Error when
    Filename :: name_all(),
    Bytes :: iodata(),
    Error :: q_error().

safe_file_read(Filename) -> gen_server:call(q_fs_srv_safe_file, {read, Filename}, infinity).

%% @doc Reads Erlang terms, separated by '.', from Filename.
%%
%% The file must be previously written by q_fs:safe_file_write_terms/2 function.
%% Returns {ok, Terms} if the file was successfully read, otherwise q_error().

-spec safe_file_read_consult(Filename) -> {ok, Terms} | Error when
    Filename :: name_all(),
    Terms :: [term()],
    Error :: q_error().

safe_file_read_consult(Filename) -> gen_server:call(q_fs_srv_safe_file, {read_consult, Filename}, infinity).

%% @doc Reads and unserialize an Erlang term from Filename.
%%
%% The file must be previously written by q_fs:safe_file_write_serialize/2 function.
%% Returns {ok, Terms} if the file was successfully read, otherwise q_error().

-spec safe_file_read_unserialize(Filename) -> {ok, Term} | Error when
    Filename :: name_all(),
    Term :: term(),
    Error :: q_error().

safe_file_read_unserialize(Filename) ->

    case safe_file_read(Filename) of
        {ok, Binary} ->
            try
                erlang:binary_to_term(Binary)
            catch
                _:_ -> ?Q_ERROR(safe_file_read_unserialize_failure)
            end;
        Error -> Error
    end.