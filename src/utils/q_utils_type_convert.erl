%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2017 10:51 AM
%%%-------------------------------------------------------------------
-module(q_utils_type_convert).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([term_to_binary/1]).
%%-export([term_to_list/1]).
%%-export([term_to_atom/1]).
%%-export([term_to_integer/1]).
%%-export([term_to_float/1]).

-export([list_atom_to_binary/1]).
-export([list_atom_to_list/1]).
-export([list_atom_to_atom/1]).
-export([list_atom_to_integer/1]).
-export([list_atom_to_float/1]).

-export([binary_atom_to_binary/1]).
-export([binary_atom_to_list/1]).
-export([binary_atom_to_atom/1]).
-export([binary_atom_to_integer/1]).
-export([binary_atom_to_float/1]).

-export([binary_list_to_binary/1]).
-export([binary_list_to_list/1]).
-export([binary_list_to_atom/1]).
-export([binary_list_to_integer/1]).
-export([binary_list_to_float/1]).

-export([binary_list_atom_to_binary/1]).
-export([binary_list_atom_to_list/1]).
-export([binary_list_atom_to_atom/1]).
-export([binary_list_atom_to_integer/1]).
-export([binary_list_atom_to_float/1]).

-export([binary_list_atom_integer_to_binary/1]).
-export([binary_list_atom_integer_to_list/1]).
-export([binary_list_atom_integer_to_atom/1]).
-export([binary_list_atom_integer_to_integer/1]).
-export([binary_list_atom_integer_to_float/1]).

-export([name_to_atom/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% term_to_...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert the specified binary, iolist, atom, integer or float to binary.
-spec term_to_binary(Term) -> binary() when
    Term :: iolist() | integer() | atom() | float() | binary().

term_to_binary(Term) when erlang:is_binary(Term) -> Term;
term_to_binary(Term) when erlang:is_list(Term) -> erlang:iolist_to_binary(Term);
term_to_binary(Term) when erlang:is_atom(Term) -> erlang:atom_to_binary(Term, utf8);
term_to_binary(Term) when erlang:is_integer(Term) -> erlang:integer_to_binary(Term);
term_to_binary(Term) when erlang:is_float(Term) -> erlang:float_to_binary(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list_atom_to_...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert the specified list or atom to binary
list_atom_to_binary(Term) when erlang:is_list(Term) -> erlang:list_to_binary(Term);
list_atom_to_binary(Term) when erlang:is_atom(Term) -> erlang:atom_to_binary(Term, utf8).

%% @doc Convert the specified list or atom to list
list_atom_to_list(Term) when erlang:is_list(Term) -> Term;
list_atom_to_list(Term) when erlang:is_atom(Term) -> erlang:atom_to_list(Term).

%% @doc Convert the specified list or atom to atom
list_atom_to_atom(Term) when erlang:is_list(Term) -> erlang:list_to_atom(Term);
list_atom_to_atom(Term) when erlang:is_atom(Term) -> Term.

%% @doc Convert the specified list or atom to integer
list_atom_to_integer(Term) when erlang:is_list(Term) -> erlang:list_to_integer(Term);
list_atom_to_integer(Term) when erlang:is_atom(Term) -> erlang:list_to_integer(erlang:atom_to_list(Term)).

%% @doc Convert the specified list or atom to float
list_atom_to_float(Term) when erlang:is_list(Term) -> erlang:list_to_float(Term);
list_atom_to_float(Term) when erlang:is_atom(Term) -> erlang:list_to_float(erlang:atom_to_list(Term)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% binary_atom_to_...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert the specified list or atom to binary
binary_atom_to_binary(Term) when erlang:is_list(Term) -> erlang:list_to_binary(Term);
binary_atom_to_binary(Term) when erlang:is_atom(Term) -> erlang:atom_to_binary(Term, utf8).

%% @doc Convert the specified list or atom to list
binary_atom_to_list(Term) when erlang:is_list(Term) -> Term;
binary_atom_to_list(Term) when erlang:is_atom(Term) -> erlang:atom_to_list(Term).

%% @doc Convert the specified list or atom to atom
binary_atom_to_atom(Term) when erlang:is_list(Term) -> erlang:list_to_atom(Term);
binary_atom_to_atom(Term) when erlang:is_atom(Term) -> Term.

%% @doc Convert the specified list or atom to integer
binary_atom_to_integer(Term) when erlang:is_list(Term) -> erlang:list_to_integer(Term);
binary_atom_to_integer(Term) when erlang:is_atom(Term) -> erlang:list_to_integer(erlang:atom_to_list(Term)).

%% @doc Convert the specified list or atom to float
binary_atom_to_float(Term) when erlang:is_list(Term) -> erlang:list_to_float(Term);
binary_atom_to_float(Term) when erlang:is_atom(Term) -> erlang:list_to_float(erlang:atom_to_list(Term)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% binary_list_to_...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert the specified list or atom to binary
binary_list_to_binary(Term) when erlang:is_list(Term) -> erlang:list_to_binary(Term);
binary_list_to_binary(Term) when erlang:is_atom(Term) -> erlang:atom_to_binary(Term, utf8).

%% @doc Convert the specified list or atom to list
binary_list_to_list(Term) when erlang:is_list(Term) -> Term;
binary_list_to_list(Term) when erlang:is_atom(Term) -> erlang:atom_to_list(Term).

%% @doc Convert the specified list or atom to atom
binary_list_to_atom(Term) when erlang:is_list(Term) -> erlang:list_to_atom(Term);
binary_list_to_atom(Term) when erlang:is_atom(Term) -> Term.

%% @doc Convert the specified list or atom to integer
binary_list_to_integer(Term) when erlang:is_list(Term) -> erlang:list_to_integer(Term);
binary_list_to_integer(Term) when erlang:is_atom(Term) -> erlang:list_to_integer(erlang:atom_to_list(Term)).

%% @doc Convert the specified list or atom to float
binary_list_to_float(Term) when erlang:is_list(Term) -> erlang:list_to_float(Term);
binary_list_to_float(Term) when erlang:is_atom(Term) -> erlang:list_to_float(erlang:atom_to_list(Term)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% binary_list_atom_to...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert the specified binary, list or atom to binary
binary_list_atom_to_binary(Term) when erlang:is_binary(Term) -> Term;
binary_list_atom_to_binary(Term) when erlang:is_list(Term) -> erlang:list_to_binary(Term);
binary_list_atom_to_binary(Term) when erlang:is_atom(Term) -> erlang:atom_to_binary(Term, utf8).

%% @doc Convert the specified binary, list or atom to list
binary_list_atom_to_list(Term) when erlang:is_binary(Term) -> erlang:binary_to_list(Term);
binary_list_atom_to_list(Term) when erlang:is_list(Term) -> Term;
binary_list_atom_to_list(Term) when erlang:is_atom(Term) -> erlang:atom_to_list(Term).

%% @doc Convert the specified binary, list or atom to atom
binary_list_atom_to_atom(Term) when erlang:is_binary(Term) -> erlang:binary_to_atom(Term, utf8);
binary_list_atom_to_atom(Term) when erlang:is_list(Term) -> erlang:list_to_atom(Term);
binary_list_atom_to_atom(Term) when erlang:is_atom(Term) -> Term.

%% @doc Convert the specified binary, list or atom to integer
binary_list_atom_to_integer(Term) when erlang:is_binary(Term) -> erlang:binary_to_integer(Term);
binary_list_atom_to_integer(Term) when erlang:is_list(Term) -> erlang:list_to_integer(Term);
binary_list_atom_to_integer(Term) when erlang:is_atom(Term) -> erlang:list_to_integer(erlang:atom_to_list(Term)).

%% @doc Convert the specified binary, list or atom to float
binary_list_atom_to_float(Term) when erlang:is_binary(Term) -> erlang:binary_to_float(Term);
binary_list_atom_to_float(Term) when erlang:is_list(Term) -> erlang:list_to_float(Term);
binary_list_atom_to_float(Term) when erlang:is_atom(Term) -> erlang:list_to_float(erlang:atom_to_list(Term)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% binary_list_atom_integer_to_...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert the specified binary, list, atom or integer to binary
binary_list_atom_integer_to_binary(Term) when erlang:is_binary(Term) -> Term;
binary_list_atom_integer_to_binary(Term) when erlang:is_list(Term) -> erlang:list_to_binary(Term);
binary_list_atom_integer_to_binary(Term) when erlang:is_atom(Term) -> erlang:atom_to_binary(Term, utf8);
binary_list_atom_integer_to_binary(Term) when erlang:is_integer(Term) -> erlang:integer_to_binary(Term).

%% @doc Convert the specified binary, list, atom or integer to list
binary_list_atom_integer_to_list(Term) when erlang:is_binary(Term) -> erlang:binary_to_list(Term);
binary_list_atom_integer_to_list(Term) when erlang:is_list(Term) -> Term;
binary_list_atom_integer_to_list(Term) when erlang:is_atom(Term) -> erlang:atom_to_list(Term);
binary_list_atom_integer_to_list(Term) when erlang:is_integer(Term) -> erlang:integer_to_list(Term).

%% @doc Convert the specified binary, list, atom or integer to atom
binary_list_atom_integer_to_atom(Term) when erlang:is_binary(Term) -> erlang:binary_to_atom(Term, utf8);
binary_list_atom_integer_to_atom(Term) when erlang:is_list(Term) -> erlang:list_to_atom(Term);
binary_list_atom_integer_to_atom(Term) when erlang:is_atom(Term) -> Term;
binary_list_atom_integer_to_atom(Term) when erlang:is_integer(Term) -> erlang:list_to_atom(erlang:integer_to_list(Term)).

%% @doc Convert the specified binary, list, atom or integer to integer
binary_list_atom_integer_to_integer(Term) when erlang:is_binary(Term) -> erlang:binary_to_integer(Term);
binary_list_atom_integer_to_integer(Term) when erlang:is_list(Term) -> erlang:list_to_integer(Term);
binary_list_atom_integer_to_integer(Term) when erlang:is_atom(Term) -> erlang:list_to_integer(erlang:atom_to_list(Term));
binary_list_atom_integer_to_integer(Term) when erlang:is_integer(Term) -> Term.

%% @doc Convert the specified binary, list, atom or integer to float
binary_list_atom_integer_to_float(Term) when erlang:is_binary(Term) -> erlang:binary_to_float(Term);
binary_list_atom_integer_to_float(Term) when erlang:is_list(Term) -> erlang:list_to_float(Term);
binary_list_atom_integer_to_float(Term) when erlang:is_atom(Term) -> erlang:list_to_float(erlang:atom_to_list(Term));
binary_list_atom_integer_to_float(Term) when erlang:is_integer(Term) -> erlang:float(Term).

%% @doc Convert the specified binary, list or atom to atom
name_to_atom(Term) when erlang:is_binary(Term) -> erlang:binary_to_atom(Term, utf8);
name_to_atom(Term) when erlang:is_list(Term) -> erlang:list_to_atom(Term);
name_to_atom(Term) when erlang:is_atom(Term) -> Term.