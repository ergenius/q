%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2017 8:38 PM
%%%-------------------------------------------------------------------
-module(q_assets).
-author("Madalin Grigore-Enescu").

-include_lib("q/include/q.hrl").

-export([exist/1]).
-export([read/1]).
-export([consult/1]).
-export([update/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec exist(Asset) -> boolean() when
    Asset :: filename_all() | binary() | atom().

%% @doc Returns true if the specified assets file exists or false otherwise.
exist(Asset) ->

    AssetKey    = asset_key(Asset),
    Exists      = ets:member(?Q_ETS_ASSETS_FILES, AssetKey),
    case Exists of
        true -> true;
        _ ->

            AssetFilename = filename:join(?Q_PATH_ASSETS, AssetKey),
            filelib:is_file(AssetFilename)

    end.

-spec read(Asset) -> {ok, Binary} | Error when
    Asset :: filename_all() | binary() | atom(),
    Binary :: binary(),
    Error  :: q_error().

%% @doc Returns {ok, Binary}, where Binary is a binary data object that contains the contents of Asset asset, or q_error() if an error occurs.
read(Asset) ->

    AssetKey     = asset_key(Asset),
    Record       = ets:lookup(?Q_ETS_ASSETS_FILES, AssetKey),
    case Record of
        [{AssetKey, Binary}] -> {ok, Binary};
        _ ->

            AssetFilename = filename:join(?Q_PATH_ASSETS, AssetKey),
            Result   	    = file:read_file(AssetFilename),
            case Result of
                {ok, Binary} ->

                    ets:insert(?Q_ETS_ASSETS_FILES, {AssetKey, Binary}),
                    {ok, Binary};

                Error -> ?Q_ERROR(Error)
            end
    end.

-spec consult(Asset) -> {ok, Terms} | Error when
    Asset     :: filename_all() | binary() | {atom(), atom()} | atom(),
    Terms     :: [term()],
    Error     :: q_error().

%% @doc Reads Erlang terms, separated by '.', from the specified assets file.
%% Returns {ok, Terms}, where Terms is a list of Erlang terms, or q_error() if an error occurs.
consult(Asset) ->

    AssetKey  = asset_key(Asset),
    Record    = ets:lookup(?Q_ETS_ASSETS_FILES_CONSULT, AssetKey),
    case Record of
        [{AssetKey, Terms}] -> {ok, Terms};
        _ ->

            AssetFilename   = filename:join(?Q_PATH_ASSETS, AssetKey),
            Result   	      = file:consult(AssetFilename),
            case Result of
                {ok, Terms} ->

                    ets:insert(?Q_ETS_ASSETS_FILES_CONSULT, {AssetKey, Terms}),
                    {ok, Terms};

                Error -> ?Q_ERROR(Error)
            end
    end.

%% @doc Update all the assets files
update() ->

    ets:delete_all_objects(?Q_ETS_ASSETS_FILES),
    ets:delete_all_objects(?Q_ETS_ASSETS_FILES_CONSULT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec asset_key(Asset) -> string() when
    Asset :: string() | binary() | atom().

%% @doc Returns a unique key for the specified file asset
asset_key(Asset) when erlang:is_list(Asset) -> Asset;
asset_key(Asset) when erlang:is_binary(Asset) -> erlang:binary_to_list(Asset);
asset_key(Asset) when erlang:is_atom(Asset) -> erlang:atom_to_list(Asset).

