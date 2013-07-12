%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc A dummy server providing processes for tests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(cpg_dummy_server).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         start_process/1,
         stop_process/1,
         registered_name/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(REGISTERED_NAME_PREFIX, "cpg_dummy_").

-record(state,
    {
        name
    }).

start_link(ClientName) ->
    gen_server:start_link({local, registered_name(ClientName)}, ?MODULE, [ClientName], []).

%% @doc Name used to register the process
-spec registered_name(binary()) -> binary().
registered_name(Name) ->
    binary_to_atom(<<?REGISTERED_NAME_PREFIX, Name/binary, ".process">>, utf8).

%% @doc Start a process
-spec start_process(binary()) -> supervisor:startchild_ret().
start_process(Name) when is_binary(Name) ->
    cpg_dummy_sup:start_process(Name).

%% @doc Stop a process
-spec stop_process(binary()) -> ok | {error, any()}.
stop_process(Name) ->
    cpg_dummy_sup:stop_process(Name).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Name]) ->
    {ok, #state{name = Name}}.

handle_call(_Request, _From, State) ->
    {stop, unknown_call, error, State}.

handle_cast(_From, State) ->
    {stop, unknown_cast, State}.

handle_info(_Info, State) ->
    {stop, unknown_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

