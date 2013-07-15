%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc Provides a set of dummy processes to use for cpg tests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(cpg_dummy_sup).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(supervisor).

-export([start_link/0]).
-export([start_process/1, stop_process/1]).
-export([init/1]).

-include("cpg_logging.hrl").


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_process(Name) ->
    supervisor:start_child(?MODULE, [Name]).


stop_process(Name) ->
    ChildName = cpg_dummy_server:registered_name(Name),
    supervisor:terminate_child(?MODULE, whereis(ChildName)).


init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{cpg_dummy_server, {cpg_dummy_server, start_link, []},
            permanent, 5000, worker, [cpg_dummy_server]}]}}.
