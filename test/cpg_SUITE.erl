%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc CPG tests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(cpg_SUITE).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').


-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).
-define(PROPTEST(F), true = proper:quickcheck(F())).
-define(PROPTEST(F, A), true = proper:quickcheck(F(A))).

-define(NUMTESTS, 500).
-define(DEFAULT_NAME, <<"cpg_dummy">>).
-define(SCOPES, [scope_1, scope_2, scope_3, scope_4, scope_5]).
-define(REMOTE_NODES, ['b@paglierino', 'c@paglierino']).

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap,{seconds,320}}].

init_per_suite(Config) ->
    Config1 = setup_environment(Config),
    Config1.

end_per_suite(Config) ->
    teardown_environment(Config),
    ok.

init_per_group(_GroupName, Config) ->
    Config1 = start(Config),
    Config1.

end_per_group(_GroupName, Config) ->
    stop(Config),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    % Kill the dummy processes that  are used by this testcase
    stop_dummy_processes(Config),
    ok.


groups() ->
    [
        {spec, [],
         [ 
                t_check_create_1,
                t_check_create_2,
                t_check_delete_1,
                t_check_delete_2
                % Can't check types, because trie uses a non_empty_string, and 
                % we can't be sure that its a trie
%                t_check_join_1,
%                t_check_join_2,
%                t_check_join_3
%                t_check_leave_1,
%                t_check_leave_2,
%                t_check_leave_3,
%                t_check_get_members_1,
%                t_check_get_members_2,
%                t_check_get_members_3
%                t_check_get_local_members_1,
%                t_check_get_local_members_2,
%                t_check_get_local_members_3
%                t_check_get_remote_members_1,
%                t_check_get_remote_members_2,
%                t_check_get_remote_members_3
         ]},
        {test, [],
         [t_test]},
        {crud, [],
         [ 
                t_create_1,
                t_create_2,
                t_delete_1,
                t_delete_2,
                t_join_1,
                t_join_2_a,
                t_join_2_a_1,
                t_join_2_b,
                t_join_3,
                t_leave_1,
                t_leave_2_a,
                t_leave_2_b,
                t_leave_3,
                t_get_members_1_a,
                t_get_members_1_b,
                t_get_members_2_a,
                t_get_members_2_b,
                t_get_members_3,
                t_get_local_members_1_a,
                t_get_local_members_1_b,
                t_get_local_members_1_c,
                t_get_local_members_1_d,
                t_get_local_members_2_a,
                t_get_local_members_2_b,
                t_get_local_members_2_c,
                t_get_local_members_2_d,
                t_get_local_members_3_a,
                t_get_local_members_3_b,
                t_get_remote_members_1_a,
                t_get_remote_members_1_b,
                t_get_remote_members_1_c,
                t_get_remote_members_1_d,
                t_get_remote_members_2_a,
                t_get_remote_members_2_b,
                t_get_remote_members_2_c,
                t_get_remote_members_2_d,
                t_get_remote_members_3_a,
                t_get_remote_members_3_b


         ]}
    ].

all() ->
    [
        {group, spec},
        {group, crud}
%        {group, test}
    ]. 

t_test(_) ->
    ?PROPTEST(prop_test).

prop_test() ->
    numtests(?NUMTESTS,
             ?FORALL(_Pid, node_and_pids(remote), begin
                true
            end)).

%% Spec checks
t_check_create_1(_) ->
    ?CHECKSPEC(cpg, create, 1).

t_check_create_2(_) ->
    ?CHECKSPEC(cpg, create, 2).

t_check_delete_1(_) ->
    ?CHECKSPEC(cpg, delete, 1).

t_check_delete_2(_) ->
    ?CHECKSPEC(cpg, delete, 2).

t_check_join_1(_) ->
    ?CHECKSPEC(cpg, join, 1).

t_check_join_2(_) ->
    ?CHECKSPEC(cpg, join, 2).

t_check_join_3(_) ->
    ?CHECKSPEC(cpg, join, 3).

t_check_leave_1(_) ->
    ?CHECKSPEC(cpg, leave, 1).

t_check_leave_2(_) ->
    ?CHECKSPEC(cpg, leave, 2).

t_check_leave_3(_) ->
    ?CHECKSPEC(cpg, leave, 3).

t_check_get_members_1(_) ->
    ?CHECKSPEC(cpg, get_members, 1).

t_check_get_members_2(_) ->
    ?CHECKSPEC(cpg, get_members, 2).

t_check_get_members_3(_) ->
    ?CHECKSPEC(cpg, get_members, 3).

t_check_get_local_members_1(_) ->
    ?CHECKSPEC(cpg, get_local_members, 1).

t_check_get_local_members_2(_) ->
    ?CHECKSPEC(cpg, get_local_members, 2).

t_check_get_local_members_3(_) ->
    ?CHECKSPEC(cpg, get_local_members, 3).

t_check_get_remote_members_1(_) ->
    ?CHECKSPEC(cpg, get_remote_members, 1).

t_check_get_remote_members_2(_) ->
    ?CHECKSPEC(cpg, get_remote_members, 2).

t_check_get_remote_members_3(_) ->
    ?CHECKSPEC(cpg, get_remote_members, 3).

t_create_1(_) ->
    ?PROPTEST(prop_create_1).

prop_create_1() ->
    numtests(?NUMTESTS,
        ?FORALL(A, any(), ok =:= cpg:create(A))).

t_create_2(_) ->
    ?PROPTEST(prop_create_2).

prop_create_2() ->
    numtests(?NUMTESTS,
             ?FORALL({A, B}, {atom(), any()}, ok =:= cpg:create(A, B))).

t_delete_1(_) ->
    ?PROPTEST(prop_delete_1).

prop_delete_1() ->
    numtests(?NUMTESTS,
        ?FORALL(A, any(), ok =:= cpg:delete(A))).

t_delete_2(_) ->
    ?PROPTEST(prop_delete_2).

prop_delete_2() ->
    numtests(?NUMTESTS,
             ?FORALL({A, B}, {atom(), any()}, ok =:= cpg:delete(A, B))).

t_join_1(_) ->
    ?PROPTEST(prop_join_1).

prop_join_1() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), ok =:= cpg:join(Name))).


t_join_2_a(_) ->
    ?PROPTEST(prop_join_2_a).

prop_join_2_a() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), ok =:= cpg:join(Name, self()))).

t_join_2_a_1(_) ->
    ?PROPTEST(prop_join_2_a_1).

prop_join_2_a_1() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pid}, {name(), pid(local)}, ok =:= cpg:join(Name, Pid))).


t_join_2_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_join_2_b, ScopeAll).

prop_join_2_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name}, {scope(ScopeAll), name()}, ok =:= cpg:join(Scope, Name))).


t_join_3(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_join_2_b, ScopeAll).

prop_join_3(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pid}, {scope(ScopeAll), name(), pid(local)}, ok =:= cpg:join(Scope, Name, Pid))).

t_leave_1(_) ->
    ?PROPTEST(prop_leave_1).

prop_leave_1() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_leave(Name))).


t_leave_2_a(_) ->
    ?PROPTEST(prop_leave_2_a).

prop_leave_2_a() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pid}, {name(), pid(local)}, join_and_leave(Name, Pid))).


t_leave_2_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_leave_2_b, ScopeAll).

prop_leave_2_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name}, {scope(ScopeAll), name()}, join_and_leave(Scope, Name))).


t_leave_3(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_leave_3, ScopeAll).

prop_leave_3(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pid}, {scope(ScopeAll), name(), pid(local)}, join_and_leave(Scope, Name, Pid))).

% Add myself to the group
t_get_members_1_a(_) ->
    ?PROPTEST(prop_get_members_1_a).

prop_get_members_1_a() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_get_members(Name))).

% Add a bunch of processes to the group
t_get_members_1_b(_) ->
    ?PROPTEST(prop_get_members_1_b).

prop_get_members_1_b() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pids}, {name(), pids(local)}, join_and_get_members(Name, Pids, _Exclude = false))).


t_get_members_2_a(_) ->
    ?PROPTEST(prop_get_members_2_a).

prop_get_members_2_a() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pids}, {name(), pids(local)}, join_and_get_members(Name, Pids, _Exclude = true))).


t_get_members_2_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_members_2_b, ScopeAll).

prop_get_members_2_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pids}, {scope(ScopeAll), name(), pids(local)}, join_and_get_members(Scope, Name, Pids, _Exclude = false))).


t_get_members_3(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_members_3, ScopeAll).

prop_get_members_3(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pids}, {scope(ScopeAll), name(), pids(local)}, join_and_get_members(Scope, Name, Pids, _Exclude = true))).


% Add myself to the group
t_get_local_members_1_a(_) ->
    ?PROPTEST(prop_get_local_members_1_a).

prop_get_local_members_1_a() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_get_local_members(Name))).

% Add a bunch of local processes to the group
t_get_local_members_1_b(_) ->
    ?PROPTEST(prop_get_local_members_1_b).

prop_get_local_members_1_b() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pids}, {name(), pids(local)}, join_and_get_local_members(Name, Pids, [], _Exclude = false))).

% Add a bunch of remote processes to the group
t_get_local_members_1_c(_) ->
    ?PROPTEST(prop_get_local_members_1_c).

prop_get_local_members_1_c() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, RemoteNodeAndPids}, {name(), node_and_pids(remote)}, join_and_get_local_members(Name, [], RemoteNodeAndPids, _Exclude = false))).

% Add a bunch of local and remote processes to the group
t_get_local_members_1_d(_) ->
    ?PROPTEST(prop_get_local_members_1_d).

prop_get_local_members_1_d() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, LocalPids, RemoteNodeAndPids}, {name(), pids(local), node_and_pids(remote)}, join_and_get_local_members(Name, LocalPids, RemoteNodeAndPids, _Exclude = false))).

% Add a bunch of local processes to the group and exclude
t_get_local_members_2_a(_) ->
    ?PROPTEST(prop_get_local_members_2_a).

prop_get_local_members_2_a() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pids}, {name(), pids(local)}, join_and_get_local_members(Name, Pids, [], _Exclude = true))).

% Add a bunch of local and remote processes to the group and exclude
t_get_local_members_2_b(_) ->
    ?PROPTEST(prop_get_local_members_2_b).

prop_get_local_members_2_b() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, LocalPids, RemoteNodeAndPids}, {name(), pids(local), node_and_pids(remote)}, join_and_get_local_members(Name, LocalPids, RemoteNodeAndPids, _Exclude = true))).

% Add a bunch of local processes to the group and exclude
t_get_local_members_2_c(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_local_members_2_c, ScopeAll).

prop_get_local_members_2_c(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pids}, {scope(ScopeAll), name(), pids(local)}, join_and_get_local_members(Scope, Name, Pids, [], _Exclude = false))).

% Add a bunch of local and remote processes to the group and exclude
t_get_local_members_2_d(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_local_members_2_d, ScopeAll).

prop_get_local_members_2_d(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, LocalPids, RemoteNodeAndPids}, {scope(ScopeAll), name(), pids(local), node_and_pids(remote)}, join_and_get_local_members(Scope, Name, LocalPids, RemoteNodeAndPids, _Exclude = false))).

% Add a bunch of local processes to the group and exclude
t_get_local_members_3_a(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_local_members_3_a, ScopeAll).

prop_get_local_members_3_a(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pids}, {scope(ScopeAll), name(), pids(local)}, join_and_get_local_members(Scope, Name, Pids, [], _Exclude = true))).

% Add a bunch of local and remote processes to the group and exclude
t_get_local_members_3_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_local_members_3_b, ScopeAll).

prop_get_local_members_3_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, LocalPids, RemoteNodeAndPids}, {scope(ScopeAll), name(), pids(local), node_and_pids(remote)}, join_and_get_local_members(Scope, Name, LocalPids, RemoteNodeAndPids, _Exclude = true))).

% Add myself to the group
t_get_remote_members_1_a(_) ->
    ?PROPTEST(prop_get_remote_members_1_a).

prop_get_remote_members_1_a() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_get_remote_members(Name))).

% Add a bunch of remote processes to the group
t_get_remote_members_1_b(_) ->
    ?PROPTEST(prop_get_remote_members_1_b).

prop_get_remote_members_1_b() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pids}, {name(), pids(local)}, join_and_get_remote_members(Name, Pids, [], _Exclude = false))).

% Add a bunch of remote processes to the group
t_get_remote_members_1_c(_) ->
    ?PROPTEST(prop_get_remote_members_1_c).

prop_get_remote_members_1_c() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, RemoteNodeAndPids}, {name(), node_and_pids(remote)}, join_and_get_remote_members(Name, [], RemoteNodeAndPids, _Exclude = false))).

% Add a bunch of remote and remote processes to the group
t_get_remote_members_1_d(_) ->
    ?PROPTEST(prop_get_remote_members_1_d).

prop_get_remote_members_1_d() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, LocalPids, RemoteNodeAndPids}, {name(), pids(local), node_and_pids(remote)}, join_and_get_remote_members(Name, LocalPids, RemoteNodeAndPids, _Exclude = false))).

% Add a bunch of remote processes to the group and exclude
t_get_remote_members_2_a(_) ->
    ?PROPTEST(prop_get_remote_members_2_a).

prop_get_remote_members_2_a() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, Pids}, {name(), pids(local)}, join_and_get_remote_members(Name, Pids, [], _Exclude = true))).

% Add a bunch of remote and remote processes to the group and exclude
t_get_remote_members_2_b(_) ->
    ?PROPTEST(prop_get_remote_members_2_b).

prop_get_remote_members_2_b() ->
    numtests(?NUMTESTS,
             ?FORALL({Name, LocalPids, RemoteNodeAndPids}, {name(), pids(local), node_and_pids(remote)}, join_and_get_remote_members(Name, LocalPids, RemoteNodeAndPids, _Exclude = true))).

% Add a bunch of remote processes to the group and exclude
t_get_remote_members_2_c(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_remote_members_2_c, ScopeAll).

prop_get_remote_members_2_c(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pids}, {scope(ScopeAll), name(), pids(local)}, join_and_get_remote_members(Scope, Name, Pids, [], _Exclude = false))).

% Add a bunch of remote and remote processes to the group and exclude
t_get_remote_members_2_d(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_remote_members_2_d, ScopeAll).

prop_get_remote_members_2_d(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, LocalPids, RemoteNodeAndPids}, {scope(ScopeAll), name(), pids(local), node_and_pids(remote)}, join_and_get_remote_members(Scope, Name, LocalPids, RemoteNodeAndPids, _Exclude = false))).

% Add a bunch of remote processes to the group and exclude
t_get_remote_members_3_a(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_remote_members_3_a, ScopeAll).

prop_get_remote_members_3_a(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, Pids}, {scope(ScopeAll), name(), pids(local)}, join_and_get_remote_members(Scope, Name, Pids, [], _Exclude = true))).

% Add a bunch of remote and remote processes to the group and exclude
t_get_remote_members_3_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_get_remote_members_3_b, ScopeAll).

prop_get_remote_members_3_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL({Scope, Name, LocalPids, RemoteNodeAndPids}, {scope(ScopeAll), name(), pids(local), node_and_pids(remote)}, join_and_get_remote_members(Scope, Name, LocalPids, RemoteNodeAndPids, _Exclude = true))).

%% Joins self() to Name and leaves 
join_and_leave(Name) ->
    % Can leave if joined
    ok = cpg:join(Name),
    ok = cpg:leave(Name),
    try
        cpg:leave(Name)
    catch
        error:{badmatch, error} -> true
    end.

join_and_leave(Name, Pid) when is_pid(Pid) ->
    % Can leave if joined
    ok = cpg:join(Name, Pid),
    ok = cpg:leave(Name, Pid),
    try
        cpg:leave(Name, Pid)
    catch
        error:{badmatch, error} -> true
    end;

join_and_leave(Scope, Name) ->
    % Can leave if joined
    ok = cpg:join(Scope, Name),
    ok = cpg:leave(Scope, Name),
    try
        cpg:leave(Scope, Name)
    catch
        error:{badmatch, error} -> true
    end.

join_and_leave(Scope, Name, Pid) ->
    % Can leave if joined
    ok = cpg:join(Scope, Name, Pid),
    ok = cpg:leave(Scope, Name, Pid),
    try
        cpg:leave(Scope, Name, Pid)
    catch
        error:{badmatch, error} -> true
    end.

join_and_get_members(Name) ->
    % I am the only member in this group
    ok = cpg:join(Name),
    Self = self(),
    {ok, Name, [Self]} = cpg:get_members(Name),
    ok = cpg:leave(Name),
    {ok, Name, []} = cpg:get_members(Name),
    true.

join_and_get_members(Name, Pids, _Exclude = false) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),

    % Join the InPids
    setup_pids(Name, UPids, []),
    check_members(Name, UPids, undefined),

    % Cleanup
    cleanup_pids(Name, UPids, []),
    {ok, Name, []} = cpg:get_members(Name),
    true;


join_and_get_members(Name, Pids, _Exclude = true) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),
    EPid = random_exclude(local, UPids),

    % Join the InPids
    setup_pids(Name, UPids, []),
    check_members(Name, UPids, EPid),

    % Cleanup
    cleanup_pids(Name, UPids, []),
    {ok, Name, []} = cpg:get_members(Name),
    true.


join_and_get_members(Scope, Name, Pids, _Exclude = false) ->
    % Only the passed in Pids are members in this group
    % in this scope
    UPids = lists:sort(Pids),

    % Join the InPids
    setup_pids(Scope, Name, UPids, []),
    check_members(Scope, Name, UPids, undefined),

    % Cleanup
    cleanup_pids(Scope, Name, UPids, []),
    {ok, Name, []} = cpg:get_members(Scope, Name),
    true;

join_and_get_members(Scope, Name, Pids, _Exclude = true) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),
    EPid = random_exclude(local, UPids),

    % Join the InPids
    setup_pids(Scope, Name, UPids, []),
    check_members(Scope, Name, UPids, EPid),

    % Cleanup
    cleanup_pids(Scope, Name, UPids, []),
    {ok, Name, []} = cpg:get_members(Scope, Name),
    true.

join_and_get_local_members(Name) ->
    % I am the only member in this group
    ok = cpg:join(Name),
    Self = self(),
    {ok, Name, [Self]} = cpg:get_local_members(Name),
    ok = cpg:leave(Name),
    {error, {no_process, Name}} = cpg:get_local_members(Name),
    true.

join_and_get_local_members(Name, Pids, RemoteNodeAndPids, _Exclude = false) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),

    % Join the InPids
    setup_pids(Name, UPids, RemoteNodeAndPids),
    check_local_members(Name, UPids, undefined),

    % Cleanup
    cleanup_pids(Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_local_members(Name),
    true;

join_and_get_local_members(Name, Pids, RemoteNodeAndPids, _Exclude = true) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),
    EPid = random_exclude(local, UPids),

    % Join the InPids
    setup_pids(Name, UPids, RemoteNodeAndPids),
    check_local_members(Name, UPids, EPid),

    % Cleanup
    cleanup_pids(Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_local_members(Name),
    true.

join_and_get_local_members(Scope, Name, Pids, RemoteNodeAndPids, _Exclude = false) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),

    % Join the InPids
    setup_pids(Scope, Name, UPids, RemoteNodeAndPids),

    check_local_members(Scope, Name, UPids, undefined),

    % Cleanup
    cleanup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_local_members(Scope, Name),
    true;

join_and_get_local_members(Scope, Name, Pids, RemoteNodeAndPids, _Exclude = true) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),
    EPid = random_exclude(local, UPids),

    % Join the InPids
    setup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    check_local_members(Scope, Name, UPids, EPid),

    % Cleanup
    cleanup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_local_members(Scope, Name),
    true.

join_and_get_remote_members(Name) ->
    % I am the only member in this group
    ok = cpg:join(Name),
    {error, {no_process, Name}} = cpg:get_remote_members(Name),
    ok = cpg:leave(Name),
    true.

join_and_get_remote_members(Name, Pids, RemoteNodeAndPids, _Exclude = false) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),

    % Join the InPids
    setup_pids(Name, UPids, RemoteNodeAndPids),
    check_remote_members(Name, RemoteNodeAndPids, undefined),

    % Cleanup
    cleanup_pids(Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_remote_members(Name),
    true;

join_and_get_remote_members(Name, Pids, RemoteNodeAndPids, _Exclude = true) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),
    EPid = random_exclude(remote, UPids),

    % Join the InPids
    setup_pids(Name, UPids, RemoteNodeAndPids),
    check_remote_members(Name, RemoteNodeAndPids, EPid),

    % Cleanup
    cleanup_pids(Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_remote_members(Name),
    true.

join_and_get_remote_members(Scope, Name, Pids, RemoteNodeAndPids, _Exclude = false) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),

    % Join the InPids
    setup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    check_remote_members(Scope, Name, RemoteNodeAndPids, undefined),

    % Cleanup
    cleanup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_remote_members(Scope, Name),
    true;

join_and_get_remote_members(Scope, Name, Pids, RemoteNodeAndPids, _Exclude = true) ->
    % Only the passed in Pids are members in this group
    UPids = lists:sort(Pids),
    EPid = random_exclude(remote, UPids),

    % Join the InPids
    setup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    check_remote_members(Scope, Name, RemoteNodeAndPids, EPid),

    % Cleanup
    cleanup_pids(Scope, Name, UPids, RemoteNodeAndPids),
    {error, {no_process, Name}} = cpg:get_remote_members(Scope, Name),
    true.

% Get a random Pid to exclude (could be one not in the list
random_exclude(_, Pids) ->
    case random:uniform(2) of
        1 -> random_element(Pids);
        2 -> get_process()
    end.

% No excluded Pid
check_members(Name, [] , _) ->
    {error,{no_such_group, Name}} = cpg:get_members(Name);
check_members(Name, AllPids , undefined) ->
    {ok, Name, ReturnedPids} = cpg:get_members(Name),
    AllPids = lists:sort(ReturnedPids);
% Only one Pid in input list, and it is excluded
check_members(Name, _AllPids = [ExcludedPid], ExcludedPid) ->
            {error,{no_process,Name}} = cpg:get_members(Name, ExcludedPid);
check_members(Name, AllPids, ExcludedPid) ->
    {ok, Name, ReturnedPids} = cpg:get_members(Name, ExcludedPid),
    % Returned Pids are always in the InList
    true = contains(AllPids, ReturnedPids),
    % Excluded Pids are *not* in the InList
    false = lists:member(ExcludedPid, ReturnedPids).

% No excluded Pid
check_members(_Scope, Name, [] , _) ->
    {error,{no_such_group, Name}} = cpg:get_members(Name);
check_members(Scope, Name, AllPids , undefined) ->
    {ok, Name, ReturnedPids} = cpg:get_members(Scope, Name),
    AllPids = lists:sort(ReturnedPids);
% Only one Pid in input list, and it is excluded
check_members(Scope, Name, _AllPids = [ExcludedPid], ExcludedPid) ->
            {error,{no_process,Name}} = cpg:get_members(Scope, Name, ExcludedPid);
check_members(Scope, Name, AllPids, ExcludedPid) ->
    {ok, Name, ReturnedPids} = cpg:get_members(Scope, Name, ExcludedPid),
    % Returned Pids are always in the InList
    true = contains(AllPids, ReturnedPids),
    % Excluded Pids are *not* in the InList
    false = lists:member(ExcludedPid, ReturnedPids).

% No excluded Pid
check_local_members(Name, [] , _) ->
    {error,{no_process, Name}} = cpg:get_local_members(Name);
check_local_members(Name, AllPids , undefined) ->
    {ok, Name, ReturnedPids} = cpg:get_local_members(Name),
    AllPids = lists:sort(ReturnedPids);
% Only one Pid in input list, and it is excluded
check_local_members(Name, _AllPids = [ExcludedPid], ExcludedPid) ->
            {error,{no_process,Name}} = cpg:get_local_members(Name, ExcludedPid);
check_local_members(Name, AllPids, ExcludedPid) ->
    {ok, Name, ReturnedPids} = cpg:get_local_members(Name, ExcludedPid),
    % Returned Pids are always in the InList
    true = contains(AllPids, ReturnedPids),
    % Excluded Pids are *not* in the InList
    false = lists:member(ExcludedPid, ReturnedPids).

check_local_members(Scope, Name, [] , _) ->
    {error,{no_process, Name}} = cpg:get_local_members(Scope, Name);
check_local_members(Scope, Name, AllPids , undefined) ->
    {ok, Name, ReturnedPids} = cpg:get_local_members(Scope, Name),
    AllPids = lists:sort(ReturnedPids);
% Only one Pid in input list, and it is excluded
check_local_members(Scope, Name, _AllPids = [ExcludedPid], ExcludedPid) ->
            {error,{no_process,Name}} = cpg:get_local_members(Scope, Name, ExcludedPid);
check_local_members(Scope, Name, AllPids, ExcludedPid) ->
    {ok, Name, ReturnedPids} = cpg:get_local_members(Scope, Name, ExcludedPid),
    % Returned Pids are always in the InList
    true = contains(AllPids, ReturnedPids),
    % Excluded Pids are *not* in the InList
    false = lists:member(ExcludedPid, ReturnedPids).

% No excluded Pid
check_remote_members(Name, [] , _) ->
    {error,{no_process, Name}} = cpg:get_remote_members(Name);
check_remote_members(Name, RemoteNodeAndPids , undefined) ->
    AllPids = remote_pids(RemoteNodeAndPids),
    {ok, Name, ReturnedPids} = cpg:get_remote_members(Name),
    AllPids = lists:sort(ReturnedPids);
% Only one Pid in input list, and it is excluded
check_remote_members(Name, _RemoteNodeAndPids = [ExcludedPid], ExcludedPid) ->
            {error,{no_process,Name}} = cpg:get_remote_members(Name, ExcludedPid);
check_remote_members(Name, RemoteNodeAndPids, ExcludedPid) ->
    AllPids = remote_pids(RemoteNodeAndPids),
    {ok, Name, ReturnedPids} = cpg:get_remote_members(Name, ExcludedPid),
    % Returned Pids are always in the InList
    true = contains(AllPids, ReturnedPids),
    % Excluded Pids are *not* in the InList
    false = lists:member(ExcludedPid, ReturnedPids).

check_remote_members(Scope, Name, [] , _) ->
    {error,{no_process, Name}} = cpg:get_remote_members(Scope, Name);
check_remote_members(Scope, Name, RemoteNodeAndPids , undefined) ->
    AllPids = remote_pids(RemoteNodeAndPids),
    {ok, Name, ReturnedPids} = cpg:get_remote_members(Scope, Name),
    AllPids = lists:sort(ReturnedPids);
% Only one Pid in input list, and it is excluded
check_remote_members(Scope, Name, _RemoteNodeAndPids = [ExcludedPid], ExcludedPid) ->
            {error,{no_process,Name}} = cpg:get_remote_members(Scope, Name, ExcludedPid);
check_remote_members(Scope, Name, RemoteNodeAndPids, ExcludedPid) ->
    AllPids = remote_pids(RemoteNodeAndPids),
    {ok, Name, ReturnedPids} = cpg:get_remote_members(Scope, Name, ExcludedPid),
    % Returned Pids are always in the InList
    true = contains(AllPids, ReturnedPids),
    % Excluded Pids are *not* in the InList
    false = lists:member(ExcludedPid, ReturnedPids).

%% PropEr generators
% For a trie, name() is a non_empty string
name() ->
    ?LET(X, non_empty(word()), X).

word() ->  atom_to_list(node()) ++ list(oneof([integer($a, $z), integer($A, $Z), integer($0, $9) , oneof("!\"#$%&'()+,-./:;<=>?@[\\]^_`{|}\~")])).

pid(local) ->
    ?LET(X, oneof(get_process_list(local)), X).

pids(local) ->
    ?LET(X, non_empty(list(pid(local))), X).

node_and_pid(remote) ->
    ?LET(X, oneof(get_node_and_process_list(remote)), X).

node_and_pids(remote) ->
    ?LET(X, non_empty(list(node_and_pid(remote))), X).

scope(ScopeAll) ->
    ?LET(X, get_scope(ScopeAll), X).

get_process() ->
    {ok, Pid} = cpg_dummy_server:start_process(random_name(?DEFAULT_NAME)),
    Pid.

get_process_list(local) ->
    lists:map(fun(_) -> get_process() end, lists:seq(1, ?NUMTESTS)).

get_node_and_process() ->
    Node = get_remote_node(),
    {ok, Pid} = ct_rpc:call(Node, cpg_dummy_server, start_process, [random_name(?DEFAULT_NAME)]),
    {Node, Pid}.

get_node_and_process_list(remote) ->
    lists:map(fun(_) -> get_node_and_process() end, lists:seq(1, ?NUMTESTS)).

    
get_remote_node() ->
    random_element(?REMOTE_NODES).

get_scope(ScopeAll) ->
    random_element(ScopeAll).


%% Start/Stop funs

start(Config) ->
    ok = reltool_util:application_start(cpg),
    ok = application:start(sasl),
    Config.

stop(_Config) ->
    ok = application:stop(sasl),
    ok = reltool_util:application_stop(cpg),
    ok.

% Add a couple of scopes to the config,
% and startup CPG remotely
setup_environment(Config) ->
    random:seed(erlang:now()),
    % So that env doesn't get overwritten
    application:load(cpg),

    % Define the Scopes
    ScopeAll = [random_name(Scope) || Scope <- ?SCOPES],
    application:set_env(cpg, scope, ScopeAll),
    Config1 = [{scope_all, ScopeAll}] ++ Config,

    % Get cpg running on the remote nodes
    Config2 = [{nodes, ?REMOTE_NODES} | Config1],
    setup_remote_nodes(Config2).

% 
% Stop CPG remotely
teardown_environment(Config) ->
    teardown_remote_nodes(Config).

setup_remote_nodes(Config) ->
    Nodes = ?config(nodes, Config),
    [setup_remote_node(X, Config) || X <- Nodes],
    Config.

setup_remote_node(Node, Config) ->
    ScopeAll = ?config(scope_all, Config),
    ct_rpc:call(Node, application, set_env, [cpg, scope, ScopeAll]),
    ct_rpc:call(Node, application, get_env, [cpg, scope]),
    ct_rpc:call(Node, reltool_util, application_start, [cpg]).

teardown_remote_nodes(Config) ->
    Nodes = ?config(nodes, Config),
    [teardown_remote_node(X) || X <- Nodes],
    Config.

teardown_remote_node(Node) ->
    ct_rpc:call(Node, reltool_util, application_stop, [cpg]).

stop_dummy_processes(Config) ->
    Nodes = ?config(nodes, Config),
    ProcList = supervisor:which_children(cpg_dummy_sup),
    lists:foreach(fun({_, Pid, _, _}) -> 
                supervisor:terminate_child(cpg_dummy_sup, Pid)
        end, ProcList),
    [stop_remote_dummy_processes(X) || X <- Nodes].

stop_remote_dummy_processes(Node) ->
    ProcList = ct_rpc:call(Node, supervisor, which_children, [cpg_dummy_sup]),
    lists:foreach(fun({_, Pid, _, _}) -> 
                ct_rpc:call(Node, supervisor, terminate_child, [cpg_dummy_sup, Pid])
        end, ProcList).


%% Utility functions

random_name(Name) when is_atom(Name) ->
    list_to_atom(binary_to_list(random_name(list_to_binary(atom_to_list(Name)))));
random_name(Name) when is_list(Name) ->
    binary_to_list(random_name(list_to_binary(Name)));
random_name(Name) when is_binary(Name) ->
    Node = list_to_binary(atom_to_list(node())),
    Id = list_to_binary(integer_to_list(random:uniform(999999999))),
    <<Name/binary, "_", Node/binary, "_", Id/binary>>.

%% Randomly select one item from the list
random_element(InList) ->
    lists:nth(random:uniform(length(InList)), InList).

% Return a list that contains a (random) subset of the input list
%  (No duplicates)
random_elements(InList) ->
    random_elements(InList, []).
random_elements([], Acc) -> lists:sort(Acc);
random_elements([H | T], Acc) ->
    case random:uniform(2) of
        1 -> random_elements(T, [H | Acc]);
        2 -> random_elements(T, Acc)
    end.

%% ListB only consists of items in ListA
contains(_ListA, []) -> 
    true;
contains(ListA, _ListB = [H | Tail]) ->
    case lists:member(H, ListA) of
        false -> false;
        true -> contains(ListA, Tail)
    end.

%% ListB only consists of items not in ListA
not_contains(_ListA, []) -> true;
not_contains(ListA, _ListB = [H | Tail]) ->
    case lists:member(H, ListA) of
        true -> false;
        false -> contains(ListA, Tail)
    end.

%% Add the relevant Pids locally and remotely
setup_pids(Name, LocalPids, RemoteNodeAndPids) ->
    lists:foreach(fun(Pid) ->
                ok = cpg:join(Name, Pid)
        end, LocalPids),
    lists:foreach(fun({Node, Pid}) ->
                ok = remote_join(Node, Name, Pid)
        end, RemoteNodeAndPids).

setup_pids(Scope, Name, LocalPids, RemoteNodeAndPids) ->
    lists:foreach(fun(Pid) ->
                ok = cpg:join(Scope, Name, Pid)
        end, LocalPids),
    lists:foreach(fun({Node, Pid}) ->
                ok = remote_join(Node, Scope, Name, Pid)
        end, RemoteNodeAndPids).

%% Remove the relevant Pids locally and remotely
cleanup_pids(Name, LocalPids, RemoteNodeAndPids) ->
    lists:foreach(fun(Pid) ->
                cpg:leave(Name, Pid)
        end, lists:usort(LocalPids)),
    SortedNodeAndPids = lists:usort(fun({_,A}, {_, B}) -> A =< B end, RemoteNodeAndPids),
    lists:foreach(fun({Node, Pid}) ->
                ok = remote_leave(Node, Name, Pid)
            end, SortedNodeAndPids).

cleanup_pids(Scope, Name, LocalPids, RemoteNodeAndPids) ->
    lists:foreach(fun(Pid) ->
                ok = cpg:leave(Scope, Name, Pid)
        end, lists:usort(LocalPids)),
    SortedNodeAndPids = lists:usort(fun({_,A}, {_, B}) -> A =< B end, RemoteNodeAndPids),
    lists:foreach(fun({Node, Pid}) ->
                ok = remote_leave(Node, Scope, Name, Pid)
        end, SortedNodeAndPids).


%% Remote Utility Functions
remote_join(Node, GroupName, Pid) ->
    ok = ct_rpc:call(Node, cpg, join, [GroupName, Pid]).

remote_join(Node, Scope, GroupName, Pid) ->
    ok = ct_rpc:call(Node, cpg, join, [Scope, GroupName, Pid]).

remote_leave(Node, GroupName, Pid) ->
    ok = ct_rpc:call(Node, cpg, leave, [GroupName, Pid]).

remote_leave(Node, Scope, GroupName, Pid) ->
    ok = ct_rpc:call(Node, cpg, leave, [Scope, GroupName, Pid]).

remote_pids(RemoteNodeAndPids) ->
    RemotePids = lists:map(fun({_, Pid}) -> Pid end, RemoteNodeAndPids),
    lists:sort(RemotePids).

