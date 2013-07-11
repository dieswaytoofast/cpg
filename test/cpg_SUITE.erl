%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) Mahesh Paolini-Subramanya
%%% @end
%%%-------------------------------------------------------------------

-module(cpg_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ubic_records/include/ubic_records.hrl").

-compile(export_all).

-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).
-define(PROPTEST(F), true = proper:quickcheck(F())).
-define(PROPTEST(F, A), true = proper:quickcheck(F(A))).

-define(NUMTESTS, 10).
-define(SCOPES, [scope_1, scope_2, scope_3, scope_4, scope_5]).

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap,{seconds,320}}].

init_per_suite(Config) ->
    Config1 = setup_environment(Config),
    Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config1 = start(Config),
    Config1.

end_per_group(_GroupName, Config) ->
    stop(Config),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
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
%                t_check_leave_3
         ]},
        {crud, [],
         [ 
                t_create_1,
                t_create_2,
                t_delete_1,
                t_delete_2,
                t_join_1,
                t_join_2_a,
                t_join_2_b,
                t_join_3,
                t_leave_1,
                t_leave_2_a,
                t_leave_2_b,
                t_leave_3

         ]}
    ].

all() ->
    [
        {group, spec},
        {group, crud}
    ]. 

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


t_join_2_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_join_2_b, ScopeAll).

prop_join_2_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_each(ScopeAll, Name))).


t_join_3(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_join_2_b, ScopeAll).

prop_join_3(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_each(ScopeAll, Name, self()))).

t_leave_1(_) ->
    ?PROPTEST(prop_leave_1).

prop_leave_1() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_leave(Name))).


t_leave_2_a(_) ->
    ?PROPTEST(prop_leave_2_a).

prop_leave_2_a() ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_leave(Name, self()))).


t_leave_2_b(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_leave_2_b, ScopeAll).

prop_leave_2_b(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_leave_each(ScopeAll, Name))).


t_leave_3(Config) ->
    ScopeAll = ?config(scope_all, Config),
    ?PROPTEST(prop_leave_2_b, ScopeAll).

prop_leave_3(ScopeAll) ->
    numtests(?NUMTESTS,
             ?FORALL(Name, name(), join_and_leave_each(ScopeAll, Name, self()))).



%% Joins self() to Name within all the scopes in ScopeAll
join_each(ScopeAll, Name) ->
    lists:foreach(fun(Scope) ->
                ok =:= cpg:join(Scope, Name)
        end, ScopeAll),
    true.

join_each(ScopeAll, Name, Pid) ->
    lists:foreach(fun(Scope) ->
                ok =:= cpg:join(Scope, Name, Pid)
        end, ScopeAll),
    true.

%% Joins self() to Name within all the scopes in ScopeAll,
%% and leaves them
join_and_leave(Name) ->
    % Can leave if joined
    ok = cpg:join(Name),
    ok = cpg:leave(Name),
    try
        cpg:leave(Name)
    catch
        error:{badmatch, error} -> true
    end.

join_and_leave(Name, Pid) ->
    % Can leave if joined
    ok = cpg:join(Name, Pid),
    ok = cpg:leave(Name, Pid),
    try
        cpg:leave(Name, Pid)
    catch
        error:{badmatch, error} -> true
    end.

join_and_leave_each(ScopeAll, Name) ->
    lists:foreach(fun(Scope) ->
                % Can leave if joined
                ok = cpg:join(Scope, Name),
                ok = cpg:leave(Scope, Name),
                try
                    cpg:leave(Scope, Name)
                catch
                    error:{badmatch, error} -> true
                end
        end, ScopeAll),
    true.

join_and_leave_each(ScopeAll, Name, Pid) ->
    lists:foreach(fun(Scope) ->
                % Can leave if joined
                ok = cpg:join(Scope, Name, Pid),
                ok = cpg:leave(Scope, Name, Pid),
                try
                    cpg:leave(Scope, Name, Pid)
                catch
                    error:{badmatch, error} -> true
                end
        end, ScopeAll),
    true.





% For a trie, name() is a non_empty string
name() ->
    ?LET(X, non_empty(string()), X).




%% Start/Stop funs

start(Config) ->
    ok = reltool_util:application_start(cpg),
    ok = application:start(sasl),
    Config.

stop(_Config) ->
    ok = application:stop(sasl),
    ok = reltool_util:application_stop(cpg),
    ok.

% Add a couple of scopes to the config
setup_environment(Config) ->
    random:seed(erlang:now()),
    % So that env doesn't get overwritten
    ok = application:load(cpg),
    ScopeAll = [random_name(Scope) || Scope <- ?SCOPES],
    application:set_env(cpg, scope, ScopeAll),
    [{scope_all, ScopeAll}] ++ Config.

random_name(Name) when is_atom(Name) ->
    list_to_atom(binary_to_list(random_name(list_to_binary(atom_to_list(Name)))));
random_name(Name) when is_list(Name) ->
    binary_to_list(random_name(list_to_binary(Name)));
random_name(Name) when is_binary(Name) ->
    Id = list_to_binary(integer_to_list(random:uniform(999999999))),
    <<Name/binary, "_", Id/binary>>.

