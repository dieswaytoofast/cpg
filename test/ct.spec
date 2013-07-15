{define, 'DIR', "/Users/mahesh/src/git/cpg"}.
{node, a, 'a@paglierino'}.
{node, b, 'b@paglierino'}.
{node, c, 'c@paglierino'}.
{init, [a], [{node_start, [{startup_functions, [{application, start, [crypto, public_key]}]},
                              {monitor_master, true},
                              {erl_flags, "-pa 'DIR'/ebin -pz 'DIR'/deps/*/ebin"}]}]}.
{init, [b], [{node_start, [{startup_functions, [{application, start, [crypto, public_key]}]},
                              {monitor_master, true},
                              {erl_flags, "-pa 'DIR'/ebin -pz 'DIR'/deps/*/ebin"}]}]}.
{init, [c], [{node_start, [{startup_functions, [{application, start, [crypto, public_key]}]},
                              {monitor_master, true},
                              {erl_flags, "-pa 'DIR'/ebin -pz 'DIR'/deps/*/ebin"}]}]}.
{include, "'DIR'/include"}.
{logdir, master, "'DIR'/test/logs"}.
{logdir, a, "'DIR'/test/logs/a"}.
{logdir, b, "'DIR'/test/logs/b"}.
{logdir, c, "'DIR'/test/logs/c"}.
{groups, [a], ".", cpg_SUITE, all}.
%{groups, [b], ".", cpg_SUITE, all}.
%{groups, [c], ".", cpg_SUITE, all}.
