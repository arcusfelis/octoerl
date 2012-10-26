-module(octoerl_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
        search_case/0,
        search_case/1
]).

-compile([{parse_transform, lager_transform}]).

suite() ->
    [{timetrap, {minutes, 3}}].

%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_group(main_group, Config) ->
    lager:start(),
    gh_app:start(),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(main_group, Config) ->
    ok;
end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Configuration
%% ----------------------------------------------------------------------




%% Tests
%% ----------------------------------------------------------------------
groups() ->
    [{main_group, [], [search_case]}].

all() ->
    [{group, main_group}].



search_case() ->
    [{require, common_conf, octoerl_common_config}].

-include_lib("eunit/include/eunit.hrl").


search_case(_CommonTestCfg) ->
    {ok, Server} = gh_server:start_link([]),
    SearchResult = gh_server:search_repositories(Server, "otp"),
    io:format(user, "~nSearchResult: ~p~n", [SearchResult]),
    ok.


%% Helpers
%% ----------------------------------------------------------------------

