-module(octoerl_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
        search_case/0,
        search_case/1,

        list_commits_case/0,
        list_commits_case/1
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
    [{main_group, [], [search_case, list_commits_case]}].

all() ->
    [{group, main_group}].



search_case() ->
    [{require, common_conf, octoerl_common_config}].

list_commits_case() ->
    [{require, common_conf, octoerl_common_config}].

-include_lib("eunit/include/eunit.hrl").


search_case(CommonTestCfg) ->
    Con = create_gh_connector(),
%   DataDir = ?config(data_dir, CommonTestCfg), 
    SearchResult = gh_lib:extract_all(Con, "erlang", "erlang", 1),
%   FilePath = filename:join(DataDir, search_result_dump.bin),
%   ok = filelib:ensure_dir(FilePath),
%   file:write_file(FilePath, erlang:term_to_binary(SearchResult)),
    ok.

list_commits_case(_CommonTestCfg) ->
    Con = create_gh_connector(),
    gh_lib:extract_application_structure(Con, "arcusfelis", "csv_parser"),

    %% Check:
    %% `rebar.config', `src/*.app.src'.
    %% `test/' directory.

    ok.

create_gh_connector() ->
    {ok, CacheServer} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    {ok, Con} = gh_api:new([{cache_server, CacheServer}]),
    Con.




%% Helpers
%% ----------------------------------------------------------------------


