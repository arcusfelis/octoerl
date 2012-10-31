-module(gh_lib).
-export([extract_all/4,
         extract_application_structure/3]).

%% Returns a list of properties.
-spec extract_application_structure(Con, UserName, RepositoryName) -> Props
    when
    Con          :: gh_type:gh_api(),
    UserName        :: unicode:unicode_binary(),
    RepositoryName  :: unicode:unicode_binary(),
    Props :: [Prop],
    Prop :: has_src | has_rebar_config | has_c_src.

extract_application_structure(Con, UserName, RepositoryName) ->
    io:format(user, "Get app structure for ~ts/~ts~n", 
              [UserName, RepositoryName]),
%   {ok, HeadCommit} = 
%       gh_api:head_revision(Con, UserName, RepositoryName),
%   io:format(user, "Head's info is extracted:~n~p~n", [HeadCommit]),
%  
%   CommitObj   = proplists:get_value(<<"commit">>, HeadCommit),
%   TreeObj     = proplists:get_value(<<"tree">>, CommitObj),
%   TreeUrl     = proplists:get_value(<<"url">>, TreeObj),
%   io:format(user, "This URL provides info about a tree:~n~p~n", [TreeUrl]),

    %% http://developer.github.com/v3/git/trees/#get-a-tree
%   {ok, Tree}  = gh_api:follow_url(Con, TreeUrl),
    case gh_api:get_head_tree(Con, UserName, RepositoryName) of
    {ok, Tree} ->
    %   io:format(user, "Tree:~n~p~n", [Tree]),
        FileObjects = proplists:get_value(<<"tree">>, Tree),
        Path2FileObj = [{proplists:get_value(<<"path">>, FileObj), FileObj}
                        || FileObj <- FileObjects],

        %% Info about "rebar.config", 
        %% if there is no such file, than it is undefined.
        RebarConfFileObj = proplists:get_value(<<"rebar.config">>, Path2FileObj),
    %   io:format(user, "rebar.config: ~n~p~n", [RebarConfFileObj]),


        SrcConfFileObj  = proplists:get_value(<<"src">>, Path2FileObj),
    %   io:format(user, "src directory: ~n~p~n", [SrcConfFileObj]),


        CSrcConfFileObj  = proplists:get_value(<<"c_src">>, Path2FileObj),
    %   io:format(user, "c_src directory: ~n~p~n", [CSrcConfFileObj]),

        AppsDirObj  = proplists:get_value(<<"apps">>, Path2FileObj),
        RelDirObj   = proplists:get_value(<<"rel">>, Path2FileObj),
        PrivDirObj  = proplists:get_value(<<"priv">>, Path2FileObj),

        Props =
        [{has_rebar_config, RebarConfFileObj}
        ,{has_c_src_dir, CSrcConfFileObj}
        ,{has_src_dir,   SrcConfFileObj}
        ,{has_rel_dir,   RelDirObj}
        ,{has_priv_dir,  PrivDirObj}
        ,{has_apps_dir,  AppsDirObj}
        ],

        [K || {K, V} <- Props, V =/= undefined];
    {error, Reason} = E ->
        error_logger:error_msg("HTTP error: ~p~n", [Reason]),
        [E]

    %% Not found: 404
    %%
    %% Empty:
    %% 
    %% {{409,"Conflict"},
    %%  [{"X-RateLimit-Remaining","2849"},
    %%   {"X-Ratelimit-Limit","5000"},
    %%   {"X-Content-Type-Options","nosniff"},
    %%   {"X-Github-Media-Type",
    %%    "github.beta; format=json"},
    %%   {"Cache-Control",[]},
    %%   {"Content-Length","38"},
    %%   {"Status","409 Conflict"},
    %%   {"Connection","keep-alive"},
    %%   {"Content-Type",
    %%    "application/json; charset=utf-8"},
    %%   {"Date",
    %%    "Mon, 29 Oct 2012 19:22:22 GMT"},
    %%   {"Con","nginx"}],
    %%  <<"{\"message\":\"Git Repository is empty.\"}">>}

    end.


-spec extract_all(Con, KeyWord, Lang, Page) -> Recs
    when
    Con          :: gh_type:gh_api(),
    KeyWord         :: unicode:unicode_binary(),
    Lang            :: unicode:unicode_binary() | undefined,
    Page            :: non_neg_integer(),
    Recs            :: proplists:proplist().

extract_all(Con, KeyWord, Lang, Page) ->
    {ok, SearchResult} = 
        gh_api:search_repositories(Con, KeyWord, Lang, Page),
    case SearchResult of
        []    -> [];
        [_|_] -> SearchResult ++ extract_all(Con, KeyWord, Lang, Page + 1)
    end.
