-module(gh_api).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1,
         follow_url/2]).

%% Search
-export([search_repositories/4,
         search_repositories/2]).

%% List commits
-export([head_revision/3]).

%% Refs
-export([head_revision_reference/3]).

%% Trees
-export([get_head_tree/3]).


%% ------------------------------------------------------------------
%% Library includes 
%% ------------------------------------------------------------------



%% ------------------------------------------------------------------
%% Import 
%% ------------------------------------------------------------------


%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

%% Used in handlers
-define(SERVER, ?MODULE).

%% Used for testing, then can be moved to an another file
-define(SRV, ?MODULE).
-define(APP, octoerl).

-define(GH_BUCKET, <<"github_api_cache">>).

-define(HTTP_DEFAULT_TIMEOUT, 10000).


%% ------------------------------------------------------------------
%% Records' Definitions
%% ------------------------------------------------------------------


%% http://developer.github.com/v3/search/#search-repositories
-record(gh_search_repos, {
        keyword    :: binary(),
        language   :: binary() | undefined,
        start_page :: non_neg_integer() | undefined
}).

%% http://developer.github.com/v3/repos/commits
-record(gh_list_commits, {
        user,
        repository,
        %% Sha. If `undefined', then all commits will be selected.
        commit :: binary() | undefined
}).

-record(gh_get_tree, {
        user,
        repository,
        %% Sha. 
        tree_hash :: binary()
}).

%% http://developer.github.com/v3/git/refs/#get-a-reference
-record(gh_get_reference, {
        user,
        repository,
        branch = [] :: [binary()]
}).

-record(request, {
        method      = "GET",
        host        = "api.github.com", 
        port        = 443, 
        is_ssl      = true,
        path,
        headers,
        timeout,
        options     = [],
        body        = [],
        %% Get params.
        url_params  = [],
        cache_server
}).

-record(gh_follow_url, {
        url
}).

%% ------------------------------------------------------------------
%% Import code
%% ------------------------------------------------------------------



%% ------------------------------------------------------------------
%% Declare parse transforms
%% ------------------------------------------------------------------



%% ------------------------------------------------------------------
%% Import external types
%% ------------------------------------------------------------------



%% ------------------------------------------------------------------
%% Internal types
%% ------------------------------------------------------------------

-type gh_server() :: pid().


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


%% @doc Start a linked server without supervision.
-spec new(Args) -> {ok, gh_server()} when
    Args :: [Arg],
    Arg :: {user, string()}
         | {password, string()}
         | {timeout, non_neg_integer()}.

new(Args) ->
    User     = proplists:get_value(user, Args, ""),
    Password = proplists:get_value(password, Args, ""),

    BasicRequest = #request{ 
            headers      = auth_header(User, Password),
            timeout      = proplists:get_value(timeout, Args, 
                                               ?HTTP_DEFAULT_TIMEOUT),
            cache_server = proplists:get_value(cache_server, Args)
        },
    {ok, BasicRequest}.


search_repositories(Con, KeyWord) ->
    search_repositories(Con, KeyWord, undefined, undefined).


search_repositories(Con, KeyWord, Lang, StartPage) ->
    Mess = #gh_search_repos{
        keyword = KeyWord,
        language = Lang,
        start_page = StartPage
    },
    handle_call(Mess, Con).


follow_url(Con, Url) ->
    Mess = #gh_follow_url{
        url = Url
    },
    handle_call(Mess, Con).


%% @doc Extract a `HEAD' commit information of the repository 
%%      `github.com/UserName/RepositoryName/'.
head_revision(Con, UserName, RepositoryName) ->
    Mess = #gh_list_commits{
        user = UserName,
        repository = RepositoryName,
        %% WARNING: Undocumented. Pass "HEAD" as a sha.
        %% http://developer.github.com/v3/repos/commits/#get-a-single-commit
        commit = <<"HEAD">>
    },
    handle_call(Mess, Con).


head_revision_reference(Con, UserName, RepositoryName) ->
    %% https://api.github.com/repos/arcusfelis/csv_parser/git/refs/heads/master
    %% http://developer.github.com/v3/git/refs/
    Mess = #gh_get_reference{
        user = UserName,
        repository = RepositoryName,
        branch = [<<"master">>]
    },
    handle_call(Mess, Con).


get_head_tree(Con, UserName, RepositoryName) ->
    %% http://developer.github.com/v3/git/trees/#get-a-tree
    %% Undocumented feature:
    %% https://api.github.com/repos/arcusfelis/csv_parser/git/trees/HEAD
    Mess = #gh_get_tree{
        user = UserName,
        repository = RepositoryName,
        tree_hash = "HEAD"
        },
    handle_call(Mess, Con).

 
%% @private
handle_call(#gh_search_repos{} = Mess, BasicRequest) ->
    #gh_search_repos{
        keyword = KeyWord,
        language = Lang,
        start_page = StartPage
    } = Mess,

    Request = BasicRequest#request{
            path = "/legacy/repos/search/" ++ url_encode(KeyWord),
            url_params = drop_undef([{"language", Lang}, 
                                     {"start_page", maybe(fun integer_to_list/1,
                                                          StartPage)}])
        },

    case lhttpc_request(Request) of
        {ok, JSONReply} ->
            {ok, proplists:get_value(<<"repositories">>, 
                                     jsx:to_term(JSONReply))};
        {error, _} = E ->
            E
    end;


%% Single commit:
%% https://api.github.com/repos/arcusfelis/csv_parser/commits/HEAD
%% A single object, contains more detail information, than for 
%% all-commits case.
%%
%% All commits:
%% https://api.github.com/repos/arcusfelis/csv_parser/commits
%% Brief information.
handle_call(#gh_list_commits{} = Mess, BasicRequest) ->
    #gh_list_commits{
        user = UserName,
        repository = RepositoryName,
        commit = CommitHash
    } = Mess,

    Path = "/repos/" 
        ++ url_encode(UserName) 
        ++ "/"
        ++ url_encode(RepositoryName)
        ++ "/commits"
        ++ case CommitHash of %% Handle an optional parameter.
           undefined -> ""; %% do not add a slash.
           _ -> "/" ++ url_encode(CommitHash)
           end,

    Request = BasicRequest#request{ path = Path },

    case lhttpc_request(Request) of
        {ok, JSONReply} -> %% `JSONReply' is a binary.
            {ok, jsx:to_term(JSONReply)};
        {error, _} = E ->
            E
    end;


handle_call(#gh_get_tree{} = Mess, BasicRequest) ->
    #gh_get_tree{
        user = UserName,
        repository = RepositoryName,
        tree_hash = TreeHash
    } = Mess,

    Path = "/repos/" 
        ++ url_encode(UserName) 
        ++ "/"
        ++ url_encode(RepositoryName)
        ++ "/git/trees/"
        ++ url_encode(TreeHash),

    Request = BasicRequest#request{ path = Path },

    case lhttpc_request(Request) of
        {ok, JSONReply} -> %% `JSONReply' is a binary.
            {ok, jsx:to_term(JSONReply)};
        {error, _} = E ->
            E
    end;



handle_call(#gh_follow_url{} = Mess, BasicRequest) ->
    #gh_follow_url{
        url = <<"https://api.github.com", PathBin/binary>>
    } = Mess,

    Request = BasicRequest#request{ path = binary_to_list(PathBin) },

    case lhttpc_request(Request) of
        {ok, JSONReply} -> %% `JSONReply' is a binary.
            {ok, jsx:to_term(JSONReply)};
        {error, _} = E ->
            E
    end;


handle_call(#gh_get_reference{} = Mess, BasicRequest) ->
    #gh_get_reference{
        user = UserName,
        repository = RepositoryName,
        branch = Branch % list()
    } = Mess,

    Path = "/repos/" 
        ++ url_encode(UserName) 
        ++ "/"
        ++ url_encode(RepositoryName)
        ++ "/git/refs/heads"
        ++ url_encode_list(Branch),

    Request = BasicRequest#request{ path = Path },

    case lhttpc_request(Request) of
        {ok, JSONReply} -> %% `JSONReply' is a binary.
            {ok, jsx:to_term(JSONReply)};
        {error, _} = E ->
            E
    end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% -----------------------------------------------------------------
%% Internal helpers.
%% -----------------------------------------------------------------


auth_header("", "") ->
    [];
auth_header(User, Password) ->
    Auth = "Basic " ++ binary_to_list(base64:encode(User ++ ":" ++ Password)),
    [{"Authorization", Auth}].


lhttpc_request(Request = #request{cache_server = CacheServer}) ->
    Request2 = build_url_path(Request),
    case CacheServer of
        undefined -> http_lhttpc_request(Request2);
        _ ->         cache_riak_request(Request2)
    end.


http_lhttpc_request(Request = #request{}) ->
    #request{
        method     = Method,
        host       = Host,
        port       = Port,
        path       = Path,
        is_ssl     = Ssl,
        headers    = Headers,
        timeout    = Timeout,
        options    = Options,
        body       = Body
    } = Request,
    io:format(user, "Request: ~p~n", [Request]),
    io:format(user, "Ask: ~p~n", [Path]),
    case lhttpc:request(Host, Port, Ssl, Path, Method,
                        Headers, Body, Timeout, Options) of
        {ok, {{200,_}, _RespondHeaders, RespondBody}} ->
            {ok, RespondBody};
        {ok, BadRespond} ->
            {error, {http_bad_respond, BadRespond}};
        {error, _Reason} = E ->
            E
    end.

build_url_path(Request = #request{path = Path, url_params = UrlParams}) ->
    Request#request{path = join_url_params(Path, UrlParams),
                    url_params = []}.


cache_riak_request(Request = #request{cache_server = CacheServer, path = Path}) ->
    Key = list_to_binary(Path),
    io:format(user, "Riak get: ~p~n", [Key]),
    case riakc_pb_socket:get(CacheServer, ?GH_BUCKET, Key) of
        {ok, Obj} ->
            io:format(user, "Riak found: ~p~n", [Key]),
            {ok, riakc_obj:get_value(Obj)};

        {error, notfound} ->
            case lhttpc_request(Request#request{cache_server = undefined}) of
                {ok, RespondBody} = R ->
                    io:format(user, "Riak put: ~p~n", [Key]),
                    Obj = riakc_obj:new(?GH_BUCKET, Key, RespondBody),
                    Indexes = [{<<"retrieved_int">>, timestamp()}],
                    MetaData = dict:store(<<"index">>, Indexes, dict:new()),
                    Obj1 = riakc_obj:update_metadata(Obj, MetaData),
                    ok = riakc_pb_socket:put(CacheServer, Obj1),
                    R;
                {error, _Reason} = E ->
                    E
            end
    end.


drop_undef(PL) ->
    [X || X = {_K, V} <- PL, V =/= undefined].


%% @doc Concat `Path' and GET parameters.
join_url_params(Path, []) ->
    Path;

join_url_params(Path, UrlParams) ->
    Path ++
    case lists:member($?, Path) of
        true  -> add_params(UrlParams);
        false -> [$? | add_params(UrlParams)]
    end.

add_params([{K, V} | UrlParams]) ->
    url_encode(K) ++ "=" ++ url_encode(V) ++
    case UrlParams of
        [] -> "";
        _  -> "&" ++ add_params(UrlParams)
    end.


url_encode(undefined) ->
    "";
url_encode(S) when is_binary(S) ->
    url_encode(binary_to_list(S));
url_encode(S) ->
    http_uri:encode(S).


url_encode_list([]) -> "";
url_encode_list([H|T]) -> "/" ++ url_encode(H) ++ url_encode(T).


maybe(_F, undefined) -> undefined;
maybe(F, X)          -> F(X).


%% Now timestamp in seconds.
timestamp() ->
    {Mega,Sec,_Micro} = os:timestamp(),
    Mega*1000000+Sec.


