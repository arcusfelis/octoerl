%%% @doc This module is a `gen_server' that handles a single connection.
-module(gh_server).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         close/1,
         search_repositories/4,
         search_repositories/2]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

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


%% ------------------------------------------------------------------
%% Records' Definitions
%% ------------------------------------------------------------------

-record(state, {
        basic_request
}).


-record('DOWN',
{
    ref,   %% monitor reference
    type,  %% type of object 'process'
    id,    %% object id (pid)
    reason %% reason for termination
}).

-record(gh_search_repos, {
        keyword    :: binary(),
        language   :: binary() | undefined,
        start_page :: non_neg_integer() | undefined
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
        url_params  = []
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
-spec start_link(Args) -> {ok, gh_server()} when
    Args :: [Arg],
    Arg :: {user, string()}
         | {password, string()}
         | {timeout, non_neg_integer()}.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%% @doc Close all connections and kill a control process (aka Server).
close(Server) ->
    gen_server:call(Server, close).


search_repositories(Server, KeyWord) ->
    search_repositories(Server, KeyWord, undefined, undefined).


search_repositories(Server, KeyWord, Lang, StartPage) ->
    Mess = #gh_search_repos{
        keyword = KeyWord,
        language = Lang,
        start_page = StartPage
    },
    gen_server:call(Server, Mess).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
init(Args) ->
    User     = proplists:get_value(user, Args, ""),
    Password = proplists:get_value(password, Args, ""),

    BasicRequest = #request{ 
            headers     = auth_header(User, Password),
            timeout     = proplists:get_value(timeout, Args, 4000)
        },

    State = #state{
            basic_request   = BasicRequest
        },

    {ok, State}.

 
%% @private
handle_call(#gh_search_repos{} = Mess, _From, State) ->
    #gh_search_repos{
        keyword = KeyWord,
        language = Lang,
        start_page = StartPage
    } = Mess,
    #state{
        basic_request = BasicRequest
    } = State,
    Request = BasicRequest#request{
            path = "/legacy/repos/search/" ++ url_encode(KeyWord),
            url_params = drop_undef([{"language", Lang}, 
                                     {"start_page", StartPage}])
        },
    Reply = lhttpc_request(Request),
    {reply, Reply, State}.




%% @private
%%
%% Assosiate ResRef with TableHash.
handle_cast(_Mess, State) ->
    {noreply, State}.


%% @private
%% Ref is created for each process that uses resources.
handle_info(_Mess, State) ->
    {noreply, State}.


%% @private
terminate(_Reason, #state{}) ->
    ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




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


lhttpc_request(Request = #request{}) ->
    #request{
        method     = Method,
        host       = Host,
        port       = Port,
        path       = Path,
        is_ssl     = Ssl,
        headers    = Headers,
        timeout    = Timeout,
        options    = Options,
        body       = Body,
        url_params = UrlParams
    } = Request,
    io:format(user, "Request: ~p~n", [Request]),
    Path2 = join_url_params(Path, UrlParams),
    {ok, {{200,_}, _RespondHeaders, RespondBody}} =
    lhttpc:request(Host, Port, Ssl, Path2, Method,
                   Headers, Body, Timeout, Options),
    jsx:to_term(RespondBody).


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
        _  -> add_params(UrlParams)
    end.

url_encode(S) ->
    http_uri:encode(S).
