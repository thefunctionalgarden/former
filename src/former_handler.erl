-module(former_handler).

-behaviour(cowboy_handler).
-behaviour(trails_handler).


% -include_lib("kernel/include/logger.hrl").
% -include_lib("kernel/include/file.hrl").


-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).
-export([
    process_post/2,
    to_send/2
]).
-export([trails/0]).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


trails() ->
    Paths = former:get_path_conf_keys(),
    FormCallbackTrails = lists:map(
        fun(SectionServerPath) ->
            trails:trail(SectionServerPath, ?MODULE, [], #{})
        end,
        Paths
    ),
    
    FormerTrails = [
        trails:trail(<<"/former">>, ?MODULE, [], #{})
    ],

    lists:append(FormCallbackTrails, FormerTrails).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


% valid_content_headers(Req, State) ->
%     {true, Req, State}.

allowed_methods(Req0, State) ->
    Req = cowboy_req:set_resp_headers(#{
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Methods">> => <<"GET, HEAD, POST">>,
        <<"Access-Control-Allow-Headers">> => <<"Content-Type">>
    }, Req0),
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    %%  define AcceptCallback callback for PUT, POST, PATCH
    Handler = [
        % {<<"application/x-www-form-urlencoded">>, process_post}
        {'*', process_post}
    ],
    {Handler, Req, State}.

content_types_provided(Req, State) ->
    %%  define ProvideResource callback (ProvideCallback) for GET, HEAD
    Handler = [
        {<<"text/html">>, to_send}
    ],
    {Handler, Req, State}.



%% AcceptCallback   (for PUT, POST, PATCH)
%%      Result  :: true
%%               | {created, URI :: iodata()}
%%               | {see_other, URI :: iodata()}
%%               | false
process_post(Req0, State) ->
    Path = cowboy_req:path(Req0),
    {ok, DecodedBody, Req1} = cowboy_req:read_urlencoded_body(Req0),
    io:format("~p:~p - requested path: ~p~n", [?MODULE, ?LINE, Path]),
    io:format("~p:~p - request body: ~p~n", [?MODULE, ?LINE, DecodedBody]),
    RespBody = process_callback(Path, DecodedBody),
    io:format("~p:~p - RespBody: ~p~n", [?MODULE, ?LINE, RespBody]),
    RespBodyEnc = jsx:encode(RespBody),
    % RespBodyEnc = cow_qs:urlencode(RespBody),
    Req2 = cowboy_req:set_resp_body(RespBodyEnc, Req1),
    ReqN = cowboy_req:reply(
        200, 
        % #{ <<"content-type">> => <<"text/html">> },
        #{ <<"content-type">> => <<"application/json">> },
        Req2
    ),
    {stop, ReqN, State}.  %%  {Result, Req, State}


%% ProvideCallback   (for GET, HEAD)
%%      Result :: cowboy_req:resp_body()
to_send(Req0, State) ->
    Path = cowboy_req:path(Req0),
    io:format("~p:~p - requested path: ~p~n", [?MODULE, ?LINE, Path]),
    
    RespBody = process_former_req(Path),
    Req1 = cowboy_req:set_resp_body(RespBody, Req0),

    ReqN = cowboy_req:reply(
        200, 
        #{ <<"content-type">> => <<"text/html">> },
        RespBody,
        Req1
    ),
    {stop, ReqN, State}.  %%  {Result, Req, State}

    

process_former_req(<<"/former">>) ->
    SectionServerPaths = former:get_path_conf_keys(),
    SectionsHTML = lists:foldl(
        fun(SectionServerPath, SectionsHTMLSoFar) ->
            SectionHTML = former:get_path_html_section(SectionServerPath),
            <<SectionsHTMLSoFar/bitstring, SectionHTML/bitstring>>
        end,
        <<"">>,
        SectionServerPaths
    ),
    HTML_Body = [
        "<!DOCTYPE html>"
        "<html><head><title>",
        "Former",
        "</title>",
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
        former:get_static_scripts(),
        "</head><body>",
        SectionsHTML,
        "</body></html>"
    ],
    HTML_Body;
process_former_req(_OtherPath) ->
    [].

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

process_callback(Path, DecodedBody) ->
    RespBody = former:do_callback(Path, DecodedBody),
    RespBody.



%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
