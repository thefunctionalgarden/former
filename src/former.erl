-module(former).

-export([
    generate_html_section/1,

    init_conf/0,
    get_conf/1,
    set_conf/2,
    delete_conf/1
]).
-export([start/2]).
-export([stop/0]).
-export([get_path_conf_keys/0]).
-export([get_path_conf/1]).
-export([init_path_conf/0]).
-export([get_path_html_section/1]).
-export([do_callback/2]).
-export([get_static_scripts/0]).

% -- -- -- -- -- -- -- -- -- -- --

% Form#{
%     name => <<"">>,
%     inputs => [
%         #{
%             id => <<"">>,
%             name => <<"">>
%         }
%     ],
%     submit => #{
%         name => <<"">>,
%         module => <<"">>,   % required
%         function => <<"">>, % required
%         param_ids => []
%     }
% }

% -- -- -- -- -- -- -- -- -- -- --

start(Forms, HTTPPort) ->

    {ok, _} = application:ensure_all_started(former),
    save_paths_data(Forms),


    % handlers    
    Handlers = [
        former_handler
    ],
    io:format("~p:~p - ~n", [?MODULE, ?LINE]),
    
    Trails = trails:trails(Handlers),
    
    % store them
    trails:store(Trails),
    % and then compile them
    Dispatch = trails:single_host_compile(Trails),
    
    % start cowboy
    {ok, _} = cowboy:start_clear(
        former_http_listener,
        [{port, HTTPPort}],
        #{
            env => #{dispatch => Dispatch},
            protocol_options => [
                {versions, ['HTTP/1.1', 'HTTP/2']}
            ],
            middlewares => [cowboy_router, cowboy_handler]
        }
    ),

    % {ok, _} = cowboy:start_tls(former_http_listener,
    %     [
    %         {port, HTTPPort},
    %         {certfile, "/path/to/certfile"},
    %         {keyfile, "/path/to/keyfile"}
    %     ],
    %     #{env => #{dispatch => Dispatch}}
    % ),

    ok.

stop() ->
    ok = cowboy:stop_listener(former_http_listener).



% -- -- -- -- -- -- -- -- -- -- --


save_paths_data([]) ->
    ok;
save_paths_data([Form | MoreForms]) ->
    save_path_data(Form),
    save_paths_data(MoreForms).


save_path_data(Form) ->
    {
        FormSubmitModule,
        FormSubmitFunction,
        FormSubmitFunctionArity,
        SectionServerPath,
        HTML_Section
    } = generate_html_section(Form),
    set_path_conf(SectionServerPath, FormSubmitModule, FormSubmitFunction, FormSubmitFunctionArity, HTML_Section).



% generate_html_sections(Forms) ->
%     [Form] = Forms,
    
%     %TODO ITERATE THIS
%     {SectionServerPath, SectionHTML} = generate_html_section(Form),
%     SectionServerPaths = [SectionServerPath],
%     SectionsHTML = SectionHTML,

%     {SectionServerPaths, SectionsHTML}.

generate_html_section(Form) ->
    #{
        submit := FormSubmit
    } = Form,
    #{
        module   := FormSubmitModule,
        function := FormSubmitFunction
    } = FormSubmit,

    FormSubmitModuleStr   = atom_to_binary(FormSubmitModule),
    FormSubmitFunctionStr = atom_to_binary(FormSubmitFunction),

    MI = case catch apply(FormSubmitModule, module_info, []) of
        {'EXIT', {_, _}} -> throw(<<"module not found: ", FormSubmitModuleStr/bitstring>>);
        R -> R
    end,

    ME = proplists:get_value(exports, MI),
    FormSubmitFunctionArity = proplists:get_value(FormSubmitFunction, ME),
    case is_integer(FormSubmitFunctionArity) of
        true -> ok;
        false -> throw(<<"function not exported: ", FormSubmitFunctionStr/bitstring>>)
    end,

    SectionServerPath = get_server_path(FormSubmitModuleStr, FormSubmitFunctionStr),
    HTML_Inputs = generate_html_basic_inputs(SectionServerPath, FormSubmitFunctionStr, FormSubmitFunctionArity),
    HTML_SubmitButton = generate_html_submit_button(SectionServerPath, FormSubmitModuleStr, FormSubmitFunctionStr, FormSubmitFunctionArity),
    HTML_Output = generate_html_basic_output(SectionServerPath),
    % HTML_FormOpen = <<"<form name=\"", SectionServerPath/bitstring, "\"  action=\"", SectionServerPath/bitstring, "\" method=\"post\" >">>,
    HTML_FormOpen = <<"<form name=\"", SectionServerPath/bitstring, "\" id=\"", SectionServerPath/bitstring, "\" >">>,
    %   HTML_Form = <<"<form name=\"", SectionServerPath/bitstring, "\" id=\"", SectionServerPath/bitstring, "\" />">>,
    HTML_FormClose = <<"</form>">>,
    
    {
        FormSubmitModule,
        FormSubmitFunction,
        FormSubmitFunctionArity,
        SectionServerPath,
        <<
            % HTML_Form/bitstring, 
            HTML_FormOpen/bitstring, 
            HTML_Inputs/bitstring, 
            HTML_FormClose/bitstring,
            HTML_SubmitButton/bitstring, 
            HTML_Output/bitstring
        >>
    }.


generate_html_submit_button(SectionServerPath, FormSubmitModuleStr, FormSubmitFunctionStr, FormSubmitFunctionArity) ->
    FormSubmitFunctionArityStr = integer_to_binary(FormSubmitFunctionArity),
    <<"<button name=\"", SectionServerPath/bitstring, "button\" onClick=\"call(this.name)\">", FormSubmitModuleStr/bitstring, ":", FormSubmitFunctionStr/bitstring, "/", FormSubmitFunctionArityStr/bitstring, "</button>">>.

generate_html_basic_output(SectionServerPath) ->
    <<"<label name=\"", SectionServerPath/bitstring, "resp\"></label>">>.

generate_html_basic_inputs(_SectionServerPath, _FormSubmitFunctionStr, 0) ->
    <<"">>;
generate_html_basic_inputs(SectionServerPath, FormSubmitFunctionStr, FormSubmitFunctionArity) ->
    FormSubmitFunctionArityStr = integer_to_binary(FormSubmitFunctionArity),
    % InputLabel = <<FormSubmitFunctionStr/bitstring, "_", FormSubmitFunctionArityStr/bitstring>>,
    InputLabel = <<"P_", FormSubmitFunctionArityStr/bitstring>>,
    InputName = <<SectionServerPath/bitstring, FormSubmitFunctionArityStr/bitstring>>,
    HTML_InputN = <<
        "<label>",
        InputLabel/bitstring, ":",
        "<input name=\"", InputName/bitstring, "\" form=\"", SectionServerPath/bitstring, "\" />",
        % "<input name=\"", InputName/bitstring, "\"  />",
        "</label>"
    >>,
    HTML_PreviousInputs = generate_html_basic_inputs(SectionServerPath, FormSubmitFunctionStr, FormSubmitFunctionArity-1),
    <<HTML_PreviousInputs/bitstring, HTML_InputN/bitstring>>.

    

get_server_path(FormSubmitModuleStr, FormSubmitFunctionStr) ->
    <<"/", FormSubmitModuleStr/bitstring, "/", FormSubmitFunctionStr/bitstring, "/">>.


% https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
% <<"
% <form method=\"post\">
%   <label>
%     Name:
%     <input name=\"submitted-name\" autocomplete=\"name\" />
%   </label>
%   <button>Save</button>
% </form>
% ">>    

% -- -- -- -- -- -- -- -- -- -- --

get_static_scripts() ->
<<"    
    <script type=\"text/javascript\">
    async function call(buttonName)
    {
        const path = buttonName.replace(new RegExp(\"button$\"), '');
        const formName = path;
        const formData = new FormData(document.forms[formName]);
        const formDataQs = new URLSearchParams(formData);

        const myInit = {
            method: \"POST\",
            body: formDataQs
            // mode: \"same-origin\",
            // cache: \"no-cache\",
        };

        const myRequest = new Request(path);

        const response = await fetch(myRequest, myInit);
        if (!response.ok) {
            throw new Error(\"Server error: \" + response.status);
        }
        const responseText = await response.text();
        const respLabelName = formName + \"resp\";
        document.getElementsByName(respLabelName)[0].innerHTML = responseText;
    }
    </script>
">>.

% -- -- -- -- -- -- -- -- -- -- --

do_callback(SectionServerPath, DecodedBody) ->
    case get_path_conf(SectionServerPath) of
        [] -> <<"">>;
        {FormSubmitModule, FormSubmitFunction, FormSubmitFunctionArity, _} ->
            Params = get_params_list(DecodedBody, SectionServerPath, FormSubmitFunctionArity),
            io:format("~p:~p - ~p:~p:~p~n", [?MODULE, ?LINE, FormSubmitModule, FormSubmitFunction, Params]),
            apply(FormSubmitModule, FormSubmitFunction, Params)
    end.


get_params_list(_TuplesList, _SectionServerPath, 0) ->
    [];
get_params_list(TuplesList, SectionServerPath, FormSubmitFunctionArity) ->
    PreviousParams = get_params_list(TuplesList, SectionServerPath, FormSubmitFunctionArity - 1),
    ThisParamNumStr = integer_to_binary(FormSubmitFunctionArity),

    KeyFind = lists:keyfind(<<SectionServerPath/bitstring, ThisParamNumStr/bitstring>>, 1, TuplesList),
    UpdatedList = case KeyFind of
        false -> PreviousParams;
        {_, ThisParam} -> lists:append(PreviousParams, [ThisParam])
    end,
    UpdatedList.

% -- -- -- -- -- -- -- -- -- -- --



init_conf() ->
    case ets:info(forms_table) of
        undefined ->
            ets:new(forms_table, [set, named_table, public]);
        _->
            ok
    end.

get_conf(ConfName) ->
    [{_ConfKey, ConfValue}] = ets:lookup(forms_table, ConfName),
    ConfValue.

% get_confs(ConfName) ->
%     ets:lookup(forms_table, ConfName).

set_conf(ConfName, ConfValue) ->
    ets:insert(forms_table, {ConfName, ConfValue}).

delete_conf(ConfName) ->
    ets:delete(forms_table, ConfName).


% -- -- -- -- -- -- -- -- -- -- --



init_path_conf() ->
    case ets:info(former_path_conf) of
        undefined ->
            ets:new(former_path_conf, [set, named_table, public]);
        _->
            ok
    end.

get_path_conf_keys() ->
    init_path_conf(),
    L = ets:tab2list(former_path_conf),
    lists:map(
        fun({SectionServerPath, _V}) ->
            SectionServerPath
        end,
        L
    ).

get_path_conf(SectionServerPath) ->
    init_path_conf(),
    case ets:lookup(former_path_conf, SectionServerPath) of
        [] -> [];
        [{_, ConfValue}] -> ConfValue
    end.

set_path_conf(SectionServerPath, FormSubmitModule, FormSubmitFunction, FormSubmitFunctionArity, HTML_Section) ->
    init_path_conf(),
    ets:insert(former_path_conf, {SectionServerPath, {FormSubmitModule, FormSubmitFunction, FormSubmitFunctionArity, HTML_Section}}).

get_path_html_section(SectionServerPath) ->
    {_, _, _, HTML_Section} = get_path_conf(SectionServerPath),
    HTML_Section.

% -- -- -- -- -- -- -- -- -- -- --