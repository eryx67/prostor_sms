%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2014, Vladimir G. Sekissov
%%% @doc API SMS-серрвиса http://api.prostor-sms.ru/messages/v2/
%%% ++++
%%% <p/>
%%% ++++
%%% @end
%%% Created : 15 Dec 2014 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(prostor_sms_srv).
-behaviour(gen_server).
-define(SRV, ?MODULE).

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("sutil/include/log.hrl").

-record(st, {base_url,
             login,
             password,
             src_address,
             request_opts
            }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SRV}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, BaseUrl} = prostor_sms:get_env(base_url),
    {ok, ReqOpts} = prostor_sms:get_env(request_opts),
    {ok, Login} = prostor_sms:get_env(login),
    {ok, Pass} = prostor_sms:get_env(password),
    {ok, SrcAddr} = prostor_sms:get_env(src_address),
    State = #st{base_url=ensure_binary(BaseUrl),
                request_opts=ReqOpts,
                login=ensure_binary(Login),
                password=ensure_binary(Pass),
                src_address=ensure_binary(SrcAddr)
               },
    {ok, State}.

handle_call(balance, _From, State) ->
    Path = [<<"balance">>],
    Params = [],
    {Res, State1} = handle_request(get, Path, Params, State),
    {reply, Res, State1};
handle_call({send_sms, DestAddrs, Data, _LifeTime}, _From,
            State=#st{src_address=SrcAddr}) ->
    Path = [<<"send">>],
    Params = [{<<"sender">>, SrcAddr},
              {<<"text">>, Data}
             ],
    {State1, Res1} =
        lists:foldl(
          fun (DA, {S, Acc}) ->
                  Ps = [{<<"phone">>, DA}|Params],
                  {Res1, S1} = handle_request(get, Path, Ps, S),
                  Res2 =
                      case Res1 of
                          {ok, [{<<"accepted">>, Code}]} ->
                              Code;
                          {ok, [Error]} ->
                              {error, Error};
                          Error={error, _} ->
                              Error
                      end,
                  {S1, [{DA, Res2}|Acc]}
          end, {State, []},
          if is_list(DestAddrs) -> DestAddrs;
             true -> [DestAddrs]
          end),
    TestErrF = fun ({_, {error, _}}) -> false; 
                   (_) -> true 
               end,
    Res2 = 
        case [R || R <- Res1, TestErrF(R) == true] of
            [] ->
                [{_, Err}|_] = Res1,
                Err;
            _ ->
                {ok, Res1}
        end,
    {reply, Res2, State1};
handle_call({sent_state, MsgId}, _From, State) ->
    Path = [<<"status">>],
    Params = [{<<"id">>, MsgId}],
    {Res, State1} = handle_request(get, Path, Params, State),
    {reply, Res, State1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_request(Method, Path, Params, S=#st{}) ->
    case do_request(Method, Path, Params, S) of
        Res={ok, _} ->
            {Res, S};
        Error={error, _} ->
            {Error, S}
    end.

do_request(Method, Path, Params, #st{base_url=BaseUrl, request_opts=ReqOpts,
                                     login=Login, password=Pass}) ->
    Params1 =[{<<"login">>, Login}, {<<"password">>, Pass}|Params],
    Url = hackney_url:make_url(BaseUrl, Path, Params1),
    case hackney:request(Method, Url, [], <<>>, ReqOpts) of
        {ok, Status, _Headers, Client} when Status =:= 200;
                                            Status =:= 201 ->
            case hackney:body(Client) of
                {ok, RespBody} ->
                    {ok, decode_body(RespBody)};
                {error, _Reason} = Error ->
                    Error
            end;
        {ok, Status, _Headers, Client} ->
            case hackney:body(Client) of
                {ok, RespBody} ->
                    ?debug("~p ~p -> ~p", [Path, Params, RespBody]),
                    {error, {Status, RespBody}};
                {error, _Reason} ->
                    {error, {Status, undefined}}
            end;
        {error, R} ->
            {error, R}
    end.

decode_body(Data) ->
    Recs = binary:split(Data, [<<$\n>>, <<$\r>>], [trim, global]),
    [begin
         [K|Vs] = binary:split(R, [<<$;>>], [trim, global]),
         {K, Vs}
     end || R <- Recs].

ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(B) when is_binary(B) ->
    B.
