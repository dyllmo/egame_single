%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : wss处理
%%--------------------------------------------------------------------------------------------------------------
-module(wss_handler).

-export([
	kill/1,						%%-----踢玩家下线
	send/2,						%%-----给玩家发送消息
	apply/4,					%%-----同步调用进程
	async_apply/4,				%%-----异步调用进程
	apply_after/2,				%%-----设置计时器
	apply_after_cancel/1		%%-----取消计时器
]).

-export([
	init/2,						%%-----HandlerInit
	websocket_init/1,			%%-----WebsocketInit
	websocket_handle/2,			%%-----WebsocketHandle
	websocket_info/2			%%-----WebsocketInfo
]).

-include("game.hrl").

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% HandlerInit
%% @end
%%--------------------------------------------------------------------------------------------------------------
init(Req, Opts) ->
	io:format("go wss init~n"),
	io:format("Req:~p~n Opts:~p~n", [Req, Opts]),
	Peer = cowboy_req:peer(Req),
	Ip = lib_misc:peer_to_ip(Peer),
	{cowboy_websocket,Req,#client_state{ip = Ip},Opts}.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% WebsocketInit
%% @end
%%--------------------------------------------------------------------------------------------------------------
websocket_init(State) ->
	io:format("go wss websocket_init~n"),
	io:format("State:~p~n ", [State]),
	{ok,State}.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% WebsocketHandle
%% @end
%%--------------------------------------------------------------------------------------------------------------
websocket_handle({binary,Request},State) ->
	io:format("go wss websocket_handle~n"),
	io:format("Request:~p~n State:~p~n", [Request, State]),
	try router:request(Request,State) of
		{ok,OutBin,NewState} ->
			{reply,{binary,OutBin},NewState}
	catch
		_ : Reason : Stack ->
			check_request_error(Request,Reason,Stack,State)
	end;
	
websocket_handle(_Request, State) ->
	io:format("go wss websocket_handle~n"),
	{ok,State}.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% WebsocketInfo
%% @end
%%--------------------------------------------------------------------------------------------------------------
websocket_info (Msg, State) ->
	io:format("go wss websocket_info~n"),
	io:format("Msg:~p~n State:~p~n", [Msg, State]),
	case Msg of
        {send,Data} ->
            {reply,{binary,Data},State};
        {apply,Pid,M,F,A} ->
            Pid ! try_apply(M,F,A,State),
            {ok,State};
        {async_apply,M,F,A} ->
			try_apply(M,F,A,State),
            {ok,State};
        {kill,Pid} ->
            mod_offline:clean(State),
            Pid ! ok,
            {stop,State};
        _ ->
            mod_offline:clean(State),
            {stop,State}
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 踢玩家下线
%% @end
%%--------------------------------------------------------------------------------------------------------------
kill (PidList) when is_list(PidList) ->
	io:format("go wss kill~n"),
	io:format("PidList:~p~n", [PidList]),
	lists:foreach(fun(Pid) -> kill(Pid) end,PidList);
	
kill (Pid) when is_pid(Pid) ->
	io:format("go wss kill~n"),
	io:format("Pid:~p~n", [Pid]),
	Pid ! {kill,self()},
	receive
		ok -> true
	after 1000 ->
		failed
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 给玩家发送消息
%% @end
%%--------------------------------------------------------------------------------------------------------------
send (PidList, OutBin) when is_list(PidList) ->
	io:format("go wss send~n"),
	io:format("PidList:~p~n OutBin:~p~n", [PidList, OutBin]),
	lists:foreach(fun(Pid) -> send(Pid,OutBin) end,PidList);
	
send (Pid, OutBin) when is_pid(Pid) ->
	io:format("go wss send~n"),
	io:format("Pid:~p~n OutBin:~p~n", [Pid, OutBin]),
	Pid ! {send,OutBin}.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 同步调用进程
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply (PidList, M, F, A) when is_list(PidList) ->
	io:format("go wss apply~n"),
	io:format("PidList:~p~n", [PidList]),
	lists:foreach(fun(Pid) -> apply(Pid,M,F,A) end,PidList);
	
apply (Pid, M, F, A) when is_pid(Pid) ->
	io:format("go wss apply~n"),
	case erlang:is_process_alive(Pid) of
		true ->
			case self() of
				Pid ->
					PlayerId = lib_misc:get_player_id(),
					try_apply(M,F,A,#client_state{player_id = PlayerId});
				_ ->
					Pid ! {apply,self(),M,F,A},
					receive
						{ok,Result} -> 
							Result;
						failed -> 
							apply_failed
					after 5000 ->
							apply_timeout
					end
			end;
		false ->
			delegate:apply(Pid,M,F,A)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步调用进程
%% @end
%%--------------------------------------------------------------------------------------------------------------
async_apply (PidList, M, F, A) when is_pid(PidList) -> 
	io:format("go wss async_apply~n"),
	lists:foreach(fun(Pid) -> async_apply(Pid,M,F,A) end,PidList);
	
async_apply (Pid, M, F, A) when is_pid(Pid) -> 
	io:format("go wss async_apply~n"),
	case erlang:is_process_alive(Pid) of
		true ->
			Pid ! {async_apply,M,F,A};
		false ->
			delegate:async_apply(Pid,M,F,A)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置计时器
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply_after (Time, {M, F, A}) ->
	io:format("go wss apply_after~n"),
    TimerRef = erlang:send_after(Time,self(),{async_apply,M,F,A}),
    {apply_after_ref,TimerRef}.
    
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 取消计时器
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply_after_cancel ({apply_after_ref, TimerRef}) ->
	io:format("go wss apply_after_cancel~n"),
    erlang:cancel_timer(TimerRef).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 尝试调用
%% @end
%%--------------------------------------------------------------------------------------------------------------
try_apply (M, F, A, State) ->
	io:format("go wss try_apply~n"),
    case catch apply(M, F, A) of
        {'EXIT',Reason} ->
			PlayerId = State #client_state.player_id,
			?ERROR(
				"try_apply_error:~n"
				"    player_id => ~p~n"
				"    mod       => ~p~n"
				"    func      => ~p~n"
				"    args      => ~w~n", 
				[PlayerId,M,F,A,Reason]
			),
            failed;
        Result ->
            {ok,Result}
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 检测请求错误
%% @end
%%--------------------------------------------------------------------------------------------------------------
check_request_error (Request, Reason, Stack, State) ->
	io:format("go wss check_request_error~n"),
	case catch handle_request_error(Request,Reason,Stack,State) of
		{'EXIT',Reason} ->
			{ok,State};
		{Result,NewState} ->
			case Result of
				keeplive ->
					{ok,NewState};
				offline ->
					mod_offline:clean(NewState),
					{stop,NewState}
			end
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 处理请求错误
%% @end
%%--------------------------------------------------------------------------------------------------------------
handle_request_error (Request, Reason, Stack, State) ->
	io:format("go wss handle_request_error~n"),
	PlayerId = State #client_state.player_id,
	Ip		 = State #client_state.ip,
	LastTime = State #client_state.last_error_time,
	{NowTime, _} = statistics(wall_clock),
	Result = if
		LastTime > 0 andalso NowTime - LastTime =< 1000 ->
			{offline,State #client_state{last_error_time = NowTime}};
		true ->
			{keeplive,State #client_state{last_error_time = NowTime}}
	end,
	error_log(PlayerId,Ip,Request,Reason,Stack),
	Result.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 错误日志
%% @end
%%--------------------------------------------------------------------------------------------------------------
error_log (PlayerId, Ip, Request, Reason, Stack) ->
	io:format("go wss error_log~n"),
	case catch router:mfargs(Request) of
		{M,F,Args} ->
			?ERROR(
				"router_request_error:~n"
				"    player_id => ~p~n"
				"    ip        => ~s~n"
				"    module    => ~p~n"
				"    function  => ~p~n"
				"    args      => ~w~n"
				"    reason    => ~p~n"
				"    stack     => ~p~n",
				[PlayerId,Ip,M,F,Args,Reason,Stack]
			);
		{'EXIT',_} ->
			?ERROR(
				"router_request_error:~n"
				"    player_id => ~p~n"
				"    ip        => ~s~n"
				"    request   => ~p~n"
				"    reason    => ~p~n",
				[PlayerId,Ip,Request,Reason]
			)
	end.