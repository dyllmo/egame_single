%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : wss初始化
%%--------------------------------------------------------------------------------------------------------------
-module(wss_init).

-export([start/0]). 	%%-----启动

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 启动
%% @end
%%--------------------------------------------------------------------------------------------------------------
start () ->
	ssl:start(),
	Dispatch	 = cowboy_router:compile([{'_',[{"/",wss_handler,#{idle_timeout => 900000}}]}]),
	ServerPort	 = 8002,
	MaxConns	 = lib_misc:get_env_int(max_conns,65535),
	Acceptors	 = lib_misc:get_env_int(acceptors,10),
	cowboy:start_clear(
		websocket,
		#{
			max_connections => MaxConns,
			num_acceptors => Acceptors,
			socket_opts =>[{port,ServerPort}]
		},
		#{env => #{dispatch => Dispatch}}
	).