%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : https初始化
%%--------------------------------------------------------------------------------------------------------------
-module(https_init).

-export([start/0]). 	%%-----启动

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 启动
%% @end
%%--------------------------------------------------------------------------------------------------------------
start() ->
	ssl:start(),
	Dispatch	 = cowboy_router:compile([{'_',[{"/login",https_handler,[]}]}]),
	ServerPort	 = 8001,
	MaxConns	 = lib_misc:get_env_int(max_conns,65535),
	Acceptors	 = lib_misc:get_env_int(acceptors,10),
	cowboy:start_clear( %%无证书，http，，，https参考wuxia
		https,
		#{
			max_connections => MaxConns,
			num_acceptors => Acceptors,
			socket_opts =>[{port,ServerPort}]
		},
		#{env => #{dispatch => Dispatch}}
	).
