-module(genstress).

-behaviour(gen_server).

%-export([bench_args/2, run/3]).
-export([bench_args/2, run/0]).

% %gen cb:s

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, stop/0]).

-record(state, {mstate}).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [16, 4, 8]; 
		intermediate -> [16, 10, 11]; 
		long -> [16, 47, 79]
	end,
  %% Np == # of clients to create
  %% N == # of messages a client sends to the server
  %% Cqueue == # of messages a client puts into its message box and sends to server
    [[Type,Np,N,Cqueue] || Type <- [proc_call,gen_call], Np <- [F1 * Cores], N <- [F2 * Cores], Cqueue <- [F3 * Cores]].

%  run([Type,Np,N,Cqueue|_], _, _) ->
  run() ->
  Type = proc_call,
  Np = 64,
  N = 5,
  Cqueue = 15,
  Tone = now(),
	Server  = start_server(Type),
  %io:format("Server = ~p ~n", [Server]),
	Clients = start_clients(Np, Cqueue),
	[Pid ! {{Type, Server}, N, self()} || Pid <- Clients],
	[receive {Pid, ok} -> ok end || Pid <- Clients],
	stop_server({Type, Server}),
	stop_clients(Clients),
  Ttwo = now(),
  Differ = timer:now_diff(Ttwo, Tone),
  io:format("~ntime: ~p s~n", [Differ/1000000]),
	ok.

start_server(gen_call) -> genstress:start();
start_server(proc_call) -> spawn_link(fun() -> server() end).

stop_server({gen_call,  _}) -> genstress:stop();
stop_server({proc_call, S}) -> S ! stop.

%% returns a list of clients PIDs, all of which execute client(Queue)
start_clients(Np, Queue) -> 
	[spawn_link(fun() -> client(Queue) end) || _ <- lists:seq(1, Np)].

stop_clients(Clients) ->
	[erlang:exit(Client, normal) || Client <- Clients],
	ok.

client(Queue) ->
	[self() ! dont_match_me || _ <- lists:seq(1, Queue)],
	client().
client() ->
	receive
		{{gen_call,  _}, N, Pid} -> 
      client(gen_call, N, Pid);
		{{proc_call, S}, N, Pid} -> 
      client({proc_call,S}, N, Pid)
	end.

client(_, 0, Pid) -> Pid ! {self(), ok};
client(CallType , N, Pid) ->
	stress = client_call(CallType, stress),
	client(CallType, N - 1, Pid).

client_call(gen_call, Msg) -> gen_server:call(?MODULE, Msg);
client_call({proc_call,S}, Msg) -> S ! {self(), Msg}, receive {S,Ans} -> Ans end.

server() ->
	receive 
		stop -> ok;
		{From, Msg} -> From ! {self(), Msg}, server()
	end.

%% -------------------------------------------------------------------- %%
%%
%% start/stop
%%
%% -------------------------------------------------------------------- %%

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
	gen_server:call(?MODULE, stop).

init(_Args) ->
	{ok, #state{}}.

%% -------------------------------------------------------------------- %%
%%
%% handle_call
%%
%% -------------------------------------------------------------------- %%

handle_call(Command, From, S)->
	{reply, Command, S#state{mstate=From}}.

%% -------------------------------------------------------------------- %%
%%
%% handle_cast
%%
%% -------------------------------------------------------------------- %%

handle_cast(_Other, State) ->
	{noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% handle_info
%%
%% -------------------------------------------------------------------- %%

handle_info(_Info, State) ->
	{noreply, State}.

%% -------------------------------------------------------------------- %%
%%
%% termination
%%
%% -------------------------------------------------------------------- %%

terminate(_Reason, _State) ->
	ok.

%% -------------------------------------------------------------------- %%
%%
%% code_change
%%
%% -------------------------------------------------------------------- %%

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

