-module(serialmsg).

-export([bench_args/2, run/0]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [2, 16, 32];  
		intermediate -> [2, 32, 40];  
		long -> [5, 16, 32]
	end,
  %% P = # of receivers
  %% # of messages
  %% Message length
	[[P,N,L] || P <- [F1 * Cores], N <- [F2 * Cores], L <- [F3 * Cores]].

%run([P,N,L|_], _, _) ->
run() ->
  P = 40,
  N = 200,
  L = 40,
	Recvs = setup_receivers(P),
	Disp  = setup_dispatcher(),
	Gens  = setup_generators(Recvs, Disp, N, L),
	[Pid ! {self(), do} || Pid <- Gens],
	[receive {Pid, done} -> ok end || Pid <- Recvs],
	Disp ! {self(), done},
	ok.

%% setups

setup_receivers(P) -> setup_receivers(P, self(), []).

setup_receivers(0, _, Out) -> Out;
setup_receivers(P, Pid, Out) -> 
	setup_receivers(P - 1, Pid, [spawn_link(fun() -> receiver(Pid) end)|Out]).

setup_dispatcher() ->
	Me = self(),
	spawn_link(fun() -> dispatcher(Me) end).

setup_generators(Recvs, Disp, N, L) ->
	setup_generators(Recvs, Disp, self(), N, L, []).

setup_generators([],_,  _, _, _, Out) -> Out;
setup_generators([Recv|Recvs], Disp, Pid, N, L, Out) ->
	setup_generators(Recvs, Disp, Pid, N, L, [spawn_link(fun() -> generator(Recv, Disp, Pid, N, L) end) | Out]).

%% processes

receiver(Master) ->
	receive
		{_, done} -> Master ! {self(), done};
		{_, _} -> receiver(Master)
	end.

dispatcher(Master) ->
	receive
		{Master, done} -> ok;
		{Pid, To, Data} -> 	To ! {Pid, Data},
							dispatcher(Master)
	end.

generator(Recv, Disp, Master, N, L) ->
	Data = lists:seq(1, L),
	receive
		{Master, do} -> generator_push_loop(Recv, Disp, N, Data);
		{Master, do, NewN} -> generator_push_loop(Recv, Disp, NewN, Data)
	end.

generator_push_loop(Recv, Disp, 0, _) ->
	Disp ! {self(), Recv, done};
generator_push_loop(Recv, Disp, N, Data) ->
	Disp ! {self(), Recv, Data},
	generator_push_loop(Recv, Disp, N - 1, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

