-module(timer_wheel).

-export([bench_args/2, run/0]).
-export([wheel/1,no_wheel/1]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	F = case Version of
		short -> 16;
		intermediate -> 40;
		long -> 125
	end,
  %% N = # of procs
	[[Wheel,N] || Wheel <- [wheel,no_wheel], N <- [F * Cores]].

run() ->
  N = 40,
  test(N, fun recv_loop_after/2).
%run([wheel,N|_], _, _) ->
%	test(N, fun recv_loop_after/2);
%run([no_wheel,N|_], _, _) ->
%	test(N, fun recv_loop/2).

wheel(N) ->
	test(N, fun recv_loop_after/2).

no_wheel(N) ->
	test(N, fun recv_loop/2).

test(N, Fun) ->
	Me = self(),
	Pids = [
    spawn_link(
      fun() -> 
        handler(N - 1, Fun, Me) 
      end
    ) 
    || _ <- lists:seq(1, N)
  ],
  %% send pids of all other processes to each process
	[Pid ! {init, Pids -- [Pid]} || Pid <- Pids],
	[Pid ! start || Pid <- Pids],
	[receive {Pid, done} -> ok end || Pid <- Pids],
	ok.

handler(N, Fun, Me) ->
  %% 'Others' contains pids of other procs
	Others = receive 
    {init, Pids} -> Pids 
  end,
	receive start -> ok end,
	loop(Others, N, Fun, Me).

loop([], 0, _, Me) ->
	Me ! {self(), done};
loop([], N, Fun, Me) ->
	loop([], Fun(undefined, N), Fun, Me);
loop([Pid|Pids], N, Fun, Me) ->
	Pid ! {self(), ping},
	loop(Pids, Fun(Pid, N), Fun, Me).

recv_loop_after(_, 0) ->
	0;
recv_loop_after(Pid, N) ->
	receive
		{Pid, pong} -> N;
		{Other, ping} ->	Other ! {self(), pong},
							recv_loop_after(Pid, N - 1)
		after 1073741824 -> exit(self(), kill)
	end.

recv_loop(_, 0) ->
	0;
recv_loop(Pid, N) ->
	receive
		{Pid, pong} -> N;
		{Other, ping} ->	Other ! {self(), pong},
							recv_loop(Pid, N - 1)
	end.

