-module(parallel).

-export([bench_args/2, run/0]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2] = case Version of
		short -> [313, 4];
		intermediate -> [469, 8];
		long -> [1094, 10]
	end,
  %% M = # of spawned procs
  %% N = the size of the list that each proc populates w/ calls to now()
    [[N,M] || N <- [F1 * Cores], M <- [F2 * Cores]].

%run([N,M|_], _, _) ->
run() ->
  M = 10000,
  N = 300,
	Me   = self(),
  %% Base contains a list of M 'ok' atoms
	Base = [ok || _ <- lists:seq(1, M)],
	Pids = [
    spawn_link(
      fun() -> 
          loop(Me, N, []) 
      end
    ) 
    || _ <- lists:seq(1, M)
  ],
	Res  = [
    receive 
      {Pid, What} -> 
        What 
    end 
    || Pid <- Pids
  ],
	Base = Res,
	ok.

loop(Pid, 0, Out) -> Pid ! {self(), check_now(Out)};
loop(Pid, N, Out) -> loop(Pid, N - 1, [now()|Out]).

check_now([_,_]) -> ok;
check_now([_|Ts]) -> check_now(Ts).

