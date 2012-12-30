-module(pcmark).

%-export([bench_args/2, run/3]).
-export([bench_args/2, run/0]).

-define(etstables, [ets1,ets2,ets3,ets4,ets5]).

%% abstract
%% 

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [4, 8, 8]; 
		intermediate -> [4, 19, 19]; 
		long -> [4, 29, 29]
    end,
	[[A,B,C] || A <- [F1 * Cores], B <- [F2 * Cores], C <- [F3 * Cores]].

%run([Size,Ongoing,Total|_], _, _) ->
run() ->
  Size = 200,
  Ongoing = 40,
  Total = 70,
	init_ets(?etstables, Size),
	master(init_procs(Ongoing), Total - Ongoing),
	[ets:delete(T) || T <- ?etstables],
	ok.

master(Pids, 0) -> 
	[receive {Pid, done} -> ok end || Pid <- Pids],
	ok;
master(Pids, N) ->
	receive
		{Pid, done} ->	
      Me  = self(),
			New = spawn_link(fun() -> worker(Me) end),
			master([New|lists:delete(Pid, Pids)], N - 1)
	end.

worker(Parent) ->
  %% S = sum of all the values stored in all 5 ets tables
	S = lists:foldl(
    fun (T, EA) ->
		  %% ets:foldl !!! (not lists:foldl)
      %% works with each {key, value} pair of a table
		  Ttotal = ets:foldl(
        fun ({K, V}, TA) ->
          %% update table's record with old_value+1
			    ets:insert(T, {K, V + 1 }),
			    TA + V
		    end, 
        0, T
      ),
		  Ttotal + EA 
	  end, 
    0, ?etstables
  ),
	do(S),
	Parent ! {self(), done}.

do(S) -> do(S,0).

do(S, N) when S > 0 ->
	do(S - 1, N + S);
do(_,_) -> ok.

init_procs(Ongoing) ->
	Me = self(),
	lists:foldl(
    fun (_,Pids) -> 
        [spawn_link(fun() -> worker(Me) end)|Pids] 
    end, 
    [], 
    lists:seq(1, Ongoing)
  ).

%% Create and populate each of 5 ets tables
init_ets([], _) -> ok;
init_ets([T|Ts], Size) ->
	ets:new(T, [public, named_table, ordered_set]),
	lists:foreach(
    fun(I) ->
		  ets:insert(T, {I, 1})
	  end, 
    lists:seq(1, Size)
  ),
	init_ets(Ts, Size).

