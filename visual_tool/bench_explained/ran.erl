-module(ran).

-export([bench_args/2, run/0]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	F = case Version of
		short -> 1;
		intermediate -> 2;
		long -> 4
	end,
	[[N] || N <- [F * Cores]].

mk_ranlist(0, _, Acc) -> Acc;
mk_ranlist(N, M, Acc) -> mk_ranlist(N-1, M, [random:uniform(M) | Acc]). 

mk_ranlist(Len, Max) ->
	random:seed(Len, Max, Max * 2),
	mk_ranlist(Len, Max, []).

random(N) ->
	Len = 100000,
	{_, [Mid| _]} = lists:split(Len div 2, lists:sort(mk_ranlist(Len, 2*N))),
	Mid.

%run([N|_], _, _) when is_integer(N) ->
run() ->
  N = 70,
	Parent = self(),
	PList = lists:map(
    fun (_) ->
      spawn(
        fun () ->
          receive {Parent, go} -> ok end,
          Parent ! {self(), random(100)}
        end
      )
	  end, 
    lists:seq(1,N)
  ),
	lists:foreach(fun (P) -> P ! {Parent, go} end, PList),
	lists:foreach(fun (P) -> receive {P, _RN} -> ok end end, PList),
	ok.

