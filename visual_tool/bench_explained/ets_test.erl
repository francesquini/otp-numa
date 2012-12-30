-module(ets_test).

-export([bench_args/2, run/0]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
% N = # of writes/reads
% W = # of writers
% R = # of readers
	[F1, F2, F3] = case Version of
		short -> [157, 1, 8];
		intermediate -> [157, 32, 32];
		long -> [157, 79, 40]
	end,
	[[N,W,R] || N <- [F1 * Cores], W <- [F2 * Cores], R <- [F3 * Cores]].

%run([N,W,R|_], _, _) ->
run() ->
  N = 1200,
  W = 40,
  R = 40,
  Tone = now(),
	Parent = self(),
  io:format("xxxxxx"),
	T = ets:new(x, [public]),
	io:format("xxx"),
%% populate table with N dummy records {1, init}, {2, init}, ...
	w(T, N, init),
	Ws = lists:map(
    fun (_) ->
			spawn_link(
        fun () ->
				  receive 
            go -> ok 
          end,
				  w(T, N, self()),
				  w(T, N, self()),
				  Parent ! {done, self()},
				  receive 
            after infinity -> 
              ok 
            end
			  end
      )
		end,
    lists:seq(1, W)
  ),
	Rs = lists:map(
    fun (_) ->
			spawn_link(
        fun () ->
				  receive 
            go -> 
              ok 
          end,
				  r(T, N),
				  r(T, N),
				  Parent ! {done, self()},
				  receive 
            after infinity -> 
                ok 
            end
			end
    )
		end, 
    lists:seq(1, R)
  ),
	lists:foreach(fun (P) -> P ! go end, Ws),
	lists:foreach(fun (P) -> P ! go end, Rs),
	lists:foreach(fun (P) -> receive {done, P} -> ok end end, Ws),
	lists:foreach(fun (P) -> receive {done, P} -> ok end end, Rs),
	lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Ws),
	lists:foreach(fun (P) -> unlink(P), exit(P, bye) end, Rs),
  Ttwo = now(),
  Differ = timer:now_diff(Ttwo, Tone),
  io:format("~ntime: ~p s~n", [Differ/1000000]),
%  file:write_file("/home/iegorov/results", io_lib:fwrite("~p.\n", [Differ])),
	ok.

r(_T, 0) ->
	ok;
r(T, N) ->
	[{N, _}] = ets:lookup(T, N),
	r(T, N-1).

w(_T, 0, _V) ->
	ok;
w(T, N, V) ->
	true = ets:insert(T, {N, V}),	
	w(T, N-1, V).

