-module(big).

-export([bench_args/2, run/0]).

bench_args(Version, Conf) ->
	{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	F = case Version of 
		short -> 8;
		intermediate -> 16;
		long -> 24
	end,
	[[N] || N <- [F * Cores]].

run() ->
  N = 30, 
%% spawn_procs (see below) returns an array of PIDs of spawned processes
	Procs = spawn_procs(N), %create N procs that wait for {procs, Procs, ReportTo} message
%% construct RMsgs in the form [{done,PID1},{done,PID2},..]
	RMsgs = lists:map(
    fun (P) -> 
        {done, P} 
    end, 
    Procs
  ),
%% send a message {procs, Procs, self()} to each process in Procs
	send_procs(Procs, {procs, Procs, self()}),

%% wait for "done" messages from all the procs
	receive_msgs(RMsgs),
%% sends "die" to all the procs
	lists:foreach(fun (P) -> P ! die end, Procs),
	ok.

%-------------------------------------------
%% see the notebook for explanations of all these different versions of
%% pinger() function
pinger([], [], true) ->
	receive
		{procs, Procs, ReportTo} -> 
      pinger(Procs, [], ReportTo)
  end;

pinger([], [], false) ->
	receive
		{ping, From} -> 
			From ! {pong, self()},
			pinger([],[],false);
		die ->
			ok			
	end;

pinger([], [], ReportTo) ->
	ReportTo ! {done, self()},
	pinger([],[],false);

pinger([],[Po|Pos] = Pongers, ReportTo) ->
	receive
	{ping, From} -> 
		From ! {pong, self()},
		pinger([], Pongers, ReportTo);
	{pong, Po} ->
		pinger([], Pos, ReportTo)
	end;

pinger([Pi|Pis], Pongers, ReportTo) ->
	receive 
		{ping, From} -> 
      From ! {pong, self()}
		after 0 -> ok
	end,
	Pi ! {ping, self()},
	pinger(Pis, [Pi|Pongers], ReportTo).
%-------------------------------------------

%-------------------------------------------
spawn_procs(N) when N =< 0 ->
	[];
spawn_procs(N) ->
	[spawn_link(
      fun () -> 
          pinger([], [], true) 
      end
   ) | spawn_procs(N-1) %no tail recursion??
  ].
%-------------------------------------------

send_procs([], Msg) ->
	Msg;
send_procs([P|Ps], Msg) ->
	P ! Msg,
	send_procs(Ps, Msg).

receive_msgs([]) ->
	ok;
receive_msgs([M|Ms]) ->
	receive
		M -> receive_msgs(Ms)
	end.

