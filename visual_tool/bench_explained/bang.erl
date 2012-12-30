-module(bang).

-export([bench_args/2, run/0]).
-compile(debug_info).

bench_args(Version, Conf) ->
	{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
%% the value of Version can be changed in run.conf
%% the default is 'short'
	F = case Version of
		short -> 16;
		intermediate -> 55;
		long -> 79
	end,
	[[S,M] || S <- [F * Cores], M <- [F * Cores]].

run() ->
  S = 30,
  M = 1000,
	Parent = self(),
  % make_ref() returns a random reference <number,number,number,number>
	Done   = make_ref(),
	Bang   = {make_ref(),make_ref(),make_ref(),make_ref(),make_ref()},
%% creates a new process that executes fun(). This process waits for S*M
%% messages
	Rec    = spawn_opt(
    fun () -> 
        rec(Bang, S*M), 
        Parent ! Done %when all the messages have been received - send ok 
    end, 
    [link] %the link is created between Rec and Parent processes
  ),
%% creates S=(short|intermediate|long*cores) senders, each of them sends
%% M=(short|intermediate|long*cores) messages to the receiver
	lists:foreach(
    fun(_) -> %arguments are 1,2,3..S - don't care about them
		  spawn_link( %every process is linked to the parent process
        fun () -> 
            send(Rec, Bang, M) 
        end
      )
	  end, 
    lists:seq(1, S)
  ),
	receive 
    Done -> ok %when get the message "Done" (from the Rec) - ok.
  end,
	ok.

%% sender sends N messages (using recursion instead of
%% for (i=0, i<N, i++) {..} )
%% N==F*cores
send(_T, _M, 0) -> ok;
send(T, M, N)   -> T ! M, send(T, M, N-1).

rec(_M, 0) -> ok;
rec(M, N)  -> receive M -> rec(M, N-1) end.

