-module(ehb).

-export([bench_args/2, run/0]).

-define(ACK, 20).
-define(DATA, {a,b,c,d,e,f,g,h,i,j,k,l}). %% 104 bytes on a 64-bit machine
-define(GSIZE, 20).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2] = case Version of 
		short -> [1, 4];
		intermediate -> [2,8];
		long -> [8, 8]
	end,    
	[[N,M] || N <- [F1 * Cores], M <- [F2 * Cores]].

% N = # of sender/receiver groups
% M = # of messages sent inside each group
%run([N,M|_], _, _) ->
run() ->
  N = 5,
  M = 100,
	Master = self(),
	Gs = lists:map(
    fun (_) -> 
        group(Master, M) 
    end, 
    lists:seq(1, N)
  ),
%% waits for "ready" message from all the Gs procs
	lists:foreach(
    fun (G) -> 
        receive {G, ready} -> 
            ok 
        end 
    end, 
    Gs
  ),
%% sends "go" to all Gs	
  lists:foreach(
    fun (G) -> 
        G ! {Master, go} 
    end, 
    Gs
  ),
	
  lists:foreach(
    fun (G) -> 
        receive 
          {G, done} -> 
            ok 
        end 
    end, 
    Gs
  ),

ok.
%%-------------------------------------------------
group(Master, Loop) ->
	spawn_link(
    fun () ->
      GMaster = self(),
      Rs = lists:map(
        fun (_) ->
          spawn_link(
            fun () -> 
                receiver(GMaster, ?GSIZE) 
            end
          )
        end, 
        lists:seq(1, ?GSIZE)
      ),
      Ss = lists:map(
        fun (_) ->
          spawn_link(
            fun () ->
              receive 
                {GMaster, go} -> sender(Rs,Loop) 
              end
            end
          )
        end, 
        lists:seq(1, ?GSIZE)
      ),
%% notify the Master with "ready" and wait for "go" from him 
      Master ! {self(), ready},
      receive 
        {Master, go} -> 
          ok 
      end,

      lists:foreach(
        fun (S) -> 
            S ! {GMaster, go} 
        end, 
        Ss
      ),
      
      lists:foreach(
        fun (R) -> 
            receive 
              {R, done} -> 
                ok 
            end 
        end, 
        Rs
      ),
      
      Master ! {self(), done}
	  end
  ).
%%-------------------------------------------------
sender(Rs, 0) ->
	lists:foreach(
    fun (R) -> 
        R ! done 
    end, 
    Rs
  );

sender(Rs, Loop) when Loop > ?ACK ->
	sender_ack(Rs, ?ACK),
	sender(Rs, Loop - ?ACK);

sender(Rs, Loop) ->
	lists:foreach(
    fun (R) -> 
        R ! ?DATA 
    end, 
    Rs
  ),
	sender(Rs, Loop-1).

sender_ack(Rs, 2) ->
	lists:foreach(
    fun (R) ->
			R ! ?DATA,
			R ! {self(), are_you_keeping_up}
		end, 
    Rs
  ),
	lists:foreach(
    fun (R) ->
			receive 
        {R, i_am_keeping_up} -> 
          ok 
      end,
			R ! ?DATA
		end, 
    Rs
  ),
	ok;

sender_ack(Rs, N) ->
	lists:foreach(
    fun (R) -> 
        R ! ?DATA 
    end, 
    Rs
  ),
	sender_ack(Rs, N-1).
%%-------------------------------------------------

receiver(GMaster, SendersLeft) ->
	receive
		done -> 
			case SendersLeft of
				1 -> GMaster ! {self(), done};
				_ -> receiver(GMaster, SendersLeft - 1)
			end;
		Msg -> 
			case Msg of
				{From, are_you_keeping_up} -> From ! {self(), i_am_keeping_up};
				_ -> ok
			end,
			receiver(GMaster, SendersLeft)
	end.

