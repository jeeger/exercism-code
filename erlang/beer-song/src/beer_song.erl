-module(beer_song).

-export([verse/1, sing/1, sing/2, test_version/0]).

verse(2) ->
    "2 bottles of beer on the wall, 2 bottles of beer.\n"
	"Take one down and pass it around, 1 bottle of beer on the wall.\n";
verse(1) ->
    "1 bottle of beer on the wall, 1 bottle of beer.\n"
	"Take it down and pass it around, no more bottles of beer on the wall.\n";
verse(0) ->
    "No more bottles of beer on the wall, no more bottles of beer.\n"
	"Go to the store and buy some more, 99 bottles of beer on the wall.\n";
verse(N) ->
    lists:concat([N, " bottles of beer on the wall, ", N, " bottles of beer.\n",
	    "Take one down and pass it around, ", N-1, " bottles of beer on the wall.\n"]).

sing(N) ->
    sing(N, 0).

sing(From, To) ->
    lists:join($\n , [verse(N) || N <- lists:seq(From, To, -1)]) ++ "\n".

test_version() -> 1.
