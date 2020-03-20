-module(custom_set).

-export([add/2, contains/2, difference/2, disjoint/2, empty/1, equal/2, from_list/1, intersection/2, subset/2,
	 union/2, test_version/0]).

% Realize sets as ordered lists.
% Insertion could also be handled by binary search.
add(Elem, []) -> [Elem];
add(Elem, Set) -> {Prefix, [Shead| _ ] = Suffix} = lists:splitwith(fun(E) -> E < Elem end, Set), 
		  if
		      Shead =:= Elem ->
			  Set;
		      true -> Prefix ++ [Elem | Suffix]
		  end.

% Could also use binary search.
contains(_, []) -> false;
contains(Elem, [Elem | _]) -> true;
contains(Elem, [_ | T]) -> contains(Elem, T).


empty([]) -> true;
empty(_) -> false.

equal(Set1, Set2) -> Set1 =:= Set2.

from_list(List) -> lists:usort(List).

intersection([], _) -> [];
intersection(_, []) -> [];
intersection([H1|T1] = S1, [H2|T2] = S2) -> if
						H1 =:= H2 ->
						    [H1 | intersection(T1, T2)];
						H1 < H2 ->
						    intersection(T1, S2);
						H1 > H2 ->
						    intersection(S1, T2)
					    end.

difference([], _) -> [];
difference(S1, []) -> S1;
difference([H1|T1] = S1, [H2|T2] = S2) -> if
					      H1 =:= H2 ->
						  difference(T1, T2);
					      H1 < H2 ->
						  [H1| difference(T1, S2)];
					      H1 > H2 ->
						  difference(S1, T2)
					  end.
						  

subset([], _) -> true;
subset(_, []) -> false;
subset([H1|T1], [H2|T2] = S2) -> if
					  H1 =:= H2 ->
					      subset(T1, T2);
					  H1 < H2 -> false;
					  H1 > H2 -> subset(T1, S2)
				 end.

% Could also be empty(intersection(S1, S2)).
disjoint([], _) -> true;
disjoint(_, []) -> true;
disjoint([H1|T1] = S1, [H2|T2] = S2) ->
    if
	H1 =:= H2 ->
	    false;
	H1 < H2 -> disjoint(T1, S2);
	H1 > H2 -> disjoint(S1, T2)
    end.
    

% Could also be usort(union(…))
union([], S2) -> S2;
union(S1, []) -> S1;
union([H1|T1] = S1, [H2|T2] = S2) -> if
					 H1 =:= H2 ->
					     [H1 | union(T1, T2)];
					 H1 < H2 ->
					     [H1 | union(T1, S2)];
					 H1 > H2 ->
					     [H2 | union(S1, T2)]
				     end.

test_version() -> 2.
