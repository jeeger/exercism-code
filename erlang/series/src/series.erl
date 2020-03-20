-module(series).

-export([from_string/2, test_version/0]).

from_string(Width, String) when length(String) < Width  -> [];
from_string(Width, String = [_|T]) -> [lists:sublist(String, Width)] ++ from_string(Width, T).


test_version() -> 1.
