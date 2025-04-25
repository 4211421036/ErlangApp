-module(solution).
-export([num_music_playlists/3]).

num_music_playlists(N, Goal, K) ->
    Mod = 1000000007,
    DP = init_dp(Goal, N),
    % Base case: DP[0][0] = 1
    BaseDP = set_dp(DP, 1, 1, 1),
    FilledDP = fill_dp(BaseDP, N, Goal, K, Mod),
    lookup_dp(FilledDP, Goal + 1, N + 1) rem Mod.

init_dp(Goal, N) ->
    lists:duplicate(Goal + 1, lists:duplicate(N + 1, 0)).

fill_dp(DP, N, Goal, K, Mod) ->
    fill_dp(DP, N, Goal, K, Mod, 1, 1).

fill_dp(DP, N, Goal, K, Mod, I, J) when I > Goal + 1 ->
    DP;
fill_dp(DP, N, Goal, K, Mod, I, J) when J > N + 1 ->
    fill_dp(DP, N, Goal, K, Mod, I + 1, 1);
fill_dp(DP, N, Goal, K, Mod, I, J) ->
    case I == 1 andalso J == 1 of
        true ->
            fill_dp(DP, N, Goal, K, Mod, I, J + 1);
        false ->
            % Calculate DP[i-1][j-1] (since DP is 1-based for the DP table)
            Val1 = case J > 1 andalso I > 1 of
                      true -> (lookup_dp(DP, I - 1, J - 1) * (N - (J - 2))) rem Mod;
                      false -> 0
                   end,
            Val2 = case I > 1 of
                      true -> (lookup_dp(DP, I - 1, J) * max(J - 1 - K, 0)) rem Mod;
                      false -> 0
                   end,
            NewVal = (Val1 + Val2) rem Mod,
            NewDP = set_dp(DP, I, J, NewVal),
            fill_dp(NewDP, N, Goal, K, Mod, I, J + 1)
    end.

set_dp(DP, Row, Col, Val) ->
    RowList = lists:nth(Row, DP),
    NewRowList = set_nth(Col, Val, RowList),
    set_nth(Row, NewRowList, DP).

lookup_dp(DP, Row, Col) ->
    lists:nth(Col, lists:nth(Row, DP)).

set_nth(1, X, [_ | T]) -> [X | T];
set_nth(N, X, [H | T]) -> [H | set_nth(N - 1, X, T)].
