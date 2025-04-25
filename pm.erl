-module(music_playlists).
-export([num_music_playlists/3]).

num_music_playlists(N, Goal, K) ->
    Mod = 1000000007,
    DP = init_dp(Goal + 1, N + 1),
    % Base case: DP[1][1] = 1 (1-based indexing)
    BaseDP = set_dp(DP, 1, 1, 1),
    FilledDP = fill_dp(BaseDP, N, Goal, K, Mod),
    lookup_dp(FilledDP, Goal + 1, N + 1) rem Mod.

init_dp(Rows, Cols) ->
    [lists:duplicate(Cols, 0) || _ <- lists:seq(1, Rows)].

fill_dp(DP, N, Goal, K, Mod) ->
    fill_dp(DP, N, Goal, K, Mod, 1, 1, Goal + 1, N + 1).

fill_dp(DP, _, _, _, _, I, _, MaxI, _) when I > MaxI ->
    DP;
fill_dp(DP, N, Goal, K, Mod, I, J, MaxI, MaxJ) when J > MaxJ ->
    fill_dp(DP, N, Goal, K, Mod, I + 1, 1, MaxI, MaxJ);
fill_dp(DP, N, Goal, K, Mod, I, J, MaxI, MaxJ) ->
    case I =:= 1 andalso J =:= 1 of
        true ->
            fill_dp(DP, N, Goal, K, Mod, I, J + 1, MaxI, MaxJ);
        false ->
            % Calculate DP[i][j] based on recurrence relation
            Val1 = case I > 1 andalso J > 1 of
                      true -> (lookup_dp(DP, I - 1, J - 1) * (N - (J - 2))) rem Mod;
                      false -> 0
                   end,
            Val2 = case I > 1 of
                      true -> (lookup_dp(DP, I - 1, J) * max(J - 1 - K, 0)) rem Mod;
                      false -> 0
                   end,
            NewVal = (Val1 + Val2) rem Mod,
            NewDP = set_dp(DP, I, J, NewVal),
            fill_dp(NewDP, N, Goal, K, Mod, I, J + 1, MaxI, MaxJ)
    end.

set_dp(DP, Row, Col, Val) ->
    RowList = lists:nth(Row, DP),
    NewRowList = set_nth(Col, Val, RowList),
    set_nth(Row, NewRowList, DP).

lookup_dp(DP, Row, Col) ->
    lists:nth(Col, lists:nth(Row, DP)).

set_nth(1, X, [_ | T]) -> [X | T];
set_nth(N, X, [H | T]) -> [H | set_nth(N - 1, X, T)].
