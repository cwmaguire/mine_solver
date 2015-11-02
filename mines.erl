-module(mines).

-export([solutions/1]).

-record(step,
        {picked :: [integer()],
         maxed :: [integer()],
         groups :: [{[integer()], integer()}]}).

solutions(Groups) when is_list(Groups) ->
    lists:usort(lists:flatten(solutions(#step{groups = Groups})));
solutions(#step{picked = Picked, groups = []}) ->
    list_to_tuple(lists:flatten(Picked));
solutions(Step = #step{picked = Picked,
                       maxed = MaxedOut,
                       groups = [{Spots, 0} | Groups]}) ->
    solutions(Step#step{maxed = lists:usort(Picked ++ MaxedOut ++ Spots),
                        groups = Groups});
solutions(Step = #step{groups = [{Spots, _} | _]}) ->
    case lists:foldl(fun valid_solutions/2, {Step, []}, Spots) of
        {_, []} ->
            list_to_tuple(lists:flatten([bad_solution | Step#step.picked]));
        {_, NewSteps} ->
            [solutions(NewStep) || NewStep <- NewSteps]
    end.

valid_solutions(Spot, {Step = #step{groups = [{Spots, Max} | Groups]}, Steps}) ->
    case is_valid(Spot, Step) of
        false ->
            {Step, Steps};
        true ->
            NewStep = Step#step{picked = [Spot | Step#step.picked],
                                groups = [{Spots -- [Spot], Max - 1} | Groups]},
            {Step, [NewStep | Steps]}
    end.

%is_valid(_, Spots, _Picked, MaxedOut, _Max = 1) ->
    %is_disjoint(Spots, MaxedOut);
is_valid(Spot, #step{picked = Picked, maxed = Maxed}) ->
    not lists:member(Spot, Picked ++ Maxed).

%is_disjoint(A, B) ->
    %A -- B == A.
