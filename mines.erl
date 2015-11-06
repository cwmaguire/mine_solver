-module(mines).

-export([solutions/1]).
-export([probabilities/1]).

-record(step,
        {picked = [] :: [integer()],
         maxed = [] :: [integer()],
         groups = [] :: [{[integer()], integer()}]}).

probabilities(Solutions) ->
    AllCells = lists:flatten(Solutions),
    Freqs = lists:foldl(fun add_cell/2, dict:new(), AllCells),
    Probs = lists:map(fun({Cell, Count}) -> {Cell, Count / length(Solutions)} end, dict:to_list(Freqs)),
    lists:sort(fun({_, X}, {_, Y}) -> X > Y end, Probs).

add_cell(Cell, Dict) ->
    dict:update(Cell, fun(X) -> X + 1 end, 1, Dict).

solutions(Groups) when is_list(Groups) ->
    Solutions = lists:usort(lists:flatten(solutions(#step{groups = Groups}))),
    ValidSolutions = lists:filter(fun(Tuple) -> element(1, Tuple) /= bad_solution end, Solutions),
    lists:sort([lists:sort(tuple_to_list(S)) || S <- ValidSolutions]);
solutions(#step{picked = Picked, groups = []}) ->
    list_to_tuple(lists:flatten(Picked));
solutions(Step = #step{picked = Picked,
                       maxed = MaxedOut,
                       groups = [{Spots, 0} | Groups]}) ->
    solutions(Step#step{maxed = lists:usort(Picked ++ MaxedOut ++ Spots),
                        groups = Groups});
solutions(Step = #step{picked = Picked,
                       maxed = Maxed,
                       groups = [{Spots, Max} | Groups]}) ->
    case num_picked(Picked, Spots) of
        X when X == Max ->
            solutions(Step#step{groups = Groups, maxed = lists:usort(Maxed ++ Spots)});
        X when X > Max ->
            list_to_tuple(lists:flatten([bad_solution | Step#step.picked]));
        _ ->
            case lists:foldl(fun valid_solutions/2, {Step, []}, Spots) of
                {_, []} ->
                    list_to_tuple(lists:flatten([bad_solution | Step#step.picked]));
                {_, NewSteps} ->
                    [solutions(NewStep) || NewStep <- NewSteps]
            end
    end.

valid_solutions(Spot, {Step = #step{groups = [{Spots, Max} | Groups]}, Steps}) ->
    case is_valid(Spot, Step) of
        {_, true} ->
            {Step, Steps};
        {true, false} ->
            NewStep = Step#step{groups = [{Spots -- [Spot], Max - 1} | Groups]},
            {Step, [NewStep | Steps]};
        {false, false} ->
            NewStep = Step#step{picked = [Spot | Step#step.picked],
                                groups = [{Spots -- [Spot], Max - 1} | Groups]},
            {Step, [NewStep | Steps]}
    end.

is_valid(Spot, #step{picked = Picked, maxed = Maxed}) ->
    {lists:member(Spot, Picked),
     lists:member(Spot, Maxed)}.

num_picked(Picked, Spots) ->
    length([S || S <- Spots, lists:member(S, Picked)]).
