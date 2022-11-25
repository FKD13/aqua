:- use_module(library(http/json)).


:- initialization(main).


:- dynamic balans/2.
:- dynamic move/3.


main :-
    current_input(StdIn),
    repeat,
    retractall(balans(_,_)),
    retractall(move(_,_,_)),
    
    
    read_line_to_codes(StdIn, LineCodes), atom_codes(Line, LineCodes),
    atom_json_dict(Line, Json, []),
    write(Json.planets), nl,
    write(Json.expeditions), nl,

    make_balans(Json.planets, Json.expeditions),
    spread_max(),
    write(ok),nl,

    findall(move(A, B, C), move(A, B, C), Moves),
    write(Moves),nl,
    
    fail.

own(P) :- P.owner = 1.

find_max(Max) :- %repeat,
    write(search),nl,
    findall(balans(P, B), (balans(P, B), own(P)), Balanses),
    max_member([balans(_, B1), balans(_, B2)]>>(B1 @=< B2), Max, Balanses).

spread_max() :-
    findall(
        _,
        (
            find_max(Max),

            retract(Max),
            Max = balans(Planet, Balans),
            write(Max), nl,

            AvailableShips is min(Balans, Planet.ship_count),
            AvailableShips > 0,
            
            % Find closest planet(s)
            find_closest(Planet, ClosestPlanets),
            write(ClosestPlanets),nl,

            length(ClosestPlanets, ClosestPlanetsCount),
            MoveSize is round(AvailableShips / ClosestPlanetsCount),
            write(MoveSize), nl,

            send_ships(Planet, ClosestPlanets, MoveSize)
        ),
        _
    ).

send_ships(_ , []         , _).
send_ships(P1, [ P2 | Ps ], C) :- 
    assertz(move(P1.name, P2.name, C)),
    send_ships(P1, Ps, C).

    % Sends ships to balance out
    
distance_to_planet(P1, P2, D) :- D is ceiling(sqrt((P1.x-P2.x) ^ 2 + (P1.y-P2.y) ^ 2)).

find_closest(Planet, ClosestPlanets) :-
    findall(balans(P, B), balans(P, B), Balanses),
    maplist([balans(P, B), distance(balans(P, B), D)]>>distance_to_planet(Planet, P, D), Balanses, Distances),
    min_member([distance(_, D1), distance(_, D2)]>>(D1 @=< D2), distance(_, Distance), Distances),
    findall(P, member(distance(balans(P, B), Distance), Distances), ClosestPlanets).


% Generate the initial planet balans

make_balans(Planets, Expeditions) :-
    maplist(
        [Planet]>>(
            make_balans_planet(Planet, Expeditions, Balans),
            assertz(balans(Planet, Balans))
        ), 
        Planets
    ).
    
make_balans_planet(Planet, [], Planet.ship_count) :- Planet.owner = 1, !.
make_balans_planet(Planet, [], Count) :- Count is -Planet.ship_count, !.

make_balans_planet(Planet, [Expedition|Expeditions], Balans) :- 
    Expedition.destination \= Planet.name, !,
    make_balans_planet(Planet, Expeditions, Balans).
make_balans_planet(Planet, [Expedition|Expeditions], BalansP) :-
    Expedition.destination = Planet.name,
    make_balans_planet(Planet, Expeditions, Balans),
    (Planet.owner = 1 ->
        (Expedition.owner = 1 ->
            BalansP is Balans + Expedition.ship_count
            ;
            BalansP is Balans - Expedition.ship_count
        )
        ;
        (Expedition.owner \= 1 ->
            BalansP is Balans - Expedition.ship_count
            ;
            BalansP is Balans + Expedition.ship_count
        )
    ).
    
