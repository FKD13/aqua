:- use_module(library(http/json)).


:- initialization(main).


main :-
    current_input(StdIn),
    repeat,
    
    read_line_to_codes(StdIn, LineCodes), atom_codes(Line, LineCodes),
    atom_json_dict(Line, Json, []),
    write(Json.planets), nl,
    write(Json.expeditions), nl,

    maplist([Planet, planet(Planet, Balans)]>>planet_balans(Planet, Json.expeditions, Balans), Json.planets, Balanses),
    write(Balanses),

    send_all(Json.planets, Json.planets, _),
    
    fail.


more_ships(P1, P2) :- P1.ship_count > P2.ship_count.

larger_planets(Planet, Planets, LargerPlanets) :-
    findall(
        LargerPlanet, 
        (
            member(LargerPlanet, Planets), 
            more_ships(LargerPlanet, Planet)
        ), 
        LargerPlanets
    ).


send_all([], _, []).
send_all([Planet|Planets], AllPlanets, Moves) :-
    larger_planets(Planet, AllPlanets, LargerPlanets).
    

planet_balans(Planet, [], Planet.ship_count) :- Planet.owner = 1, !.
planet_balans(Planet, [], Count) :- Count is -Planet.ship_count, !.

planet_balans(Planet, [Expedition|Expeditions], Balans) :- 
    Expedition.destination \= Planet.name, !,
    planet_balans(Planet, Expeditions, Balans).
planet_balans(Planet, [Expedition|Expeditions], BalansP) :-
    Expedition.destination = Planet.name,
    planet_balans(Planet, Expeditions, Balans),
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
    
