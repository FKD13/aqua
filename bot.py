import sys
import json
from collections import defaultdict
from math import sqrt

for line in sys.stdin:
    data = json.loads(line)
    planets = data['planets']
    expeditions = data['expeditions']

    planets_by_name = {planet['name']: planet for planet in planets}
    expeditions_by_destination = defaultdict(lambda: [])

    for expedition in expeditions:
        expeditions_by_destination[expedition["destination"]].append(expedition)

    # Make a delta for each planet
    for planet in planets:
        expedition_part = sum(
            map(lambda x: x["ship_count"], [e for e in expeditions_by_destination[planet["name"]] if e["owner"] == 1])
        )

        expedition_part -= sum(
            map(lambda x: x["ship_count"], [e for e in expeditions_by_destination[planet["name"]] if e["owner"] != 1])
        )

        if planet["owner"] == 1:
            planet["delta"] = planet["ship_count"] + expedition_part
        else:
            planet["delta"] = -planet["ship_count"] + expedition_part

    planets = sorted(planets, key=lambda x: -x["delta"])
    moves = []

    while len(planets) != 0:
        planet = planets.pop(0)

        if planet["owner"] != 1:
            continue

        available_ships = min([planet["ship_count"], planet["delta"]])
        if available_ships <= 0:
            continue

        distances = list(map(lambda other: (round(sqrt((planet["x"] - other["x"]) ** 2 + (planet["y"] - other["y"]) ** 2), 2), other), planets))
        closest_distance = min(distances, key=lambda x: x[0])

        closest_planets = [p for d, p in distances if d == closest_distance[0]]

        if len(closest_planets) > available_ships:
            continue

        for other in closest_planets:
            moves.append({
                'origin': planet["name"],
                'destination': other["name"],
                'ship_count': available_ships // len(closest_planets)
            })
            other["delta"] += available_ships // len(closest_planets)

        planets = sorted(planets, key=lambda x: -x["delta"])

    print(json.dumps({'moves': moves}), flush=True)
