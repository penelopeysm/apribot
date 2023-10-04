#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import sys

def get_ems(species):
    r = requests.get(f"https://pokemondb.net/pokedex/{species}/moves/8")
    soup = BeautifulSoup(r.text, "html.parser")
    breeding = soup.find_all(lambda elem: elem.name == 'p' and
                             'breeding in Pokémon Brilliant Diamond' in elem.text)[0]
    em_table = breeding.parent.find_all('table')[1]
    moves = em_table.find_all(lambda elem: elem.name == 'a' and
                              elem.get('href').startswith('/move/'))
    move_names = [move.get('href').replace('/move/', '') for move in moves]
    return move_names

def __main__():
    print(' '.join(get_ems(sys.argv[1])))

if __name__ == '__main__':
    __main__()
