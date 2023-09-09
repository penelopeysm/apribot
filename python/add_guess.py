#!/usr/bin/env python3

import gspread

import sys
from pathlib import Path

CONFIG_PATH = Path(__file__).parents[1] / 'gspread.config'


def get_nature_row_no(nature):
    n = nature.lower()
    return (3 if n == "hardy" else 4 if n == "lonely"
            else 5 if n == "adamant" else 6 if n == "naughty"
            else 7 if n == "brave" else 8 if n == "bold"
            else 9 if n == "docile" else 10 if n == "impish"
            else 11 if n == "lax" else 12 if n == "relaxed"
            else 13 if n == "modest" else 14 if n == "mild"
            else 15 if n == "bashful" else 16 if n == "rash"
            else 17 if n == "quiet" else 18 if n == "calm"
            else 19 if n == "gentle" else 20 if n == "careful"
            else 21 if n == "quirky" else 22 if n == "sassy"
            else 23 if n == "timid" else 24 if n == "hasty"
            else 25 if n == "jolly" else 26 if n == "naive"
            else 27 if n == "serious" else 0)

def get_type_col_no(tera):
    t = tera.lower()
    return (4 if t == "normal" else 5 if t == "fighting" else
            6 if t == "flying" else 7 if t == "poison" else
            8 if t == "ground" else 9 if t == "rock" else
            10 if t == "bug" else 11 if t == "ghost" else
            12 if t == "steel" else 13 if t == "fire" else
            14 if t == "water" else 15 if t == "grass" else
            16 if t == "electric" else 17 if t == "psychic" else
            18 if t == "ice" else 19 if t == "dragon" else
            20 if t == "dark" else 21 if t == "fairy" else 0)

WINNERS = {
    '31': ("Naive", "Water"),
    '32': ("Rash", "Electric"),
    '33': ("Calm", "Bug"),
    '34': ("Sassy", "Fire"),
    '35': ("Docile", "Flying"),
    '36': ("Hardy", "Ghost"),
    '37': ("Gentle", "Poison"),
    '38': ("Naughty", "Normal"),
    '39': ("Hardy", "Normal"),
    '40': ("Lonely", "Fire"),
}

def add_to_sheet(nature, tera, score, user):
    gc = gspread.service_account(filename=CONFIG_PATH)
    sh = gc.open_by_key('1IR6rCNQYFccBrc_cxNVv2gEQnVpecEAo58IDRJYWNlo')

    tab = sh.worksheet('Mew GA Reveal')

    row = get_nature_row_no(nature)
    col = get_type_col_no(tera)
    if row == 0 or col == 0:
        raise ValueError("invalid nature or tera type")

    tab.update_cell(row, col, score)
    
    if int(score) == 15:
        for win_row_no, winner in WINNERS.items():
            wn = winner[0].lower()
            wt = winner[1].lower()
            if nature == wn and tera == wt:
                tab.update_cell(int(win_row_no), 4, winner[0])
                tab.update_cell(int(win_row_no), 7, winner[1])
                tab.update_cell(int(win_row_no), 10, user)

if __name__ == "__main__":
    nature, tera, score, user = sys.argv[1:]
    if int(score) > 0:
        add_to_sheet(nature, tera, score, user)
