import re
import unidecode

def cleanup(text):
    # convert to ASCII
    text = unidecode.unidecode(text, errors='ignore')
    # remove Markdown links
    text = re.sub(r'\[([^\]]+)\]\([^)]+\)', r'\1', text)
    # split on spaces
    words = text.split()
    # remove links (heuristically)
    words = [w for w in words if not w.startswith('http') and not w.startswith('www')]
    # convert curly double/single quotes to plain single quotes
    words = [w.replace('“', "'").replace('”', "'").replace('‘', "'").replace('’', "'") for w in words]
    # split on other punctuation (except apostrophes and hyphens) and convert to lowercase
    words = [w for word in words for w in re.split(r"[^A-Za-z0-9'-]+", word.lower())]
    # remove any remaining punctuation
    words = [re.sub(r"[^A-Za-z0-9]", '', w) for w in words]
    # get rid of anything that doesn't have at least one letter
    words = [s for s in words if any(c.isalpha() for c in s)]
    # string back together
    return ' '.join(words)
