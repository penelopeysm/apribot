#!/usr/bin/env python3

from langchain_openai import ChatOpenAI
from langchain.tools import tool
# from langchain_core.utils.function_calling import convert_to_openai_function
from langchain.agents import AgentExecutor
from langchain.agents.openai_tools.base import create_openai_tools_agent
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from langchain_core.output_parsers import StrOutputParser

import os
import json
import sys
import psycopg2
import io
from contextlib import redirect_stdout
from typing import Optional

SYSTEM_PROMPT = """
You are ApriBot, a highly advanced Discord bot which knows everything about the
Pokémon universe. You can answer any question about Pokémon, and you can also
provide information about their abilities, stats, and type advantages. Your
knowledge spans the anime, movies, games (both main series and spin-offs), and
the Pokémon Trading Card Game.

When someone asks for a hidden ability, you can use the get_ha() tool to look
this up. If it returns 'No HA', the Pokémon does not have a hidden ability. If
if returns 'Not a Pokémon', the name is not recognized as a Pokémon, and you
should use the get_random_ability() tool to provide a random ability. If it
returns any other string, that is the hidden ability of the Pokémon.

The messages that you will receive are in the form of a conversation on
Discord, with the most recent messages at the bottom. Thus, the human messages
may in fact come from different users. Each message you receive will tell you
the user's name, followed by the actual message content.

In your responses, you should provide as much information as possible and also
be as opinionated as possible, drawing on your vast knowledge of the Pokémon
universe. Do not ever provide vague or ambiguous answers, and do not ever hedge
your answers. You should always be confident. Do not ever suggest that an
answer is subjective or wrong.

When you reply, you should address the last person in the message chain. The
remaining messages are provided to you so that you have additional context to
formulate your reply. In your response, you should always address the author of
the last message by name. Do not add the @ symbol before the name.

Because your response will be sent on Discord, you must make sure that your
response is 3 paragraphs or less. You will also need to format your responses
with Markdown.

An additional piece of information that you will need to answer questions is
the Dreamfast Ball, a special Poké Ball that is an amalgamation of the Dream
Ball and Fast Ball. It is highly aesthetically pleasing and is considered the
best ball to catch Pokémon in. This is not in the main series games yet but
will be added in the next instalment, called 'Pokémon Legends Z-A'.
"""

ON_FLY = "FLY_APP_NAME" in os.environ

PSQL_URL = (os.getenv("DATABASE_URL")
            if ON_FLY
            else os.getenv("FLY_PG_PROXY_CONN_STRING")
            )
try:
    CONN = psycopg2.connect(PSQL_URL)
except Exception as e:
    print("Failed to connect to database, if running locally, run `make c`")

########################
## POKEMON DB QUERIES ##
########################

def get_pokemon_id(name):
    fuzzy_name = (name
                  .strip()
                  .replace("farfetch'd", "farfetchd")
                  .replace("sirfetch'd", "sirfetchd")
                  .replace("mr.-mime", "mr-mime")
                  .replace("mime-jr.", "mime-jr")
                  .replace("mr.-rime", "mr-rime")
                  .replace("flabébé", "flabebe")
                  .lower()
                  .split())
    fuzzy_name = '-'.join(fuzzy_name)
    fuzzy_name += '%'

    cur = CONN.cursor()
    # Check aliases first
    cur.execute("SELECT pokemon_id FROM aliases WHERE alias ILIKE %s", (fuzzy_name,))
    hits = cur.fetchall()
    if len(hits) == 1:
        return hits[0]

    print("fuzzy_name", fuzzy_name)

    # Then check rest of table
    cur.execute("SELECT id FROM pokemon WHERE unique_name ILIKE %s", (fuzzy_name,))
    hits = cur.fetchall()
    if len(hits) == 1:
        return hits[0]
    else:
        return None

@tool
def get_ha(pokemon_name: str) -> Optional[str]:
    """Retrieve the hidden ability of a Pokémon."""
    cur = CONN.cursor()
    pokemon_id = get_pokemon_id(pokemon_name)

    if pokemon_id is None:
        return "Not a Pokémon"
    else:
        cur.execute("SELECT ha_id FROM pokemon WHERE id = %s", (pokemon_id,))
        ha_id = cur.fetchone()
        if ha_id[0] is None:
            # Pokemon found but it has no HA
            return "No HA"
        else:
            cur.execute("SELECT name FROM abilities WHERE id = %s", (ha_id,))
            ha = cur.fetchone()[0]
            return ha

@tool
def get_random_ability() -> str:
    """Retrieve the name of a random ability."""
    cur = CONN.cursor()
    cur.execute("SELECT name FROM abilities ORDER BY RANDOM() LIMIT 1")
    ability = cur.fetchone()[0]
    return ability

TOOLS = [get_ha, get_random_ability]

########################
##     ENTRY POINT    ##
########################

if __name__ == "__main__":
    # Parse JSON input and convert it into a ChatPromptTemplate
    # JSON schema (earlier messages come first):
    # [
    #     { "role": "assistant", "content": "..." },
    #     { "role": "user", "name": "Penny", "content": "..." },
    # ]
    # System prompt is not included in JSON
    input_json = json.loads(sys.stdin.read())
    messages = [("system", SYSTEM_PROMPT)]
    for message in input_json:
        role = message["role"]
        content = message["content"].replace("{", "{{").replace("}", "}}")
        if role == "assistant":
            messages.append((role, content))
        elif role == "user":
            name = message["name"]
            messages.append((role,
                             (f"The user '{name}' said: {content}")))
    messages.append(MessagesPlaceholder("agent_scratchpad"))

    prompt = ChatPromptTemplate.from_messages(messages)
    llm = ChatOpenAI()
    output_parser = StrOutputParser()
    agent = create_openai_tools_agent(llm, [get_ha], prompt)

    with redirect_stdout(io.StringIO()) as ae_output:
        agent_executor = AgentExecutor(agent=agent, tools=TOOLS, verbose=True)
        s = agent_executor.invoke({})
    print(ae_output.getvalue(), file=sys.stderr)
    print(s["output"][:2000])
