# apribot

**https://apribot.fly.dev**

ApriBot is the resident bot in [the Aprimarket Discord server](https://www.reddit.com/r/BankBallExchange/comments/157vavl/rbankballexchange_discord_server_is_now_live_with/).

The codebase comprises several parts:

 - The bulk of ApriBot is written in Haskell. This comprises:
   - a Reddit bot (`hs/app/Main.hs`), which checks for new posts approximately once per minute;
   - a Python script (`hs/python/predict.py`) which runs a `scikit-learn` pipeline on the pokemontrades posts to determine whether each post is about Aprimon;
   - a Discord bot (`hs/app/DiscordBot.hs`), which:
     - posts into a specified channel any pokemontrades posts that are hits;
     - posts all bankballexchange posts into another channel;
     - provides various server commands to aid with trading and to provide information about hidden abilities / egg moves / etc.
   - A small REST API (`hs/app/Web.hs`), which the SvelteKit site uses
 - Separately, there is a small website written in in SvelteKit (`web`) which provides information about the bot and allows Reddit users to log in and manually, retroactively, label posts (so that they can be used for ML training);
 - Finally, there is a PostgreSQL database which threads everything together.

ApriBot is currently hosted on [Fly.io](https://fly.io).
