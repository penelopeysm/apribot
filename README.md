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

# Prerequisites

The Haskell app needs a few binary dependencies.
The following should probably suffice:

```
brew install zlib pkg-config postgresql
```

To run locally, you'll also need a bunch of environment variables.
These aren't described here, except to note that I should have them stored somewhere on my computer...

Then:

- `make hs`: Runs the full Haskell app. If you're testing the Reddit scraping or Discord bot, you'll need this.

- `make a`: Runs the Haskell app but only the web backend. If you're only testing the web components, then this suffices.

- `make b`: Launches the frontend.

- `make c`: Opens a proxy to the `apripsql` database on Fly.io. You could in principle host the database locally, but you'll need to check the `apripsql` repository for that.
