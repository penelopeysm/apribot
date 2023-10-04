# apribot

**https://apribot.fly.dev**

ApriBot is a bot which monitors the [/r/pokemontrades](https://reddit.com/r/pokemontrades) and [/r/BankBallExchange](https://reddit.com/r/BankBallExchange) subreddits for posts which involve Aprimon trading.

It is comprised of several parts:

 - a Reddit bot (`hs/app/Main.hs`), which checks for new posts approximately once per minute;
 - a Python script (`hs/python/predict.py`) which runs a `scikit-learn` pipeline on the pokemontrades posts to determine whether each post is about Aprimon;
 - a Discord bot (`hs/app/DiscordBot.hs`), which posts into a specified channel anything that is a hit;
 - a backend (`hs/app/Web.hs`)
 - another (technically full-stack) web app in SvelteKit (`web`) which provides information about the bot and allows Reddit users to log in and manually, retroactively, label posts (so that they can be used for ML training);
 - a PostgreSQL database which threads everything together.

ApriBot is currently hosted on [Fly.io](https://fly.io).
