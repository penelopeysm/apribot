# apribot

**https://apribot.fly.dev**

ApriBot is a bot which monitors the [/r/pokemontrades](https://reddit.com/r/pokemontrades) subreddit for posts which involve Aprimon trading.

It is comprised of several parts:

 - a Reddit bot (`app/Main.hs`), which checks for new posts approximately once per minute;
 - a Python script (`python/predict.py`) which runs a `scikit-learn` pipeline to determine whether the post is about Aprimon;
 - a Discord bot (`app/DiscordBot.hs`), which posts into a specified channel anything that is a hit;
 - a web app (`app/Web.hs`, and `static`) which provides information about the bot and allows Reddit users to log in and manually, retroactively, label posts (so that they can be used for ML training);
 - a SQLite database which threads everything together.

ApriBot is currently hosted on [Fly.io](https://fly.io), although this may change in the future.
