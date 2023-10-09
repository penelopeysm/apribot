<script lang="ts">
    export let data;
    import PostTable from "./PostTable.svelte";
</script>

<svelte:head>
    <title>ApriBot</title>
</svelte:head>

<h1>ApriBot</h1>

<div>
    <p>
        ApriBot is a bot for the Aprimarket Discord server, a trading hub for
        Pokémon in Apricorn and other rare balls. (Are you not a member of the
        server? <a href="https://discord.gg/f5YBDzeWhC"
            >Here's an invite link!</a
        >)
    </p>

    <p>
        ApriBot provides a few commands in the server itself (type <code
            >!help</code
        >
        in any channel to see this). Apart from that, ApriBot also monitors new posts
        on the
        <a href="https://reddit.com/r/pokemontrades">/r/pokemontrades</a> subreddit
        and identifies potential Aprimon-related threads, using a machine learning
        algorithm trained on manually labelled /r/pokemontrades posts.
    </p>

    <p>
        So far, ApriBot's new algorithm has processed a total of {data.totalPosts}
        posts, of which
        {data.totalHits} were hits ({(
            (100 * data.totalHits) /
            data.totalPosts
        ).toFixed(2)}%). This page shows you the most recent 50 hits and
        non-hits, ordered by most recent first.
    </p>

    <p>
        ApriBot's tech stack is Haskell, SvelteKit, PostgreSQL, and Python for
        the ML bits. If you have any questions about ApriBot, feel free to get
        in touch with me, either via <a
            href="https://github.com/penelopeysm/apribot">GitHub</a
        >
        or
        <a href="https://reddit.com/u/is_a_togekiss">Reddit</a>.
    </p>

    <h2>Hits</h2>
    <PostTable posts={data.hits} />

    <h2>Non-hits</h2>
    <PostTable posts={data.nonhits} />
</div>
