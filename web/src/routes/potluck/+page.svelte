<script lang="ts">
    import { potluckData } from "./data";

    function showDate(d: Date) {
        return d.toLocaleString("default", {
            day: "numeric",
            month: "long",
            year: "numeric",
        });
    }

    const sortedPotluckEntries = Array.from(potluckData.entries()).sort(
        ([, a], [, b]) => b.date.getTime() - a.date.getTime()
    );
</script>

<svelte:head>
    <title>ApriBot :: Potlucks</title>
</svelte:head>
<h1>Potlucks</h1>

<p>Each week, the Aprimarket Discord server runs a <i>potluck</i> focused on one species, where members collaborate to complete a full set of said species in every rare ball.</p>

<p>Here is a list of potlucks which have been added to the website.</p>

<ul>
    {#each sortedPotluckEntries as [slug, entry]}
        <li>
            <a href="/potluck/{slug}"
                >{entry.pokemonName} ({showDate(entry.date)})</a
            >
        </li>
    {/each}
</ul>
