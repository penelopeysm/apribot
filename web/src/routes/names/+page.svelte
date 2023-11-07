<script lang="ts">
    import type { Name } from "$lib/types";
    export let data;

    let searchQuery = "";

    let standardisedQuery: string;
    let searchHits: Name[];
    $: {
        standardisedQuery = searchQuery
            .trim()
            .toLowerCase()
            .replace(/[^\-a-z0-9]/g, "");

        searchHits =
            standardisedQuery === ""
                ? data.names
                : data.names.filter((p) =>
                      p.unique_name.includes(standardisedQuery)
                  );
    }
</script>

<svelte:head>
    <title>ApriBot :: Names</title>
</svelte:head>

<h1>Names</h1>

<p>
    If you're having trouble getting ApriBot to recognise the name of a Pokémon,
    you can use this page to find the name that ApriBot uses internally.
</p>

<input type="text" placeholder="Search..." bind:value={searchQuery} />

<table>
    <thead>
        <tr>
            <th class="name">Name</th>
            <th class="form">Form (if relevant)</th>
            <th class="unique-name">Use this for ApriBot...</th>
        </tr>
    </thead>
    <tbody>
        {#each searchHits as hit}
            <tr>
                <td class="name">{hit.name}</td>
                <td class="form">{hit.form === null ? "" : hit.form}</td>
                <td class="unique-name">{hit.unique_name}</td>
            </tr>
        {/each}
    </tbody>
</table>

<style>
    input {
        font-family: inherit;
        font-size: 120%;
        margin-bottom: 1em;
    }

    .name {
        min-width: 120px;
        width: 120px;
    }

    .form {
        min-width: 170px;
        width: 170px;
    }

    .unique-name {
        min-width: 250px;
        width: 250px;
    }

    td.unique-name {
        font-family: "Fira Mono", monospace;
        font-weight: bold;
        font-size: 100%;
    }
</style>
