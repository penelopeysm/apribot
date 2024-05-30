<script lang="ts">
    type TableRow = {
        username: string;
        ball: string;
        giving: boolean[];
    };
    export let clickedUser: string | null;
    export let tableData: TableRow[];
    import BallTableOneCol from "./BallTableOneCol.svelte";
    import BallTableMerged from "./BallTableMerged.svelte";

    let users: string[] = tableData.map((x) => x.username);
    let clickedUserRow: TableRow = tableData.find(
        (x) => x.username === clickedUser
    ) as TableRow; // assert that there is always a clickedUserRow

    // Calculate whom you're giving to
    let givingToUsers: string[] = clickedUserRow.giving
        .map((giving, i) => (giving ? users[i] : null))
        .filter((x) => x !== null) as string[];
    let givingToUsersMap: Map<string, string[]> = new Map();
    givingToUsersMap.set(clickedUserRow.ball, givingToUsers);
    // Calculate who's giving to you
    // map<ball, username>
    let receivingFromUsersMap: Map<string, string[]> = new Map();
    const clickedUserIndex = users.indexOf(clickedUser as string);
    for (const tableRow of tableData) {
        if (tableRow.giving[clickedUserIndex]) {
            receivingFromUsersMap.set(tableRow.ball, [tableRow.username]);
        }
    }

    let mergeBallTables: boolean = true;
    let copyButton: HTMLButtonElement;
    let discordText: string;
    function copyTextToClipboard() {
        navigator.clipboard.writeText(discordText.trim());
        copyButton.textContent = "Copied!";
        setTimeout(() => {
            copyButton.textContent =
                "Copy to clipboard (in Discord-compatible form)";
        }, 1000);
    }
</script>

<button on:click={() => (clickedUser = null)}><div id="cover" /></button>
<div id="popup">
    <h2>{clickedUser}'s trades</h2>

    {#if mergeBallTables}
        <BallTableMerged
            sendingData={givingToUsersMap}
            receivingData={receivingFromUsersMap}
            bind:discordText
        />
    {:else}
        <BallTableOneCol
            sendingData={givingToUsersMap}
            receivingData={receivingFromUsersMap}
            bind:discordText
        />
    {/if}

    <button
        on:click={() => {
            mergeBallTables = !mergeBallTables;
        }}>Toggle format</button
    >
    <button
        bind:this={copyButton}
        on:click={() => {
            copyTextToClipboard();
        }}>Copy to clipboard (in Discord-compatible form)</button
    >
    <button on:click={() => (clickedUser = null)}>Close</button>
</div>
<svelte:window
    on:keydown={(e) => {
        if (e.key === "Escape") {
            clickedUser = null;
        }
    }}
/>

<style>
    div#cover {
        position: fixed;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        background-color: black;
        opacity: 0.3;
        cursor: pointer;
    }

    div#popup {
        position: fixed;
        width: 450px;
        left: 50%;
        top: 10vh;
        max-height: 80vh;
        transform: translate(-50%, 0);
        background-color: #aec6f2;
        border: 1px solid black;
        border-radius: 5px;
        padding: 1em;
        display: flex;
        flex-direction: column;
        gap: 5px;
        overflow: auto;
    }

    div#popup h2 {
        margin-top: 5px;
        margin-bottom: 5px;
    }

    div#popup button {
        font-family: inherit;
        margin-top: 5px;
        padding: 2px;
        font-size: 1em;
        border: 1px solid black;
        border-radius: 5px;
        background-color: #ddd;
        box-shadow: 1px 1px 1px rgba(0, 0, 0, 0.5);
    }
</style>
