<script lang="ts">
    export let sendingData: Map<string, string[]>;
    export let receivingData: Map<string, string[]>;
    export let discordText: string;

    import BallImage from "./BallImage.svelte";

    // Generate text
    discordText = "";
    if (sendingData.size > 0) {
        discordText += "**Sending**\n";
        for (const [ball, users] of sendingData.entries()) {
            discordText += `:${ball.toLowerCase()}ball: ${users.join(", ")}\n`;
        }
    }
    if (receivingData.size > 0) {
        discordText += "**Receiving**\n";
        for (const [ball, users] of receivingData.entries()) {
            discordText += `:${ball.toLowerCase()}ball: ${users.join(", ")}\n`;
        }
    }
</script>

{#if sendingData.size > 0 }
<h3>Sending</h3>

<div id="ball-table">
    {#each sendingData.entries() as [ball, users]}
        <BallImage {ball} />
        <div class="ball-row-users">
            {users.join(", ")}
        </div>
    {/each}
</div>
{/if}

{#if receivingData.size > 0 }
<h3>Receiving</h3>

<div id="ball-table">
    {#each receivingData.entries() as [ball, users]}
        <BallImage {ball} />
        <div class="ball-row-users">
            {users.join(", ")}
        </div>
    {/each}
</div>
{/if}

<style>
    div#ball-table {
        display: grid;
        grid-template-columns: 27px 1fr;
        align-items: center;
        grid-row-gap: 5px;
    }

    h3 {
        margin-top: 5px;
        margin-bottom: 5px;
    }
</style>
