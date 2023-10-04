<script lang="ts">
    export let sendingData: Map<string, string[]>;
    export let receivingData: Map<string, string[]>;
    export let discordText: string;
    import BallImage from "./BallImage.svelte";

    // Invert the receivingData to be user: ball
    let receivingDataInverted: Map<string, string> = new Map();
    for (const [ball, users] of receivingData.entries()) {
        for (const user of users) {
            receivingDataInverted.set(user, ball);
        }
    }

    // Reconstruct a list of senders, including whether you are also receiving
    // something from them
    let sendingList: {
        ball: string;
        user: string;
        receiving: string | null;
    }[] = [];
    for (const [ball, users] of sendingData.entries()) {
        for (const user of users) {
            const receiving = receivingDataInverted.get(user);
            if (receiving !== undefined) {
                sendingList.push({
                    ball,
                    user,
                    receiving,
                });
                receivingDataInverted.delete(user);
            } else {
                sendingList.push({ ball, user, receiving: null });
            }
        }
    }
    sendingList.sort((a, b) => {
        if (a.receiving === b.receiving) {
            return 0;
        } else if (a.receiving === null) {
            return 1;
        } else if (b.receiving === null) {
            return -1;
        } else {
            return 0;
        }
    });

    discordText = "";
    if (sendingList.length > 0) {
        discordText += "**Sending**\n";
        for (const send of sendingList) {
            discordText += `:${send.ball.toLowerCase()}ball: ${send.user}`;
            if (send.receiving !== null) {
                discordText += ` (receiving: :${send.receiving.toLowerCase()}ball:)`;
            }
            discordText += "\n";
        }
    }
    if (receivingDataInverted.size > 0) {
        discordText += "**Receiving**\n";
        for (const [user, ball] of receivingDataInverted.entries()) {
            discordText += `:${ball.toLowerCase()}ball: ${user}\n`;
        }
    }
</script>

{#if sendingList.length > 0}
    <h3>Sending</h3>

    <div id="ball-table">
        {#each sendingList as { ball, user, receiving }}
            <BallImage {ball} />
            <div class="ball-row-users">
                {user}
                {#if receiving !== null}
                    (receiving:
                    <BallImage ball={receiving} />
                    )
                {/if}
            </div>
        {/each}
    </div>
{/if}

{#if receivingDataInverted.size > 0}
    <h3>Receiving</h3>

    <div id="ball-table">
        {#each receivingDataInverted.entries() as [user, ball]}
            <BallImage {ball} />
            <div class="ball-row-users">
                {user}
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
