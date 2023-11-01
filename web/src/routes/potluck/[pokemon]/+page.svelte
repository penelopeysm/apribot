<script lang="ts">
    import ClickedUser from "./ClickedUser.svelte";
    import BallImage from "./BallImage.svelte";
    export let data;

    let highlightedRow: string | null = null;
    let highlightedCol: string | null = null;
    let clickedUser: string | null = null;

    type TableRow = {
        username: string;
        ball: string;
        giving: boolean[];
    };

    function parseCsvLine(line: string): TableRow {
        const [ball, username, ...giving] = line.split(",");
        return {
            username,
            ball,
            giving: giving.map((x) => x === "1"),
        };
    }

    function countTotalTrades(tableData: TableRow[]) {
        let n = 0;
        for (let row of tableData) {
            n += row.giving.filter(Boolean).length;
        }
        return n;
    }

    function setHighlighted(
        usernameRow: string | null,
        usernameCol: string | null
    ) {
        if (usernameRow === null) {
            highlightedRow = null;
        } else {
            highlightedRow = usernameRow;
        }
        if (usernameCol === null) {
            highlightedCol = null;
        } else {
            highlightedCol = usernameCol;
        }
    }

    function showDate(d: Date) {
        return d.toLocaleString("default", {
            day: "numeric",
            month: "long",
            year: "numeric",
        });
    }

    const tableData: TableRow[] = data.csv.trim().split("\n").map(parseCsvLine);
    const users: string[] = tableData.map((x) => x.username);
</script>

<svelte:head>
    <title>ApriBot :: {data.pokemonName}</title>
</svelte:head>
<h1>{data.pokemonName} Potluck ({showDate(data.date)})</h1>

<p>
    You can click on your name to see a list of your trades. (You don't need to
    log in for this!)
</p>

<p>
    This potluck round features {tableData.length} participants and {countTotalTrades(tableData)} trades.
</p>

<table>
    <thead>
        <tr>
            <td />
            <td />
            <td />
            <th colspan={users.length}>Receiving</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td />
            <th>Ball</th>
            <th>Username</th>
            {#each users as user}
                <td
                    class="clickable rotated"
                    class:highlighted={user === highlightedCol}
                    on:mouseover={() => {
                        setHighlighted(user, user);
                    }}
                    on:mouseout={() => {
                        setHighlighted(null, null);
                    }}
                    on:focus={() => {
                        setHighlighted(user, user);
                    }}
                    on:blur={() => {
                        setHighlighted(null, null);
                    }}
                    on:click={() => {
                        clickedUser = user;
                    }}><div>{user}</div></td
                >
            {/each}
            <th style="width:min-content">Total sending</th>
        </tr>
        {#each tableData as tableRow, i}
            <tr>
                {#if i === 0}
                    <th rowspan={tableData.length} class="rotated"
                        ><div>Sending</div></th
                    >
                {/if}
                <td
                    on:mouseover={() => {
                        setHighlighted(tableRow.username, tableRow.username);
                    }}
                    on:mouseout={() => {
                        setHighlighted(null, null);
                    }}
                    on:focus={() => {
                        setHighlighted(tableRow.username, tableRow.username);
                    }}
                    on:blur={() => {
                        setHighlighted(null, null);
                    }}
                    on:click={() => {
                        clickedUser = tableRow.username;
                    }}
                    class="clickable"
                    class:highlighted={tableRow.username === highlightedRow}
                    >{tableRow.ball}</td
                >
                <td
                    on:mouseover={() => {
                        setHighlighted(tableRow.username, tableRow.username);
                    }}
                    on:mouseout={() => {
                        setHighlighted(null, null);
                    }}
                    on:focus={() => {
                        setHighlighted(tableRow.username, tableRow.username);
                    }}
                    on:blur={() => {
                        setHighlighted(null, null);
                    }}
                    on:click={() => {
                        clickedUser = tableRow.username;
                    }}
                    class="clickable horizontal-name"
                    class:highlighted={tableRow.username === highlightedRow}
                    >{tableRow.username}</td
                >
                {#each tableRow.giving as giving, j}
                    <td
                        on:mouseover={() => {
                            if (giving)
                                setHighlighted(tableRow.username, users[j]);
                        }}
                        on:mouseout={() => {
                            setHighlighted(null, null);
                        }}
                        on:focus={() => {
                            if (giving)
                                setHighlighted(tableRow.username, users[j]);
                        }}
                        on:blur={() => {
                            setHighlighted(null, null);
                        }}
                        class="constrained"
                        class:highlighted={tableRow.username ===
                            highlightedRow || users[j] === highlightedCol}
                        class:greyed={i === j}
                    >
                        {#if giving}
                            <BallImage ball={tableRow.ball} />
                        {/if}
                    </td>
                {/each}
                <td
                    class="strong middle"
                    on:mouseover={() => {
                        setHighlighted(tableRow.username, null);
                    }}
                    on:mouseout={() => {
                        setHighlighted(null, null);
                    }}
                    on:focus={() => {
                        setHighlighted(tableRow.username, null);
                    }}
                    on:blur={() => {
                        setHighlighted(null, null);
                    }}
                    class:highlighted={tableRow.username === highlightedRow}
                    >{tableRow.giving.filter(Boolean).length}</td
                >
            </tr>
        {/each}
        <tr>
            <td />
            <th>Ball</th>
            <th>Username</th>
            {#each users as user}
                <td
                    class="clickable rotated"
                    class:highlighted={user === highlightedCol}
                    on:mouseover={() => {
                        setHighlighted(user, user);
                    }}
                    on:mouseout={() => {
                        setHighlighted(null, null);
                    }}
                    on:focus={() => {
                        setHighlighted(user, user);
                    }}
                    on:blur={() => {
                        setHighlighted(null, null);
                    }}
                    on:click={() => {
                        clickedUser = user;
                    }}><div>{user}</div></td
                >
            {/each}
                <td></td>
        </tr>
        <tr>
            <td />
            <th colspan="2">Total receiving</th>
            {#each tableData as tableRow, j}
                <td
                    class="strong middle"
                    on:mouseover={() => {
                        setHighlighted(null, tableRow.username);
                    }}
                    on:mouseout={() => {
                        setHighlighted(null, null);
                    }}
                    on:focus={() => {
                        setHighlighted(null, tableRow.username);
                    }}
                    on:blur={() => {
                        setHighlighted(null, null);
                    }}
                    class:highlighted={tableRow.username === highlightedCol}
                    >{tableData.filter((row) => row.giving[j]).length}</td
                >
            {/each}
            <td />
        </tr></tbody
    >
</table>

{#if clickedUser !== null}
    <ClickedUser bind:clickedUser {tableData} />
{/if}

<style>
    :root {
        --max-name-size: 135px;
    }

    table {
        margin-top: 30px;
    }

    td,
    th {
        padding: 2px;
        margin: 2px;
        height: 25px;
        max-height: 25px;
    }

    td.constrained {
        width: 27px;
        max-width: 27px;
        text-align: center;
    }

    .strong {
        font-weight: bold;
    }

    td.greyed {
        background-color: #aaa;
    }

    td.clickable {
        cursor: pointer;
        color: #2276b5;
        border-color: black;
    }

    .horizontal-name {
        min-width: var(--max-name-size);
        width: var(--max-name-size);
    }

    .rotated {
        white-space: nowrap;
        min-width: 25px;
        max-width: 25px;
        /* This value needs to account for the maximum height of the rotated inner div.
           There's no CSS-only way to calculate this, afaik (though you can use JavaScript) */
        min-height: var(--max-name-size);
        height: var(--max-name-size);
    }

    .rotated div {
        transform: rotate(-90deg);
        transform-origin: center left;
        translate: 50% calc((var(--max-name-size) * 0.5) - 2px);
        height: max-content;
    }

    .middle {
        text-align: center;
    }

    .highlighted {
        background-color: #aec6f2;
    }
</style>
