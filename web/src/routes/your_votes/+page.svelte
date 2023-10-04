<script lang="ts">
    export let data;
</script>

<svelte:head>
    <title>ApriBot :: Your votes</title>
</svelte:head>

<h1>Your votes</h1>

{#if data.username === null}
    <p>You must log in to view this page.</p>
{:else if data.totalVotes === 0}
    <p>You have not labelled any posts yet.</p>

    <p>
        If you want to help out, please visit the <a href="/contribute"
            >contribute</a
        > page—thank you!
    </p>
{:else}
    <p>
        In total, you have labelled {data.totalVotes} posts; {data.totalPositiveVotes}
        of them were positive votes.
    </p>
    <p>
        Here are
        {#if data.votes.length < data.totalVotes}
            the last {data.votes.length} posts
        {:else}
            all the posts
        {/if}
        you voted on (most recent on top). Thank you so much for your help!
    </p>
    <p>
        If you find you need to change or delete any of your votes, please get
        in touch with me via Discord (<code>@is_a_togekiss</code>) or
        <a href="https://reddit.com/u/is_a_togekiss">Reddit</a>.
    </p>

    <table>
        <thead>
            <tr>
                <th>Post ID</th>
                <th>Post title</th>
                <th>Submitter</th>
                <th>Your vote</th>
            </tr>
        </thead>
        <tbody>
            {#each data.votes as v}
                <tr>
                    <td>{v.postId}</td>
                    <td><a href={v.postUrl}>{v.postTitle}</a></td>
                    <td>/u/{v.postSubmitter}</td>
                    <td>{v.vote ? "Yes" : "No"}</td>
                </tr>{/each}
        </tbody>
    </table>
{/if}
