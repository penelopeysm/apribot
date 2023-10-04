<script lang="ts">
    export let posts: any[];

    function pad(n: number) {
        return n < 10 ? `0${n}` : n;
    }

    function showDate(d: Date) {
        // getMonth returns 0 for January (?!)
        return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
    }
</script>

{#if posts.length > 0}
    <table>
        <thead>
            <tr>
                <th>Post ID</th>
                <th>Local time</th>
                <th>Submitter</th>
                <th>Flair</th>
                <th>Title</th>
            </tr>
        </thead>
        <tbody>
            {#each posts as post}
                <tr
                    ><td><code>{post.id}</code></td><td>{showDate(post.time)}</td><td
                        >/u/{post.submitter}</td
                    ><td>{post.flair === null ? "" : post.flair}</td><td
                        class="post-title"
                        ><a href={post.url}>{post.title}</a></td
                    ></tr
                >
            {/each}
        </tbody>
    </table>
{/if}
