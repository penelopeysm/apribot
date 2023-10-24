<script lang="ts">
    import { showDate } from "./utils";
    export let posts: any[];
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
                    ><td><code>{post.id}</code></td><td
                        >{showDate(post.time)}</td
                    ><td>/u/{post.submitter}</td><td
                        >{post.flair === null ? "" : post.flair}</td
                    ><td class="post-title"
                        ><a href={post.url}>{post.title}</a></td
                    ></tr
                >
            {/each}
        </tbody>
    </table>
{/if}
