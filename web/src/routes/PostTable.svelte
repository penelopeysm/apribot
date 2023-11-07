<script lang="ts">
    import { showDate } from "$lib/utils";
    import type { Post } from "$lib/types";
    export let posts: Post[];
</script>

{#if posts.length > 0}
    <table>
        <thead>
            <tr>
                <th class="can-truncate">Post ID</th>
                <th class="can-truncate">Local time</th>
                <th class="can-truncate">Submitter</th>
                <th class="can-truncate">Flair</th>
                <th class="can-truncate">Title</th>
            </tr>
        </thead>
        <tbody>
            {#each posts as post}
                <tr
                    ><td class="show-all"><code>{post.id}</code></td>
                    <td class="show-all">{showDate(post.time)}</td><td
                        class="show-all">/u/{post.submitter}</td
                    ><td class="show-all"
                        >{post.flair === null ? "" : post.flair}</td
                    ><td class="can-truncate"
                        ><a href={post.url}>{post.title}</a
                        ></td
                    ></tr
                >
            {/each}
        </tbody>
    </table>
{/if}

<style>
    /* Prevent wrapping, unless on small screens */
    .show-all {
        white-space: nowrap;
    }
    @media (max-width: 600px) {
        .show-all {
            white-space: normal;
            overflow-wrap: anywhere;
        }
    }

    .can-truncate {
        overflow-wrap: anywhere;
    }
</style>
