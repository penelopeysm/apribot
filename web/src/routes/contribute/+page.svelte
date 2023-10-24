<script lang="ts">
    import { showDate } from "../utils";
    import { onMount } from "svelte";
    export let data;

    let disabled = true;
    let trustedUsers = ["JBSouls", "againpedro", "is_a_togekiss"];
    let timer = data.username !== null && trustedUsers.includes(data.username) ? 500 : 1500;

    onMount(() => {
        setTimeout(() => {
            disabled = false;
        }, timer);
    });
</script>

<svelte:head>
    <title>ApriBot :: Contribute</title>
</svelte:head>

<h1>Contribute</h1>

{#if data.username === null}
    <p>
        Machine learning algorithms, such as the one ApriBot uses, need to be
        trained on
        <i>labelled data</i>: that is, posts which have been manually classified
        (by experts—yes, that's you!) as being either Aprimon-related or not.
    </p>

    <p>So far:</p>
    <ul>
        <li>ApriBot has seen a total of {data.totalSeen} posts.</li>
        <li>
            {data.totalNeedNotLabel} of these were marked as not needing manual review
            (as they are almost certainly not Aprimon-related); and
        </li>
        <li>
            {data.totalLabelled} of these have been labelled by some amazing people,
            of which 14487 were used to train ApriBot's current algorithm.
        </li>
    </ul>

    <p>
        ApriBot's current algorithm is based on XGBoost, and has quite
        respectable performance: it achieves an accuracy of approximately 96% on
        unseen posts, or an F1 score of 93%. Generally, this accuracy doesn't
        seem to improve with more data: so, there's no urgent need to label more
        posts. However, if you want to, you can still do so—I will be very
        grateful, and there is a possibility that a different algorithm may be
        able to make use of the extra data!
    </p>

    <p class="strong">
        Please log in with Reddit (in the top-right corner) to continue.
    </p>
{:else if data.nextUnlabelled === null}
    <p>There are no more unlabelled posts. Please check back again tomorrow!</p>
{:else}
    <p>
        Right now, out of {data.totalSeen - data.totalNeedNotLabel} posts requiring
        review, {data.totalLabelled} have been labelled.
        {#if data.labelledByUser !== null && data.labelledByUser > 0}
            {data.labelledByUser} of these {data.labelledByUser === 1
                ? "was"
                : "were"} by you. Thank you so much!
        {/if}
    </p>
    <div class="form-container">
        <form class="aprimon-question" action="/contribute" method="POST">
            <span class="strong"
                >Is the post below offering, or looking for, non-shiny breedable
                Aprimon?</span
            >
            <input type="hidden" name="id" value={data.nextUnlabelled.id} />
            <div id="button-container">
                <button type="submit" name="vote" value="1" disabled={disabled}>✅ Yes</button>
                <button type="submit" name="vote" value="0" disabled={disabled}>❌ No</button>
                <button type="submit" name="vote" value="2" disabled={disabled}>⏭️ Skip</button>
            </div>
        </form>
    </div>

    <div>
        <span class="title">{data.nextUnlabelled.title}</span>
        <span class="boxed-flair"
            >{data.nextUnlabelled.flair === null
                ? ""
                : data.nextUnlabelled.flair}</span
        >
    </div>

    <ul>
        <li>
            Submitted by /u/{data.nextUnlabelled.submitter} at {showDate(
                new Date(data.nextUnlabelled.time)
            )} (local time)
        </li>
        <li>
            <a href={data.nextUnlabelled.url}>Link to original Reddit post</a>
        </li>
    </ul>

    <div class="post-body">
        {#if data.nextUnlabelled.body.trim() === ""}
            &lt;empty post body&gt;
        {:else}
            <!-- Note: this is safe because the post body is sanitized by the server -->
            {@html data.nextUnlabelled.body}
        {/if}
    </div>
{/if}

<style>
    .strong {
        font-weight: bold;
    }
</style>
