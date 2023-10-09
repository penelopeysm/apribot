<script lang="ts">
    export let data;
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
            {data.totalLabelled} of these have been labelled by some amazing people!
        </li>
        <li>7709 of these were used to train ApriBot's current algorithm.</li>
    </ul>

    <p>
        ApriBot's current algorithm is based on XGBoost, and has quite
        respectable performance: it achieves an accuracy of approximately 96% on
        unseen posts, or an F1 score of 93%. I have tried retraining it with
        more data (around 15,000 posts), but it doesn't seem to improve at all.
        So, there's no urgent need to label more posts. However, if you want to,
        you can still do so — I will be very grateful, and there is a
        possibility that a different algorithm may be able to make use of the
        extra data!
    </p>

    <p class="strong">
        Please log in with Reddit (in the top-right corner) to continue.
    </p>
{:else}
    <p>
        Post labelling will be back soon! I'm currently migrating the website to
        a proper frontend framework.
    </p>
{/if}

<style>
    .strong {
        font-weight: bold;
    }
</style>
