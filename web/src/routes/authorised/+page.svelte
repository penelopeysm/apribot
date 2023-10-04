<script lang="ts">
    export let data;
</script>

<svelte:head>
    <title>ApriBot :: Login</title>
</svelte:head>

{#if data.loginSuccess === false}
    <p>There was an error logging into Reddit: {data.loginError}</p>
{/if}
