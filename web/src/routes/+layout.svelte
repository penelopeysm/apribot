<script lang="ts">
    import { page } from "$app/stores";
    export let data;

    const links = [
        ["/", "home"],
        ["/contribute", "contribute"],
        ["/privacy", "privacy"],
        ["/names", "names"],
    ];
</script>

<nav>
    <ul class="links">
        {#each links as [path, description]}
            <li>
                {#if $page.url.pathname == path}
                    <span class="italic">{description}</span>
                {:else}
                    <a
                        class="italic"
                        class:here={$page.url.pathname == path}
                        href={path}>{description}</a
                    >
                {/if}
            </li>
        {/each}
        <li>
            {#if $page.url.pathname == "/potluck"}
                <span class="italic">potlucks</span>
            {:else}
                <a
                    class="italic"
                    class:here={$page.url.pathname == "/potluck"}
                    href="/potluck"
                >
                    <div id="potluck-dropdown">potlucks</div>
                </a>
            {/if}
        </li>
    </ul>

    <div id="login-status">
        <ul class="links">
            {#if data.username !== null}
                <li>
                    <span class="italic">logged in as: {data.username}</span>
                </li>
                <li>
                    {#if $page.url.pathname == "/your_votes"}
                        <span class="italic">your votes</span>
                    {:else}
                        <a
                            class="italic"
                            class:here={$page.url.pathname == "/your_votes"}
                            href="/your_votes">your votes</a
                        >
                    {/if}
                </li>
                <li>
                    <form
                        method="POST"
                        action="/logout?redirect_to={$page.url.pathname}"
                    >
                        <button class="like-link" type="submit">logout</button>
                    </form>
                </li>
            {:else}
                <li>
                    <form
                        method="POST"
                        action="/login?redirect_to={$page.url.pathname}"
                    >
                        <button class="like-link" type="submit"
                            >login with Reddit</button
                        >
                    </form>
                </li>
            {/if}
        </ul>
    </div>
</nav>

<slot />

<style>
    @media (max-width: 600px) {
        nav {
            flex-direction: column;
            align-items: flex-start;
        }
        ul {
            padding: 0;
        }
        ul.links li {
            list-style-type: none;
        }
    }

    @media (min-width: 600px) {
        nav {
            display: flex;
            flex-direction: row;
            flex-wrap: wrap;
            row-gap: 0px;
            column-gap: 50px;
            align-items: center;
            width: 100%;
            justify-content: center;
        }

        ul.links {
            display: flex;
            flex-direction: row;
            gap: 12px;
            align-items: center;
            justify-content: center;
            margin-right: auto;
            padding: 0;
            margin-top: 5px;
            margin-bottom: 5px;
        }

        ul.links li {
            list-style-type: none;
            display: inline-block;
        }

        ul.links li + li:before {
            content: " • ";
            padding-right: 10px;
        }
    }

    form {
        display: inline;
    }

    div#login-status {
        margin-left: auto;
    }

    a.here {
        color: #111111;
        text-decoration: none;
    }

    .italic {
        font-style: italic;
    }

    button {
        font-family: inherit;
    }

    button.like-link {
        font-style: italic;
        background: none !important;
        border: none;
        padding: 0 !important;
        font-size: 1em;
        text-decoration: underline;
        color: #0000ee;
        cursor: pointer;
    }

    div#potluck-dropdown {
        display: inline-block;
        font-style: italic;
        text-decoration: underline;
    }
</style>
