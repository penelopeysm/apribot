<script lang="ts">
    export let ball: string;

    import { ballImages } from "./ballImages";
</script>

<img class="small" src={ballImages[ball]} alt={ball} />

<style>
    img.small {
        width: 20px;
        padding: 0px;
        margin: 0px;
        margin-bottom: 1px;
        vertical-align: middle;
    }
</style>
