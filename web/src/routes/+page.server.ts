import type { Post } from "./types"

export async function load({ fetch }) {
    const stats: { rows: number, hits: number } = await fetch("http://localhost:8080/api/total_assigned").then((res) => res.json());
    const hits: Post[] = await fetch("http://localhost:8080/api/hits?limit=50").then((res) => res.json());
    const nonhits: Post[] = await fetch("http://localhost:8080/api/nonhits?limit=50").then((res) => res.json());
    hits.forEach((hit) => hit.time = new Date(hit.time));
    nonhits.forEach((nonhit) => nonhit.time = new Date(nonhit.time));

    const res = {
        totalPosts: stats.rows,
        totalHits: stats.hits,
        hits: hits,
        nonhits: nonhits,
    }
    return res;
}
