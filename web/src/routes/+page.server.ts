import type { Post } from "$lib/types";
import {
    getTotalAssigned,
    getTotalHits,
    getLatestHits,
    getLatestNonHits
} from "$lib/server/database";

export async function load() {
    const totalAssigned = await getTotalAssigned();
    const totalHits = await getTotalHits();
    const hits: Post[] = await getLatestHits(50);
    const nonhits: Post[] = await getLatestNonHits(50);
    return { totalAssigned, totalHits, hits, nonhits };
}
