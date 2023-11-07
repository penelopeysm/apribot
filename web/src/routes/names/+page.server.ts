import { getAllNames } from "$lib/server/database";

export async function load() {
    const names = await getAllNames();
    return { names };
}
