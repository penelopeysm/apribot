import type { Name } from "./types";

export async function load({ fetch }) {
    const names: Name[] = await fetch("http://localhost:8080/api/names").then((res) => res.json());
    return { names };
}
