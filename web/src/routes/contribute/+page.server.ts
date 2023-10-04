export async function load({ fetch }) {
    const stats = await fetch("http://localhost:8080/api/ml_stats").then((res) => res.json());

    const res = {
        totalSeen: stats.seen,
        totalLabelled: stats.labelled,
    }
    return res;
}
