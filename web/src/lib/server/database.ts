// Instead of `import {Client} from "pg"` we need a slightly awkward
// construction here to make `npm run build` work. This is because pg is a
// CommonJS module.
import pg from "pg";
import type { Post, Name, Vote } from "$lib/types";
import config from "$lib/server/config";
import { sanitisePost } from "$lib/utils";

async function withClient(async_callback: (client: pg.Client) => Promise<any>) {
    const client = new pg.Client({ connectionString: config.dbUrl });
    await client.connect();
    const res = await async_callback(client);
    client.end();
    return res;
}

export async function getTotalPosts(): Promise<Post | null> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(*) FROM posts;");
        return parseInt(res.rows[0].count);
    });
}

export async function getTotalAssigned(): Promise<number> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(*) FROM posts WHERE hit IS NOT NULL;");
        return parseInt(res.rows[0].count);
    });
}

export async function getTotalHits(): Promise<number> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(*) FROM posts WHERE hit;");
        return parseInt(res.rows[0].count);
    });
}

export async function getTotalLabelled(): Promise<number> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(DISTINCT post_id) FROM votes;");
        return parseInt(res.rows[0].count);
    });
}

export async function getTotalLabelledBy(username: string): Promise<number> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(*) FROM votes WHERE username = $1;", [username]);
        return parseInt(res.rows[0].count);
    });
}

export async function getTotalPositiveLabelledBy(username: string): Promise<number> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(*) FROM votes WHERE username = $1 and vote;", [username]);
        return parseInt(res.rows[0].count);
    });
}

export async function getTotalNeedNotLabel(): Promise<number> {
    return withClient(async (client) => {
        const res = await client.query("SELECT COUNT(*) FROM posts WHERE NOT needs_review;");
        return parseInt(res.rows[0].count);
    });
}

export async function getLatestHits(n: number): Promise<Post[]> {
    return withClient(async (client) => {
        const res = await client.query(`
SELECT id, url, title, submitter, utc_time, flair FROM posts
WHERE hit
ORDER BY utc_time DESC
LIMIT $1;
`, [n]);
        return res.rows.map((row) => {
            return sanitisePost({
                unsafeId: row.id,
                unsafeTime: new Date(row.utc_time),
                unsafeTitle: row.title,
                unsafeBody: "",
                unsafeFlair: row.flair,
                unsafeUrl: row.url,
                unsafeSubmitter: row.submitter,
            });
        });
    });
}

export async function getLatestNonHits(n: number): Promise<Post[]> {
    return withClient(async (client) => {
        const res = await client.query(`
SELECT id, url, title, submitter, utc_time, flair FROM posts
WHERE NOT hit
ORDER BY utc_time DESC
LIMIT $1;
`, [n]);
        return res.rows.map((row) => {
            return sanitisePost({
                unsafeId: row.id,
                unsafeTime: new Date(row.utc_time),
                unsafeTitle: row.title,
                unsafeBody: "",
                unsafeFlair: row.flair,
                unsafeUrl: row.url,
                unsafeSubmitter: row.submitter,
            });
        });
    });
}

export async function getAllNames(): Promise<Name[]> {
    return withClient(async (client) => {
        const res = await client.query("SELECT id, name, form, unique_name FROM pokemon ORDER BY ndex ASC;");
        return res.rows;
    });
}

export async function getNextUnlabelled(): Promise<Post | null> {
    return withClient(async (client) => {
        const res = await client.query(`
SELECT id, url, title, body, submitter, utc_time, flair
FROM posts
WHERE id NOT IN (SELECT post_id FROM votes)
AND needs_review
ORDER BY RANDOM()
LIMIT 1;
`);
        if (res.rows.length === 0) {
            return null;
        } else {
            return sanitisePost({
                unsafeId: res.rows[0].id,
                unsafeTime: new Date(res.rows[0].utc_time),
                unsafeTitle: res.rows[0].title,
                unsafeBody: res.rows[0].body,
                unsafeFlair: res.rows[0].flair,
                unsafeUrl: res.rows[0].url,
                unsafeSubmitter: res.rows[0].submitter,
            })
        }
    });
}

export async function getLatestVotesBy(username: string, n: number): Promise<Vote[]> {
    return withClient(async (client) => {
        const res = await client.query(`
SELECT v.post_id, p.title, p.url, p.submitter, v.vote
FROM posts as p
INNER JOIN votes as v ON p.id = v.post_id
WHERE v.username = $1
ORDER BY v.id DESC
LIMIT $2;
`, [username, n]);
        return res.rows.map((row) => {
            return {
                post: sanitisePost({
                    unsafeId: row.post_id,
                    unsafeTime: new Date(), // Unneeded
                    unsafeTitle: row.title,
                    unsafeBody: "",         // Unneeded
                    unsafeFlair: null,      // Unneeded
                    unsafeUrl: row.url,
                    unsafeSubmitter: row.submitter,
                }),
                vote: row.vote,
            };
        });
    });
}

export async function changeVote(username: string, postId: string): Promise<void> {
    return withClient(async (client) => {
        const res = await client.query("UPDATE votes SET vote = NOT vote WHERE post_id = $1 AND username = $2", [postId, username]);
        if (res.rowCount !== 1) {
            throw new Error("Vote change failed");
        }
    });
}

export async function deleteVote(username: string, postId: string): Promise<void> {
    return withClient(async (client) => {
        const res = await client.query("DELETE FROM votes WHERE post_id = $1 AND username = $2", [postId, username]);
        if (res.rowCount !== 1) {
            throw new Error("Vote delete failed");
        }
    });
}
