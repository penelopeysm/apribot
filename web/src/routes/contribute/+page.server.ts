import type { Post } from "../types";
import showdown from "showdown";
import sanitizeHtml from "sanitize-html";

function markdown2HtmlSafe(md: string): string {
    const converter = new showdown.Converter();
    const html = sanitizeHtml(converter.makeHtml(md));
    return html.replace(/&amp;#x200B;/g, '');
}

export async function load({ parent, fetch }) {
    const nextUnlabelled = await fetch("http://localhost:8080/api/next_unlabelled").then((res) => res.json());
    const mlStats = await fetch("http://localhost:8080/api/ml_stats").then((res) => res.json());

    const parentData = await parent();
    let labelledByUser: number | null;
    let next: Post | null;

    if (parentData.username === null) {
        labelledByUser = null;
        next = null;
    } else {
        // Get number of posts labelled by user
        const userStats = await fetch(`http://localhost:8080/api/user_stats?username=${parentData.username}&limit=1`).then(res => res.json());
        labelledByUser = userStats.totalVotes;
        // Get next post not yet labelled by user
        if (nextUnlabelled.error === "no_posts_needing_review") {
            next = null;
        } else {
            next = nextUnlabelled.post as Post;
            next.body = markdown2HtmlSafe(next.body);
            next.time = new Date(next.time);
        }
    }

    return {
        totalSeen: mlStats.seen,
        totalLabelled: mlStats.labelled,
        labelledByUser: labelledByUser,
        nextUnlabelled: next,
    }
}
