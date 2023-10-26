import type { Post } from "../types";
import { getLoginUsername, unescapeHtmlChars, markdown2HtmlSafe } from "../utils";
import { redirect } from "@sveltejs/kit";

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
            next.title = unescapeHtmlChars(next.title);
            next.body = markdown2HtmlSafe(next.body);
            next.time = new Date(next.time);
        }
    }

    return {
        totalSeen: mlStats.seen,
        totalLabelled: mlStats.labelled,
        totalNeedNotLabel: mlStats.need_not_label,
        labelledByUser: labelledByUser,
        nextUnlabelled: next,
    }
}

export const actions = {
    default: async ({ fetch, request, cookies }) => {
        const data = await request.formData();

        // Get username from backend
        const username = await getLoginUsername(cookies);

        // Send to Haskell backend
        const backendResp = await fetch('http://localhost:8080/api/contribute', {
            method: 'POST',
            body: JSON.stringify({
                contrib_post_id: data.get('id'),
                contrib_username: username,
                // TODO: ugly. How can we make this correct?
                contrib_vote: parseInt(data.get('vote') as string),
            }),
        }).then((r) => r.json());

        if (backendResp.success) {
            throw redirect(301, '/contribute');
        }
    }
}
