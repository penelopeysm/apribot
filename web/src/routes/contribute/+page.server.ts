import type { Post } from "$lib/types";
import { getLoginUsername } from "$lib/utils";
import { redirect } from "@sveltejs/kit";
import {
    getTotalPosts,
    getTotalLabelled,
    getTotalNeedNotLabel,
    getTotalLabelledBy,
    getNextUnlabelled,
} from "$lib/server/database";

export async function load({ parent }) {
    const parentData = await parent();

    const totalSeen = await getTotalPosts();
    const totalLabelled = await getTotalLabelled();
    const totalNeedNotLabel = await getTotalNeedNotLabel();
    const labelledByUser: number | null = parentData.username === null
        ? null
        : await getTotalLabelledBy(parentData.username);
    const next: Post | null = parentData.username === null
        ? null
        : await getNextUnlabelled();

    return {
        totalSeen,
        totalLabelled,
        totalNeedNotLabel,
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
