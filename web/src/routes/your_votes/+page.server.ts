import { redirect } from '@sveltejs/kit';
import { getLoginUsername } from '$lib/utils';
import {
    getTotalLabelledBy,
    getTotalPositiveLabelledBy,
    getLatestVotesBy,
    changeVote,
    deleteVote,
} from '$lib/server/database';

export async function load({ parent }) {
    const parentData = await parent();

    if (parentData.username === null) {
        throw redirect(307, '/');
    }
    else {
        const totalVotes = await getTotalLabelledBy(parentData.username);
        const totalPositiveVotes = await getTotalPositiveLabelledBy(parentData.username);
        const latestVotes = await getLatestVotesBy(parentData.username, 100);
        return { totalVotes, totalPositiveVotes, latestVotes };
    }
}


export const actions = {
    change: async ({ request, cookies }) => {
        const data = await request.formData();
        const username = await getLoginUsername(cookies);

        if (username !== null) {
            await changeVote(username, data.get('id') as string);
        }
        throw redirect(301, '/your_votes');
    },

    delete: async ({ request, cookies }) => {
        const data = await request.formData();
        const username = await getLoginUsername(cookies);
        if (username !== null) {
            await deleteVote(username, data.get('id') as string);
        }
        throw redirect(301, '/your_votes');
    },
}
