import { redirect } from '@sveltejs/kit';

type UserStatsResponse = {
    totalVotes: number;
    totalPositiveVotes: number;
    votes: VoteResponse[];
}

type VoteResponse = {
    postId: string;
    postTitle: string;
    postUrl: string;
    postSubmitter: string;
    vote: boolean;
}

export async function load({ parent, fetch }) {
    const parentData = await parent();

    if (parentData.username === null) {
        throw redirect(307, '/');
    }
    else {
        const resp: UserStatsResponse = await fetch(`http://localhost:8080/api/user_stats?username=${parentData.username}&limit=100`)
            .then(res => res.json());
        return resp;
    }
}
