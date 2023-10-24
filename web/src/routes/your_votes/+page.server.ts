import { redirect } from '@sveltejs/kit';
import { getLoginUsername } from '../utils';

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

export const actions = {
  change: async ({ fetch, request, cookies }) => {
    const data = await request.formData();
    const username = await getLoginUsername(cookies);

    const backendResp = await fetch('http://localhost:8080/api/change_vote', {
      method: 'POST',
      body: JSON.stringify({
        vote_post_id: data.get('id'),
        vote_username: username,
      }),
    }).then((r) => r.json());

    if (backendResp.success) {
      throw redirect(301, '/your_votes');
    }
  },

  delete: async ({ fetch, request, cookies }) => {
    const data = await request.formData();
    const username = await getLoginUsername(cookies);


    const backendResp = await fetch('http://localhost:8080/api/delete_vote', {
      method: 'POST',
      body: JSON.stringify({
        vote_post_id: data.get('id'),
        vote_username: username,
      }),
    }).then((r) => r.json());

    if (backendResp.success) {
      throw redirect(301, '/your_votes');
    }
  },
}
