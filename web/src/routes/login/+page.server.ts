import { redirect } from '@sveltejs/kit';

function makeRandomState(len: number): string {
    if (len < 1) {
        throw new Error('Invalid state length');
    }
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let result = '';
    for (let i = 0; i < len; i++) {
        result += chars.charAt(Math.floor(Math.random() * chars.length));
    }
    return result;
}

export const actions = {
    default: async ({ url, fetch, cookies }) => {
        const redirectTo = url.searchParams.get('redirect_to');

        const state = makeRandomState(40);
        cookies.set('apribotState', state, { path: '/' });
        const redditAuthUrl = await fetch('http://localhost:8080/api/get_login_url', {
            method: 'POST',
            body: JSON.stringify({ state: `${state}_${redirectTo}` }),
        }).then((r) => r.json());
        throw redirect(303, redditAuthUrl.url);
    }
}
