import { redirect } from '@sveltejs/kit';

export const actions = {
    default: async ({ url, cookies, fetch }) => {
        const redirectToParam = url.searchParams.get('redirect_to');
        const redirectTo = redirectToParam === null ? '/' : redirectToParam;
        const sessionId = cookies.get('apribotSessionId');

        // Tell backend to delete token
        if (sessionId !== undefined) {
            const res = await fetch('http://localhost:8080/api/logout', {
                method: 'POST',
                body: JSON.stringify({ sessionId }),
            });
            const json = await res.json();
            if (json.error === undefined) {
                // Delete cookies
                cookies.delete('apribotSessionId');
                // Redirect to wherever the user was. 303 code is required
                // to ensure that the browser uses a GET request instead of
                // a POST request.
                throw redirect(303, redirectTo);
            }
        }
    }
}
