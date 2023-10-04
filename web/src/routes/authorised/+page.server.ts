import { redirect } from '@sveltejs/kit';

function parseReturnedState(returnedState: string | null): { parsedState: string, redirectTo: string } {
    if (returnedState === null) {
        return {
            parsedState: '',
            redirectTo: '/',
        }
    }
    const i = returnedState.indexOf('_');
    if (i === -1) {
        return {
            parsedState: returnedState,
            redirectTo: '/',
        }
    } else {
        return {
            parsedState: returnedState.slice(0, i),
            redirectTo: returnedState.slice(i + 1),
        }
    }
}

export async function load({ url, cookies, fetch }) {
    const error = url.searchParams.get('error');
    if (error !== null) {
        // Error logging in. TODO: Show error page
        return {
            loginSuccess: false,
            loginError: error,
        }
    }

    // If no error, then we have a code and state. The backend is also
    // guaranteed to have stored the token.
    const code = url.searchParams.get('code');
    // returnedState is what we sent to Reddit as the state parameter (and also
    // what it sends back). It consists of the _actual_ state, plus an
    // underscore, plus the URL to redirect back to after logging in.
    const returnedState = url.searchParams.get('state');
    const { parsedState, redirectTo } = parseReturnedState(returnedState);

    if (code === null) {
        return {
            loginSuccess: false,
            loginError: "Reddit login did not return an authentication code",
        }
    }
    else {
        // Check that state matches existing cookie
        const storedState = cookies.get('apribotState');
        if (storedState !== parsedState) {
            return {
                loginSuccess: false,
                loginError: "State parameter mismatch",
            }
        }
        // If reached here, then login was successful.
        cookies.delete('apribotState');
        const redditLoggedIn = await fetch('http://localhost:8080/api/make_reddit_token', {
            method: 'POST',
            body: JSON.stringify({ code }),
        });
        await redditLoggedIn.json().then((json) => {
            const sessionId: string = json.sessionId;
            cookies.set('apribotSessionId', sessionId, {
                path: '/',
                maxAge: 2 * 60 * 60 * 24 * 365, // 2 years
            });
        });
        throw redirect(307, redirectTo);
    }

}
