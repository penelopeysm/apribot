export async function load({ fetch, cookies }) {

    async function getLoginUsername(): Promise<string | null> {
        const sessionId = cookies.get('apribotSessionId');

        if (sessionId === undefined) {
            return null;
        } else {
            const res = await fetch('http://localhost:8080/api/login_username', {
                method: 'POST',
                body: JSON.stringify({ sessionId }),
            });
            if (res.status !== 200) {
                return null;
            } else {
                const json = await res.json();
                return json.username;
            }
        }
    }

    const username = await getLoginUsername();
    return { username: username };
}

