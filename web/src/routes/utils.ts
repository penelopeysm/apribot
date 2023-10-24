function pad(n: number) {
  return n < 10 ? `0${n}` : n;
}

export function showDate(d: Date) {
  // getMonth returns 0 for January (?!)
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}


export async function getLoginUsername(cookies): Promise<string | null> {
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
